running_stats<-function (x, y, weight, bins = 10, type = "median", 
                         ranges = pnorm(c(-1,1)), 
                         binaxis = "x",
                         equalN = TRUE, xcut, ycut, log = "", 
                         Nscale = FALSE, diff = FALSE, 
                         outlier_thresh=0.15) {
    if (missing(y)) {
        if (length(dim(x)) > 1) {
            y = x[, 2]
            x = x[, 1]
        }
        else {
            stop("Illegal x/y data format.")
        }
    }
    if (missing(weight)) {
      weight<-rep(1,length(x))
    } else { 
      if (length(weight)!=length(x)) { 
        stop(paste("x and weights have different length:",length(x),length(weight)))
      }
    }
    if (length(ranges) > 0) {
        if (ranges[1] != "sd" & (any(ranges < 0) | any(ranges > 
            1))) {
            stop("Illegal probabilities in ranges, outside [0,1]!")
        }
    }
    tempx = x
    tempy = y
    tempw = weight
    good = is.na(tempx) == FALSE & is.nan(tempx) == FALSE & is.infinite(tempx) == 
        FALSE & is.null(tempx) == FALSE & is.na(tempy) == FALSE & 
        is.nan(tempy) == FALSE & is.infinite(tempy) == FALSE & 
        is.null(tempy) == FALSE & is.finite(weight) == TRUE 
    x = tempx[good]
    y = tempy[good]
    w = tempw[good]
    xsel = rep(T, length(x))
    ysel = rep(T, length(y))
    wsel = rep(T, length(w))
    if (missing(xcut) == F) {
        xsel = x >= xcut[1] & x <= xcut[2]
    }
    if (missing(ycut) == F) {
        ysel = y >= ycut[1] & y <= ycut[2]
    }
    x = x[xsel & ysel]
    y = y[xsel & ysel]
    w = w[xsel & ysel]
    if (missing(log) == F) {
        if (log == "x") {
            sel = which(x > 0)
            x = log10(x[sel])
            y = y[sel]
            w = w[sel]
        }
        if (log == "y") {
            sel = which(y > 0)
            x = x[sel]
            y = log10(y[sel])
            w = w[sel]
        }
        if (log == "xy" | log == "yx") {
            sel = which(x > 0 & y > 0)
            x = log10(x[sel])
            y = log10(y[sel])
            w = w[sel]
        }
    }
    if (binaxis == "x") {
        checkvec = x
    }
    if (binaxis == "y") {
        checkvec = y
    }
    outl = {
    }
    xmid = {
    }
    ymid = {
    }
    xquan = {
    }
    yquan = {
    }
    xsd = {
    }
    ysd = {
    }
    bincens = {
    }
    binlims = {
    }
    if (length(bins) == 1) {
        if (equalN) {
          if (any(w!=1)) { 
            cdf = wtd_ecdf(checkvec,w)
            breaks = approxfun(x=cdf(seq(min(checkvec),max(checkvec),length=max(bins*10,1e4))),
                               y=seq(min(checkvec),max(checkvec),length=max(bins*10,1e4)),
                               ties='ordered')(seq(0, 1, len = bins + 1))
          } else { 
            breaks = quantile(checkvec, seq(0, 1, len = bins + 1))
          }
        }
        else {
            breaks = seq(min(checkvec[is.finite(checkvec)], na.rm = T), 
                max(checkvec[is.finite(checkvec)], na.rm = T), 
                len = bins + 1)
        }
    }
    else {
        breaks = bins
        bins = length(bins) - 1
    }
    Nbins = {
    }
    for (i in 1:bins) {
        binsel = which(checkvec >= breaks[i] & checkvec <= breaks[i + 
            1])
        Nbin = length(binsel)
        Nbins = c(Nbins, Nbin)
        if (type == "median") {
            xmtemp = matrixStats::weightedMedian(x[binsel],w[binsel], na.rm = TRUE)
            ymtemp = matrixStats::weightedMedian(y[binsel],w[binsel], na.rm = TRUE)
        }
        if (type == "mean") {
            xmtemp = matrixStats::weightedMean(x[binsel],w[binsel], na.rm = TRUE)
            ymtemp = matrixStats::weightedMean(y[binsel],w[binsel], na.rm = TRUE)
        }
        if (type == "mode") {
            tempdenx = density(x[binsel],weight=w[binsel], na.rm = TRUE)
            tempdeny = density(y[binsel],weight=w[binsel], na.rm = TRUE)
            xmtemp = tempdenx$x[which.max(tempdenx$y)]
            ymtemp = tempdeny$x[which.max(tempdeny$y)]
        }
        outytemp<-length(which(abs(y[binsel])>outlier_thresh))/length(binsel)
        if (type == "mode2d") {
            tempden2d = kde2d(x[binsel], y[binsel],w=w[binsel])
            xmtemp = tempden2d$x[which(tempden2d$z >= max(tempden2d$z, 
                na.rm = TRUE), arr.ind = TRUE)[1]]
            ymtemp = tempden2d$y[which(tempden2d$z >= max(tempden2d$z, 
                na.rm = TRUE), arr.ind = TRUE)[2]]
        }
        outl = c(outl, outytemp)
        xmid = c(xmid, xmtemp)
        ymid = c(ymid, ymtemp)
        bincens = c(bincens, (breaks[i + 1] + breaks[i])/2)
        if (length(ranges) > 0) {
          if (any(w[binsel]!=1)) { 
            xcdf = wtd_ecdf(x[binsel],w[binsel])
            ycdf = wtd_ecdf(y[binsel],w[binsel])
            xqtemp = approxfun(x=xcdf(seq(min(x[binsel]),max(x[binsel]),length=1e4)),
                               y=seq(min(x[binsel]),max(x[binsel]),length=1e4),
                               ties = "ordered")(ranges)
            yqtemp = approxfun(x=ycdf(seq(min(y[binsel]),max(y[binsel]),length=1e4)),
                               y=seq(min(y[binsel]),max(y[binsel]),length=1e4),
                               ties = "ordered")(ranges)
          } else { 
            xqtemp = quantile(x[binsel], na.rm=T,probs=ranges)
            yqtemp = quantile(y[binsel], na.rm=T,probs=ranges)
          } 
            if (Nscale) {
                diffx = xqtemp - xmtemp
                diffy = yqtemp - ymtemp
                xqtemp = xmtemp + diffx/sqrt(Nbin)
                yqtemp = ymtemp + diffy/sqrt(Nbin)
            }
            if (diff) {
                xqtemp = xqtemp - xmtemp
                yqtemp = yqtemp - ymtemp
            }
            xquan = rbind(xquan, xqtemp)
            yquan = rbind(yquan, yqtemp)
        }
        if (diff) {
            if (Nscale) {
                xsd = c(xsd, matrixStats::weightedMad(x[binsel],w[binsel])/sqrt(Nbin))
                ysd = c(ysd, matrixStats::weightedMad(y[binsel],w[binsel])/sqrt(Nbin))
            }
            else {
                xsd = c(xsd, matrixStats::weightedMad(x[binsel],w[binsel]))
                ysd = c(ysd, matrixStats::weightedMad(y[binsel],w[binsel]))
            }
        }
        else {
            if (Nscale) {
                xsd = rbind(xsd, c(xmtemp - matrixStats::weightedMad(x[binsel],w[binsel])/sqrt(Nbin), 
                  xmtemp + matrixStats::weightedMad(x[binsel],w[binsel])/sqrt(Nbin)))
                ysd = rbind(ysd, c(ymtemp - matrixStats::weightedMad(y[binsel],w[binsel])/sqrt(Nbin), 
                  ymtemp + matrixStats::weightedMad(y[binsel],w[binsel])/sqrt(Nbin)))
            }
            else {
                xsd = rbind(xsd, c(xmtemp - matrixStats::weightedMad(x[binsel],w[binsel]), xmtemp + 
                  matrixStats::weightedMad(x[binsel],w[binsel])))
                ysd = rbind(ysd, c(ymtemp - matrixStats::weightedMad(y[binsel],w[binsel]), ymtemp + 
                  matrixStats::weightedMad(y[binsel],w[binsel])))
            }
        }
    }
    if (missing(log) == F) {
        if (log == "x") {
            xmid = 10^xmid
            if (length(ranges) > 0) {
                xquan = 10^xquan
            }
        }
        if (log == "y") {
            ymid = 10^ymid
            if (length(ranges) > 0) {
                yquan = 10^yquan
            }
        }
        if (log == "xy" | log == "yx") {
            xmid = 10^xmid
            ymid = 10^ymid
            if (length(ranges) > 0) {
                xquan = 10^xquan
                yquan = 10^yquan
            }
        }
    }
    return = list(x = xmid, y = ymid, xquan = xquan, yquan = yquan, 
        xsd = xsd, ysd = ysd, bincens = bincens, binlims = breaks, 
        Nbins = Nbins,outlier=outl)
}


#Taken from : https://rdrr.io/github/bvegetabile/entbal/
wtd_ecdf <- function (var_data, wts) {
  #-----------------------------------------------------------------------------
  # wtd_ecdf is a modification of the ecdf() function in base R.  It modifies
  # the function to be able to incorporate weights.  This is to visualize
  # balance using the empirical cumulative distribution function for continuous
  # covariates after weighting by the inverse of the propensity score (IPTW)
  #
  # Input variables
  # --- var_data : covariate values - vector of data
  # --- wts      : weights for assessing cov balance by IPTW - vector of data.
  #-----------------------------------------------------------------------------
  ord <- order(var_data)
  var_ordered <- var_data[ord]
  wts_ordered <- wts[ord]
  n <- length(var_data)
  if (n < 1)
    stop("'var_data' must have 1 or more non-missing values")
  vals <- unique(var_ordered)
  matched_vals <- match(var_ordered, vals)
  weight_list <- aggregate(wts_ordered, by=list(matched_vals), sum)
  rval <- approxfun(vals, cumsum(weight_list[,2])/sum(wts_ordered),
                    method = "constant", yleft = 0, yright = 1, f = 0, ties = "ordered")
  class(rval) <- c("ecdf", "stepfun", class(rval))
  assign("nobs", n, envir = environment(rval))
  attr(rval, "call") <- sys.call()
  rval
}
