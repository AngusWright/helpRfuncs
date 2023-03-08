running_stats<-function (x, y, bins = 10, type = "median", 
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
    if (length(ranges) > 0) {
        if (ranges[1] != "sd" & (any(ranges < 0) | any(ranges > 
            1))) {
            stop("Illegal probabilities in ranges, outside [0,1]!")
        }
    }
    tempx = x
    tempy = y
    good = is.na(tempx) == FALSE & is.nan(tempx) == FALSE & is.infinite(tempx) == 
        FALSE & is.null(tempx) == FALSE & is.na(tempy) == FALSE & 
        is.nan(tempy) == FALSE & is.infinite(tempy) == FALSE & 
        is.null(tempy) == FALSE
    x = tempx[good]
    y = tempy[good]
    xsel = rep(T, length(x))
    ysel = rep(T, length(y))
    if (missing(xcut) == F) {
        xsel = x >= xcut[1] & x <= xcut[2]
    }
    if (missing(ycut) == F) {
        ysel = y >= ycut[1] & y <= ycut[2]
    }
    x = x[xsel & ysel]
    y = y[xsel & ysel]
    if (missing(log) == F) {
        if (log == "x") {
            sel = which(x > 0)
            x = log10(x[sel])
            y = y[sel]
        }
        if (log == "y") {
            sel = which(y > 0)
            x = x[sel]
            y = log10(y[sel])
        }
        if (log == "xy" | log == "yx") {
            sel = which(x > 0 & y > 0)
            x = log10(x[sel])
            y = log10(y[sel])
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
            breaks = quantile(checkvec, seq(0, 1, len = bins + 
                1))
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
            xmtemp = median(x[binsel], na.rm = TRUE)
            ymtemp = median(y[binsel], na.rm = TRUE)
        }
        if (type == "mean") {
            xmtemp = mean(x[binsel], na.rm = TRUE)
            ymtemp = mean(y[binsel], na.rm = TRUE)
        }
        if (type == "mode") {
            tempdenx = density(x[binsel], na.rm = TRUE)
            tempdeny = density(y[binsel], na.rm = TRUE)
            xmtemp = tempdenx$x[which.max(tempdenx$y)]
            ymtemp = tempdeny$x[which.max(tempdeny$y)]
        }
        outytemp<-length(which(abs(y[binsel])>outlier_thresh))/length(binsel)
        if (type == "mode2d") {
            tempden2d = kde2d(x[binsel], y[binsel])
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
            xqtemp = quantile(x[binsel], ranges)
            yqtemp = quantile(y[binsel], ranges)
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
                xsd = c(xsd, mad(x[binsel])/sqrt(Nbin))
                ysd = c(ysd, mad(y[binsel])/sqrt(Nbin))
            }
            else {
                xsd = c(xsd, mad(x[binsel]))
                ysd = c(ysd, mad(y[binsel]))
            }
        }
        else {
            if (Nscale) {
                xsd = rbind(xsd, c(xmtemp - mad(x[binsel])/sqrt(Nbin), 
                  xmtemp + mad(x[binsel])/sqrt(Nbin)))
                ysd = rbind(ysd, c(ymtemp - mad(y[binsel])/sqrt(Nbin), 
                  ymtemp + mad(y[binsel])/sqrt(Nbin)))
            }
            else {
                xsd = rbind(xsd, c(xmtemp - mad(x[binsel]), xmtemp + 
                  mad(x[binsel])))
                ysd = rbind(ysd, c(ymtemp - mad(y[binsel]), ymtemp + 
                  mad(y[binsel])))
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
