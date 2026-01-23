triplot<-function (chains, samples = Inf, thin = 1, samptype = "end", col,
    grid = FALSE, do.tick = FALSE, refvals = NULL, chain.alpha=0.5,
    weightname='weight', conlevels=c(pnorm(1)-pnorm(-1),pnorm(2)-pnorm(-2)),
    con.lty=c(1,2),lwd=1,do.points=TRUE,fill=FALSE,fill.col=NA,majorn=3,
    ngrid=200,columnlabmap,labels,rangefrom,ranges,bw,...) 
{
  if (class(chains)[1]!='list') { 
    chains<-list(chains)
  }
  if (!missing(labels)) { 
    if (length(labels)!=length(chains)) stop("Supplied chain labels are not the same length as the number of supplied chains!")
    labels<-rev(labels)
  }
  if (missing(ranges)) { 
    ranges<-list()
  }
  if (missing(rangefrom)) { 
    rangefrom<-1:length(chains)
  } else { 
    rangefrom<-(length(chains):1)[rangefrom]
  }
  if (missing(col)) col<-rev(RColorBrewer::brewer.pal(8,'Set2')[1:length(chains)])
  chains<-rev(chains)
  col<-rev(col)
  if (fill==TRUE) { 
    fill<-length(chains):1
  } else if (fill==FALSE) { 
    fill<-NULL
  } else if (is.numeric(fill)) { 
    fill<-(length(chains):1)[fill]
  }
  weight<-list()
  allcolnames<-NULL
  for (ch in 1:length(chains)) { 
    chain = as.data.frame(chains[[ch]])
    chaincolnames = colnames(chain)
    #print(colnames(chain))
    if (any(chaincolnames==weightname)) {
      chain<-chain[which(chain[[weightname]]!=0),]
      chain<-chain[which(is.finite(chain[[weightname]])),]
      weight[[ch]]<-chain[[weightname]]
      chain[[weightname]]<-NULL
      weighted.sd<-function(x,wt,...) { 
        xm<-weighted.mean(x,wt,...)
        v<-weighted.mean((x-xm)^2, wt,...)
        return=sqrt(v)
      }
      chaincolnames = colnames(chain)
      allcolnames<-unique(c(chaincolnames,allcolnames))
    } else {
      weight[[ch]]<-rep(1,nrow(chain))
    }
    chains[[ch]]<-chain
  }
  for (ch in 1:length(chains)) { 
    if (!all(allcolnames%in%colnames(chains[[ch]]))) { 
      stop(paste0("missing column(s)",paste(allcolnames[which(!allcolnames%in%colnames(chains[[ch]]))],collapse=" "),"in chain number ",ch))
    }
    if (any(allcolnames!=colnames(chains[[ch]]))) { 
      chains[[ch]]<-chains[[ch]][,allcolnames]
    }
  }

  Nsamp = dim(chains[[1]])[1]
  Npar = dim(chains[[1]])[2]
  if (!is.null(refvals)) {
    if (length(refvals) != Npar) {
      stop("Length of refvales must be equal to number of parameters!")
    }
  }
  if (Npar <= 1) {
    stop("Need 2+ parameters!")
  }
  if (thin > 1) {
    for (i in 1:length(chains)) { 
      chains[[i]] = chains[[i]][seq(1, Nsamp, by = thin), , drop = FALSE]
    }
  }
  if (!missing(labels) & do.points) { 
    layout(cbind(matrix(1:Npar^2, Npar, Npar)[Npar:1,Npar:1],rep(Npar(Npar^2+1))))
  } else if (do.points) { 
    layout(cbind(matrix(1:Npar^2, Npar, Npar)[Npar:1,Npar:1]))
  } else if (!missing(labels)) { 
    mat<-matrix(0, Npar, Npar)
    count<-0
    for (i in 1:Npar) {
      for (j in 1:Npar) {  
        if (i >= j) { 
          count<-count+1
          mat[j,i]<-count
        } 
      }
    }
    mat<-mat[Npar:1,Npar:1]
    mat[1:(Npar/2),Npar:(Npar/2+1)]<-count+1
    #print(mat)
    layout(mat)
  }

  meanvec = matrix(NA,ncol=Npar,nrow=length(chains))
  sdvec = matrix(NA,ncol=Npar,nrow=length(chains))
  h = matrix(NA,ncol=Npar,nrow=length(chains))

  usesamps<-list()
  for (ch in 1:length(chains)) { 
    Nsamp = dim(chains[[ch]])[1]
    stemp<-samples
    if (stemp > Nsamp) {
      stemp = Nsamp
    }
    if (stemp != Nsamp) { 
      if (samptype == "end") {
        usesamps[[ch]] = (Nsamp - stemp + 1):Nsamp
      }
      if (samptype == "ran") {
        usesamps[[ch]] = sample(Nsamp, stemp)
      }
      if (samptype == "thin") {
        usesamps[[ch]] = seq(1, Nsamp, length = stemp)
      }
    } else { 
      usesamps[[ch]]<-1:Nsamp
    }
  }
  for (i in 1:Npar) {
    for (ch in 1:length(chains)) { 
      if (is.null(weight[[ch]])) { 
        meanvec[i,ch] = mean(chains[[ch]][usesamps[[ch]], i],na.rm=T)
        sdvec[i,ch] = sd(chains[[ch]][usesamps[[ch]], i],na.rm=T)
        h[ch,i] = sm::h.select(x = chains[[ch]][usesamps[[ch]], i], y = NA, nbins = 0)
      } else { 
        #print(str(weight[[ch]]))
        #print(str(usesamps[[ch]]))
        #print(str(chains[[ch]][,i]))
        meanvec[ch,i] = weighted.mean(chains[[ch]][usesamps[[ch]], i],w=weight[[ch]][usesamps[[ch]]],na.rm=T)
        sdvec[ch,i] = weighted.sd(chains[[ch]][usesamps[[ch]], i],w=weight[[ch]][usesamps[[ch]]],na.rm=T)
        h[ch,i] = sm::h.select(x = chains[[ch]][usesamps[[ch]], i], y = NA, weights = weight[[ch]][usesamps[[ch]]], nbins = 0)
      }
    }
  }
  #bw<-matrixStats::colMins(sdvec,na.rm=T)/2
  if (missing('bw')) { 
    bw<-colMeans(h,na.rm=T)
  } else if (length(bw)!=ncol(h)) { 
    warning("misspecified bw length: must be equal to the number of columns; using internal calculation!") 
    bw<-colMeans(h,na.rm=T)*1.1
  } 
  print(bw)
  #print(length(chains))
  par(oma = c(4.1, 4.1, 1.1, 1.1))
  for (i in 1:Npar) {
    for (j in 1:Npar) {
      par(mar = c(0, 0, 0, 0))
      xrange<-yrange<-NULL
      for (ch in rangefrom) { 
        #xrange = range(c(xrange,range(chains[[ch]][usesamps[[ch]], i],na.rm=T)))
        #yrange = range(c(yrange,range(chains[[ch]][usesamps[[ch]], j],na.rm=T)))
        if (chaincolnames[i]%in%names(ranges)) { 
          xrange<-ranges[[chaincolnames[i]]]
        } else {
          if (is.null(weight[[ch]])) { 
            tmp<-density(chains[[ch]][usesamps[[ch]], i],n=1e5,na.rm=T,bw=bw[i])
          } else { 
            tmp<-density(chains[[ch]][usesamps[[ch]], i],weight=weight[[ch]][usesamps[[ch]]]/sum(weight[[ch]][usesamps[[ch]]]),n=1e5,na.rm=T,bw=bw[i])
          } 
          csum<-cumsum(tmp$y)/sum(tmp$y)
          xrange<-range(c(xrange,tmp$x[which(csum>=0.025)[1]]-sdvec[ch,i],tmp$x[which(csum>=0.975)[1]]+sdvec[ch,i]))
        }
        if (chaincolnames[j]%in%names(ranges)) { 
          yrange<-ranges[[chaincolnames[j]]]
        } else {
          if (is.null(weight[[ch]])) { 
            tmp<-density(chains[[ch]][usesamps[[ch]], j],n=1e5,na.rm=T,bw=bw[j])
          } else { 
            tmp<-density(chains[[ch]][usesamps[[ch]], j],weight=weight[[ch]][usesamps[[ch]]]/sum(weight[[ch]][usesamps[[ch]]]),n=1e5,na.rm=T,bw=bw[j])
          } 
          csum<-cumsum(tmp$y)/sum(tmp$y)
          yrange<-range(c(yrange,tmp$x[which(csum>=0.025)[1]]-sdvec[ch,j],tmp$x[which(csum>=0.975)[1]]+sdvec[ch,j]))
        }
      }
      if (xrange[1] == xrange[2]) {
        val = xrange[1]
        xrange[1] = val - 0.05
        xrange[2] = val + 0.05
      }
      if (yrange[1] == yrange[2]) {
        val = yrange[1]
        yrange[1] = val - 0.05
        yrange[2] = val + 0.05
      }
      print(c(xrange,yrange))
      if (i == j) {
        dlist<-list()
        ymax<--Inf
        for(ch in 1:length(chains)) { 
          xtemp = chains[[ch]][usesamps[[ch]], i]
          if (sd(xtemp,na.rm=T) == 0) {
            xtemp = xtemp + rnorm(length(xtemp), sd = bw[i]/1e4)
          }
          if (is.null(weight[[ch]])) { 
            dlist[[ch]]<-density(xtemp,na.rm=T,kern='rect',bw=bw[i]/sqrt(12))
          } else { 
            dlist[[ch]]<-density(xtemp,weight=weight[[ch]][usesamps[[ch]]]/sum(weight[[ch]][usesamps[[ch]]]),
                                 na.rm=T,kern='rect',bw=bw[i]/sqrt(12))
          }
          if (ch %in% rangefrom) { 
            ymax<-max(ymax,max(dlist[[ch]]$y))
          }
        }

        plot.new()
        plot.window(xlim=xrange,ylim=c(0,ymax))
        for (ch in 1:length(chains)) { 
          lines(dlist[[ch]],col=col[ch],lwd=lwd,...)
          err<-HDInterval::hdi(dlist[[ch]],credMass=diff(pnorm(c(-1,1))),allowSplit=FALSE)
          ind<-which(dlist[[ch]]$x>=min(err) & dlist[[ch]]$x<=max(err))
          polygon(dlist[[ch]]$x[c(ind[1],ind,ind[length(ind)])],c(-ymax,dlist[[ch]]$y[ind],-ymax),
                  col=ifelse(ch%in%fill,seqinr::col2alpha(col[ch],0.3),NA),
                  border=seqinr::col2alpha(col[ch],0.4),lty=1,lwd=lwd/2)
        }
        magicaxis::magaxis(1, grid = grid, grid.col = "lightgrey", majorn=majorn,
                           labels = FALSE, do.tick = do.tick,family=par("family"))
        #abline(v = meanvec[i], lty = 1, col = "red")
        #abline(v = meanvec[i] - sdvec[i], lty = 3, col = "red")
        #abline(v = meanvec[i] + sdvec[i], lty = 3, col = "red")
        if (!is.null(refvals)) {
          abline(v = refvals[i], lty = 1, col = "blue")
        }
        box()
        if (i == 1) {
          if (missing(columnlabmap)) {
            magicaxis::magaxis(1, xlab = chaincolnames[i],majorn=majorn,family=par("family"))
          }
          else {
            if (length(columnlabmap[[chaincolnames[i]]])!=0) { 
              magicaxis::magaxis(1, xlab = parse(text=columnlabmap[[chaincolnames[i]]]),majorn=majorn,family=par("family"))
            } else if (length(columnlabmap)==length(chaincolnames)) { 
              magicaxis::magaxis(1, xlab = parse(text=columnlabmap[[i]]),majorn=majorn,family=par("family"))
            } else { 
              warning("no column label found in mapping list!")
              magicaxis::magaxis(1, xlab = chaincolnames[i],majorn=majorn,family=par("family"))
            }
          }
        }
        if (i == Npar) {
          if (missing(columnlabmap)) {
            magicaxis::magaxis(2, ylab = chaincolnames[j],majorn=majorn,family=par("family"))
          }
          else {
            if (length(columnlabmap[[chaincolnames[j]]])!=0) { 
              magicaxis::magaxis(2, ylab = parse(text=columnlabmap[[chaincolnames[j]]]),majorn=majorn,family=par("family"))
            } else if (length(columnlabmap)==length(chaincolnames)) { 
              magicaxis::magaxis(2, ylab = parse(text=columnlabmap[[j]]),majorn=majorn,family=par("family"))
            } else { 
              warning("no column label found in mapping list!")
              magicaxis::magaxis(2, ylab = chaincolnames[j],majorn=majorn,family=par("family"))
            }
          }
        }
      }
      else {
        if (i > j) {
          plot.new()
          #print(rbind(c(i,j),xrange,yrange))
          plot.window(xlim = xrange, ylim = yrange)
          for (ch in 1:length(chains)) { 
            xtemp = chains[[ch]][usesamps[[ch]], i]
            ytemp = chains[[ch]][usesamps[[ch]], j]
            if (sd(xtemp,na.rm=T) == 0) {
              xtemp = xtemp + rnorm(length(xtemp), sd = bw[i]*0.001)
            }
            if (sd(ytemp,na.rm=T) == 0) {
              ytemp = ytemp + rnorm(length(ytemp), sd = bw[j]*0.001)
            }
            filler<-NULL; alpha<-0.8
            for (lev in 1:length(conlevels)) { 
              alpha<-alpha-0.2
              filler<-c(filler,seqinr::col2alpha(col[ch],alpha))
            }
            if (is.null(weight[[ch]])) { 
              helpRfuncs::contour(xtemp, ytemp, dobar = FALSE, doim = FALSE, 
                                  fill=(ch%in%fill),
                                  fill.col=filler,lwd=lwd,h=c(bw[i],bw[j]),
                                  add = TRUE, lty = con.lty, xlim = xrange, col=col[ch],
                                  ylim = yrange, #h = c(diff(xrange), diff(yrange))/10, 
                                  conlevels = conlevels,nbins=0,ngrid=ngrid,
                                  ...)
            } else { 
              helpRfuncs::contour(xtemp, ytemp, weight=weight[[ch]][usesamps[[ch]]]/sum(weight[[ch]][usesamps[[ch]]]),
                                dobar = FALSE, doim = FALSE, h=c(bw[i],bw[j]),
                                fill=(ch%in%fill),
                                fill.col=filler,lwd=lwd,
                                add = TRUE, lty = con.lty, xlim = xrange, col=col[ch],
                                ylim = yrange, #h = c(diff(xrange), diff(yrange))/10, 
                                conlevels = conlevels,nbins=0,ngrid=ngrid,
                                ...)
            }
          }
          magicaxis::magaxis(1:2, grid = grid, grid.col = "lightgrey", 
                             labels = FALSE, do.tick = do.tick,majorn=majorn,family=par("family"))
          #points(meanvec[i], meanvec[j], col = "red", 
          #  pch = 4, cex = 2)
          box()
          #abline(v = meanvec[i], lty = 1, col = "red")
          #abline(v = meanvec[i] - sdvec[i], lty = 3, 
          #  col = "red")
          #abline(v = meanvec[i] + sdvec[i], lty = 3, 
          #  col = "red")
          if (!is.null(refvals)) {
            abline(v = refvals[i], lty = 1, col = "blue")
          }
          if (j == 1) {
            if (missing(columnlabmap)) {
              magicaxis::magaxis(1, xlab = chaincolnames[i],majorn=majorn,family=par("family"))
            }
            else {
              if (length(columnlabmap[[chaincolnames[i]]])!=0) { 
                magicaxis::magaxis(1, xlab = parse(text=columnlabmap[[chaincolnames[i]]]),majorn=majorn,family=par("family"))
              } else if (length(columnlabmap)==length(chaincolnames)) { 
                magicaxis::magaxis(1, xlab = parse(text=columnlabmap[[i]]),majorn=majorn,family=par("family"))
              } else { 
                warning("no column label found in mapping list!")
                magicaxis::magaxis(1, xlab = chaincolnames[i],majorn=majorn,family=par("family"))
              }
            }
          }
          if (i == Npar) {
            if (missing(columnlabmap)) {
              magicaxis::magaxis(2, ylab = chaincolnames[j],majorn=majorn,family=par("family"))
            }
            else {
              if (length(columnlabmap[[chaincolnames[j]]])!=0) { 
                magicaxis::magaxis(2, ylab = parse(text=columnlabmap[[chaincolnames[j]]]),majorn=majorn,family=par("family"))
              } else if (length(columnlabmap)==length(chaincolnames)) { 
                magicaxis::magaxis(2, ylab = parse(text=columnlabmap[[j]]),majorn=majorn,family=par("family"))
              } else { 
                warning("no column label found in mapping list!")
                magicaxis::magaxis(2, ylab = chaincolnames[j],majorn=majorn,family=par("family"))
              }
            }
          }
        }
        else {
          if (!do.points & !missing(labels)) { next }
          plot.new()
          if (!do.points) { next } 
          #print(rbind(c(i,j),xrange,yrange))
          plot.window(xlim = xrange, ylim = yrange)
          for (ch in 1:length(chains)) { 
            if (is.null(weight[[ch]])) { 
              points(chains[[ch]][usesamps[[ch]], c(i, j)], pch = ".", 
                    col = seqinr::col2alpha(col[ch],chain.alpha))
            } else { 
              points(chains[[ch]][usesamps[[ch]], c(i, j)], pch = ".", 
                    cex=magicaxis::magmap(weight[[ch]][usesamps[[ch]]],range=c(0,1),lo=0,hi=0.9)$map,
                    col = seqinr::col2alpha(col[ch],chain.alpha))
            }
          }
          magicaxis::magaxis(1:2, grid = grid, grid.col = "lightgrey", 
                             labels = FALSE, do.tick = do.tick,majorn=majorn,family=par("family"))
          #points(meanvec[i], meanvec[j], col = "red", 
          #  pch = 4, cex = 2)
          box()
        }
      }
    }
  }
  if (!missing(labels)) { 
    plot.new()
    plot.window(xlim = c(0,1), ylim = c(0,1))
    legend('left',legend=parse(text=rev(labels)),col=rev(col),lty=1,lwd=1.5,pch=ifelse(length(chains):1%in%fill,15,NA),bty='n',cex=2,pt.cex=2.5)
  } 
  output = cbind(t(meanvec), t(sdvec))
  rownames(output) = chaincolnames
  return(invisible(output))
}
