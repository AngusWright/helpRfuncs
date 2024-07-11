#
# Function for making 2D Histograms
# Created by A.H.Wright (2018-10-22)
#

hist2D<-function(xf,yf,w,z,zfun=median,x.bin,y.bin,nbins=c(25,25),dx=NULL,dy=NULL,zlog=FALSE,xlim=NULL,ylim=NULL,
                 palette=grey.colors,ncol=256,colBar=TRUE,flip=FALSE,colmin=0,colmax=1,inset=0.05,
                 zlim=NULL,barloc='left',orient='v',barscale=c(0.5,1/20),axes=T,useRaster=TRUE,
                 titleshift=1.5,title.cex=1,labels=c(T,T,F,F),side=1:4,label.cex=1,add=FALSE,alpha=1,asp=1,plot=TRUE,badval=0,
                 ...) {
 
  #> Define the title tables before they are evaluated {{{
  if (!missing(z)) { 
    tlab=paste0(as.character(substitute(zfun)),"(",as.character(substitute(z)),")")[1]
  } else if (!missing(w)) { 
    tlab=paste0('Sum(',as.character(substitute(w)),')')[1]
  } else { 
    tlab='Count'
  }
  #print(tlab)
  #}}}
  #Install/Load the required packages {{{
  if (!plot & (axes | colBar)) { 
    axes<-colBar<-FALSE
  }
  if ((axes | colBar) && !require(magicaxis)) { 
    install.packages(magicaxis)
  }
  #}}}
  #Check the inputs {{{
  if (length(xf)!=length(yf)) { stop("Lengths of input X & Y differ") }
  if (!missing(z) && length(xf)!=length(z)) { stop("Lengths of input X & Z differ") }
  if (length(nbins)!=2) { nbins=rep(nbins,2)[1:2] }
  if (any(!is.finite(xf))) { warning("Removing non-finite x-values") }
  if (any(!is.finite(yf))) { warning("Removing non-finite y-values") }
  if (!missing(z) && any(!is.finite(z))) { warning("Removing non-finite z-values") }
  #}}}
  #Subset to the finite data {{{
  if (!missing(z)) { 
    ind<-which(is.finite(xf)&is.finite(yf)&is.finite(z))
  } else { 
    ind<-which(is.finite(xf)&is.finite(yf))
  }
  xf<-xf[ind]
  yf<-yf[ind]
  if (!missing(z)) { 
    z<-z[ind]
  }
  if (!missing(w)) { 
    w<-w[ind]
  }
  #}}}
  #If they don't exist, define the x & y limits {{{
  if (!is.null(xlim)) {
    if (length(xlim)==2 & all(is.finite(xlim))) {
      ind<-which(xf>min(xlim) & xf<max(xlim))
      if (length(ind)==0){
        stop("No data points contained in x-limits provided")
      }
      x<-xf[ind]
      y<-yf[ind]
      if (!missing(z)) { 
        z<-z[ind]
      }
      if (!missing(w)) { 
        w<-w[ind]
      }
    } else {
      stop("Bad xlim values provided")
    }
  } else {
    x<-xf
    y<-yf
    xlim=range(x,na.rm=T)
  }

  if (!is.null(ylim)) {
    if (length(ylim)==2 & all(is.finite(ylim))) {
      ind<-which(y>min(ylim) & y<max(ylim))
      if (length(ind)==0){
        stop("No data points contained in y-limits provided")
      }
      x<-x[ind]
      y<-y[ind]
      if (!missing(w)) { 
        w<-w[ind]
      }
      if (!missing(z)) { 
        z<-z[ind]
      }
    } else {
      stop("Bad ylim values provided")
    }
  } else {
    ylim=range(y,na.rm=T)
  }
  #}}}
  #If bins aren't given {{{
  if (missing(x.bin) & missing(y.bin)) { 
    #If they exist, use the dx and dy values to define nbins {{{ 
    if (!is.null(dx)) { 
      #message("Overwriting nbins[1] with the specified dx!")
      dx<-abs(dx)
      nbins[1]<-ceiling(abs(diff(xlim))/dx)
    } 
    if (!is.null(dy)) { 
      #message("Overwriting nbins[2] with the specified dy!")
      dy<-abs(dy)
      nbins[2]<-ceiling(abs(diff(ylim))/dy)
    } 
    #}}}
    #Set the dx and dy values using nbins {{{
    dx<-abs(diff(xlim))/nbins[1]
    dy<-abs(diff(ylim))/nbins[2]
    #}}}
    #Define the bin edges {{{
    x.bin <- seq(min(xlim), max(xlim), length=nbins[1]+1) #These are the edges 
    y.bin <- seq(min(ylim), max(ylim), length=nbins[2]+1) #These are the edges 
    #}}}
    #Save the bin centers {{{
    bins<-list(x=x.bin[-1]-dx/2,y=y.bin[-1]-dy/2) #These are the centers
    #}}}
  } else { 
    #Save the bin centers {{{
    bins<-list(x=x.bin[-length(x.bin)]+diff(x.bin)/2,y=y.bin[-length(y.bin)]+diff(y.bin)/2) #These are the centers
    #}}}
  } 
  #}}}

  #if (length(inset)>1) { 
  #  warning("magicaxis::magbar only supports a single value for inset. Using the first one provided")
  #  inset<-inset[1]
  #}

  if (!missing(z)) { 
    tmp<-data.frame(x=x,y=y,z=z)
  } else if (!missing(w)) { 
    tmp<-data.frame(x=x,y=y,w=w)
  } else { 
    tmp<-data.frame(x=x,y=y)
  } 

  #Bin the data {{{
  #> for each pixel, find the most common factor and return the equivalent number density
  if (!missing(z)) { 
    freq2D<-with(tmp,tapply(z,list(x=cut(x, breaks=x.bin, include.lowest=T),
                                   y=cut(y, breaks=y.bin, include.lowest=T)),zfun))
  } else if (!missing(w)) { 
    freq2D<-with(tmp,tapply(w,list(x=cut(x, breaks=x.bin, include.lowest=T),
                                   y=cut(y, breaks=y.bin, include.lowest=T)),sum))
  } else { 
    freq2D<-with(tmp,tapply(x,list(x=cut(x, breaks=x.bin, include.lowest=T),
                                   y=cut(y, breaks=y.bin, include.lowest=T)),length))
  }
  freq2D[which(is.na(freq2D))]<-badval
  #}}}
  #Setup the colour palette {{{
  col<-suppressMessages(try(palette(ncol,start=colmin,end=colmax),silent=TRUE))
  if (class(col)=='try-error') {
    col<-palette(ncol)
  }
  if (flip) { col<-rev(col) }
  if (alpha!=1) { 
    col<-unlist(lapply(col,col2alpha,alpha=alpha))
  }
  #}}}
  #Plot the array using either logarithmic scaling or linear scalling {{{
  if (zlog) {
    # Log
    p.freq2D<-freq2D
    if (is.null(zlim)) { 
      zlim=range(log10(freq2D)[which(is.finite(log10(freq2D)))]) 
    } else { 
      p.freq2D[which(log10(p.freq2D)>max(zlim))]<-10^max(zlim) 
      p.freq2D[which(log10(p.freq2D)<min(zlim))]<-10^min(zlim) 
    }
    if (plot) { 
      if (useRaster) { 
        dx<-diff(x.bin)
        dy<-diff(y.bin)
        if (any(dx!=dx[1])|any(dy!=dy[1])) { 
          warning("Cannot use Raster with non-uniform binning") 
          useRaster<-FALSE
        } 
      }
      suppressWarnings(image(bins$x,bins$y, log10(p.freq2D),xlim=xlim,ylim=ylim,col=col,axes=F,xlab="",ylab="",useRaster=useRaster,zlim=zlim,add=add,asp=asp))
    }
  } else {
    # Normal
    p.freq2D<-freq2D
    if (is.null(zlim)) { 
      zlim=range((freq2D)[which(is.finite((freq2D)))]) 
    } else { 
      p.freq2D[which((p.freq2D)>max(zlim))]<-max(zlim) 
      p.freq2D[which((p.freq2D)<min(zlim))]<-min(zlim) 
    }
    if (plot) { 
      if (useRaster) { 
        dx<-diff(x.bin)
        dy<-diff(y.bin)
        if (any(dx!=dx[1])|any(dy!=dy[1])) { 
          warning("Cannot use Raster with non-uniform binning") 
          useRaster<-FALSE
        } 
      }
      suppressWarnings(image(bins$x,bins$y, p.freq2D,xlim=xlim,ylim=ylim,col=col,axes=F,xlab="",ylab="",useRaster=useRaster,zlim=zlim,add=add,asp=asp))
    }
  }
  #}}}
  #Do the axes, if required {{{
  if(axes) { magaxis(side=side,labels=labels,...) }
  #}}}
  #Plot the colour bar, if required {{{
  if (colBar) {
    #Remake the colour palette with low-N (better for the bar) {{{
    col<-suppressMessages(try(palette(100,start=colmin,end=colmax),silent=TRUE))
    if (class(col)=='try-error') {
      col<-palette(ncol)
    }
    if (flip) { col<-rev(col) }
    if (alpha!=1) { 
      col<-unlist(lapply(col,col2alpha,alpha=alpha))
    }
    #}}}
    if (zlog) {
      suppressWarnings(helpRfuncs::magbar(barloc,title=paste0("log(",tlab,")"),range=zlim,col=col,labN=3,scale=barscale,orient=orient,titleshift=titleshift,title.cex=title.cex,cex=label.cex,inset=inset))
    } else {
      suppressWarnings(helpRfuncs::magbar(barloc,title=tlab,range=zlim,col=col,labN=3,scale=barscale,orient=orient,titleshift=titleshift,title.cex=title.cex,cex=label.cex,inset=inset))
    }
  }
  #}}}
  #Return {{{
  return=list(bincen=list(x=bins$x,y=bins$y),breaks=list(x=x.bin,y=y.bin),zlim=zlim,map=freq2D)
  #}}}

}


