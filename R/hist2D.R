#
# Function for making 2D Histograms
# Created by A.H.Wright (2018-10-22)
#

hist2D<-function(xf,yf,nbins=c(25,25),dx=NULL,dy=NULL,zlog=FALSE,xlim=NULL,ylim=NULL,
                 palette=grey.colors,ncol=256,colBar=TRUE,flip=FALSE,colmin=0,colmax=1,
                 zlim=NULL,barloc='topleft',orient='v',axes=T,useRaster=TRUE,
                 titleshift=1.0,add=FALSE,alpha=1,asp=1,...) {
 
  #Install/Load the required packages {{{
  if ((axes | colBar) && !require(magicaxis)) { 
    install.packages(magicaxis,)
  }
  #}}}
  #Check the inputs {{{
  if (length(xf)!=length(yf)) { stop("Lengths of input X & Y differ") }
  if (length(nbins)!=2) { nbins=rep(nbins,2)[1:2] }
  if (any(!is.finite(xf))) { warning("Removing non-finite x-values") }
  if (any(!is.finite(yf))) { warning("Removing non-finite y-values") }
  #}}}
  #Subset to the finite data {{{
  ind<-which(is.finite(xf)&is.finite(yf))
  xf<-xf[ind]
  yf<-yf[ind]
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
    } else {
      stop("Bad ylim values provided")
    }
  } else {
    ylim=range(y,na.rm=T)
  }
  #}}}
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
  tmp<-data.frame(x=x,y=y)
  #}}}
  #Bin the data {{{
  #> for each pixel, find the most common factor and return the equivalent number density
  bins<-list(x=x.bin[-1]-dx/2,y=y.bin[-1]-dy/2) #These are the centers
  freq2D<-with(tmp,tapply(x,list(x=cut(x, breaks=x.bin, include.lowest=T),
                                    y=cut(y, breaks=y.bin, include.lowest=T)),length))
  freq2D[which(is.na(freq2D))]<-0
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
    if (is.null(zlim)) { zlim=range(log10(freq2D)[which(is.finite(log10(freq2D)))]) } else { freq2D[which(log10(freq2D)>max(zlim))]<-10^max(zlim) }
    suppressWarnings(image(bins$x,bins$y, log10(freq2D),xlim=xlim,ylim=ylim,col=col,axes=F,xlab="",ylab="",useRaster=useRaster,zlim=zlim,add=add,asp=asp))
  } else {
    # Normal
    if (is.null(zlim)) { zlim=range((freq2D)[which(is.finite((freq2D)))]) } else { freq2D[which((freq2D)>max(zlim))]<-max(zlim) }
    suppressWarnings(image(bins$x,bins$y, freq2D,xlim=xlim,ylim=ylim,col=col,axes=F,xlab="",ylab="",useRaster=useRaster,zlim=zlim,add=add,asp=asp))
  }
  #}}}
  #Do the axes, if required {{{
  if(axes) { magaxis(...) }
  #}}}
  #Plot the colour bar, if required {{{
  if (colBar) {
    if (zlog) {
      suppressWarnings(magbar(barloc,title="log(Count)",range=zlim,col=col,labN=3,scale=c(0.2,1/20),orient=orient,titleshift=titleshift))
    } else {
      suppressWarnings(magbar(barloc,title="Count",range=zlim,col=col,labN=3,scale=c(0.2,1/20),orient=orient,titleshift=titleshift))
    }
  }
  #}}}
  #Return {{{
  return=list(bincen=list(x=bins$x,y=bins$y),zlim=zlim,map=freq2D)
  #}}}

}


