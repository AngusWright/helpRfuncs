
#Show bandwidth function {{{
showbw<-function(dens,kernel=NULL,loc='topleft',scale=0.2,inset=c(0.1,0.1),cex=1,col='black',type='s',logbw=TRUE,as.bw=TRUE,lwd=1,labels=TRUE) {
  #Get location parameters
  usercoord = par()$usr
  xlogcheck = FALSE
  ylogcheck = FALSE
  if (par()$xlog) {
    par(xlog = FALSE)
    par(usr = c(log10(par()$usr[1:2]), par()$usr[3:4]))
    xlogcheck = TRUE
  }
  if (par()$ylog) {
    par(ylog = FALSE)
    par(usr = c(par()$usr[1:2], log10(par()$usr[3:4])))
    ylogcheck = TRUE
  }
  if (length(inset)==1) {
    inset<-c(inset,inset)
  }
  xlo = usercoord[1]
  xhi = usercoord[2]
  ylo = usercoord[3]
  yhi = usercoord[4]
  xdiff = xhi - xlo
  ydiff = yhi - ylo
  xl = xlo + xdiff/2 - xdiff * scale/2
  yb = ylo + ydiff/2 - ydiff * scale/2
  xr = xlo + xdiff/2 + xdiff * scale/2
  yt = ylo + ydiff/2 + ydiff * scale/2
  if (grepl("bottom",loc)) {
    yb = ylo + ydiff * inset[2]
    yt = ylo + ydiff * inset[2] + ydiff * scale
  }
  if (grepl("top",loc)) {
    yb = yhi - ydiff * inset[2] - ydiff * scale
    yt = yhi - ydiff * inset[2]
  }
  if (grepl("left",loc)) {
    xl = xlo + xdiff * inset[1]
    xr = xlo + xdiff * inset[1] + xdiff * scale
  }
  if (grepl("right",loc)) {
    xl = xhi - xdiff * inset[1] - xdiff * scale
    xr = xhi - xdiff * inset[1]
  }
  dy<-yt-yb
  dx<-xr-xl
  #Get density information from call
  if (is.null(kernel)) {
    kernel<-dens$call$kernel
    if (is.null(kernel)) { kernel<-'gaussian' }
  }
  #Get number information from call
  dens.n<-dens$n
  if (is.null(dens.n)) { dens.n<-512 }
  bw<-dens$bw
  #Create Kernel
  kern<-density(rep(0,10),from=-xdiff/2,to=xdiff/2,bw=bw,kernel=kernel,na.rm=TRUE,n=dens.n)
  kern.sum<-sum(kern$y)
  kern$y<-kern$y/max(kern$y)
  #kern$y[which(zapsmall(hanning.smooth(kern$y,3))==0)]<-NA
  kern$y[rev(which(zapsmall(kern$y[1:which.max(kern$y)])==0))[-3:-1]]<-NA
  kern$y[which.max(kern$y):length(kern$y)][which(zapsmall(kern$y[which.max(kern$y):length(kern$y)])==0)[-3:-1]]<-NA
  #ind<-range(which(!is.na(kern$y)))
  #kern$y<-kern$y[ind[1]:ind[2]]
  #kern$x<-kern$x[ind[1]:ind[2]]
  kern$y<-dy*(kern$y*0.8 + 0.1) + yb
  kern$x<-kern$x+xl+dx/2
  if (labels) {
    text(xl+dx/2,max(kern$y,na.rm=T),lab="Density Kernel",cex=cex,pos=3,col=col)
    if (kernel=='rect' & !as.bw) { 
      if (logbw) { 
        text(xl+dx/2,min(kern$y,na.rm=T),lab=bquote(paste("log"[10],"(width) = ",.(fsignif(log10(kern$bw*sqrt(12)),digits=2)))),cex=cex,pos=1,col=col)
      } else { 
        text(xl+dx/2,min(kern$y,na.rm=T),lab=bquote(paste("width = ",.(fsignif(kern$bw*sqrt(12),digits=2)))),cex=cex,pos=1,col=col)
      }
    } else { 
      if (logbw) { 
        text(xl+dx/2,min(kern$y,na.rm=T),lab=bquote(paste("log"[10],"(bw) = ",.(fsignif(log10(kern$bw),digits=2)))),cex=cex,pos=1,col=col)
      } else { 
        text(xl+dx/2,min(kern$y,na.rm=T),lab=bquote(paste("bw = ",.(fsignif(kern$bw,digits=2)))),cex=cex,pos=1,col=col)
      }
    } 
  }

  lines(kern,col=col,type=type,lwd=lwd)
  par(xlog = xlogcheck)
  par(ylog = ylogcheck)
  par(usr = usercoord)
  return=kern.sum
}
#}}}
fsignif<-function(x,digits) gsub('-','\U2212',sapply(signif(x,digits), sprintf, fmt=paste0("%#.",digits,"g")))
fround<-function(x,digits) gsub('-','\U2212',gsub('-0.00','0.00',(sapply(round(x,digits), sprintf, fmt=paste0("%#.",digits,"f")))))

hanning.smooth <-
function(arr1,degree=3) {
#Perform Hanning smoothing on 2D data

  #Determine degree {{{
  degree<-abs(degree)
  #}}}
  #If degree is valid, smooth {{{
  if (degree%%2!=1) {
    #Degree is even -> Error {{{
    stop("Hanning degree must be odd")
    #}}}
  } else if (degree > 1) {
    #Initialise {{{
    fact<-1
    cumul<-1
    sm.arr1<-arr1
    #}}}
    #Perform Unnormalised Smoothing over data {{{
    for (i in 2:ceiling(degree/2)) {
      fact<-0.5+0.5*(cos((pi*abs(i))/ceiling(degree/2)))
      cumul<-cumul+fact
      index<-1-i
      sm.arr1<-sm.arr1+fact*elementshift(arr1,index)
      sm.arr1<-sm.arr1+fact*elementshift(arr1,-index)
    }#}}}
    #Normalise and return {{{
    return=sm.arr1/cumul
    #}}}
  } else {
    #Smoothing does nothing: Return {{{
    return=arr1
    #}}}
  }
  #}}}
}
