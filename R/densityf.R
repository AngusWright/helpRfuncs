#
#
#

#Create the histf function (for when not plotting density)
histf<-function(X,bw,from,to,kern,weights,wt.norm=FALSE,density=FALSE,n,...){ 
  bw<-bw*sqrt(12)
  if (!missing(weights)){ 
    if (length(weights)!=length(X)) { 
      weights<-rep(weights,length(X))[1:length(X)]
    }
  }
  if(wt.norm) weights<-weights/sum(weights)
  ind<-which(X > from & X < to )
  breaks<-seq(from-bw,to+bw,by=bw)
  if (missing(weights)) { 
    histr=hist(X[ind],breaks=breaks,plot=F,...)
  } else { 
    histr=plotrix::weighted.hist(X[ind],weights[ind],breaks=breaks,plot=F,...)
  } 
  histr$x<-histr$mids
  if (!density) { 
    histr$y<-histr$count#/diff(histr$x[1:2])
  } else { 
    histr$y<-histr$count/diff(histr$x[1:2])
  } 
  if (!missing(n)) { 
    x<-seq(from,to,len=n)
    y<-approxfun(histr$breaks, c(histr$y,0),method='constant')(x)
    histr$x<-x
    histr$y<-y
  } 
  #if (!density) { 
  #  histr$y<-histr$y/diff(histr$x[1:2])
  #} 
  return=data.frame(x=histr$x,y=histr$y)
}

#Create the densityf function (for when not plotting hist)
densityf<-function(x,weight,wt.norm=FALSE,...) {

  if (!missing(weight)){
     if (length(weight)==1) { 
       weight<-rep(weight,length(x))
     }
     if (wt.norm) weight<-weight/sum(weight)
     ind<-which(is.finite(x) & is.finite(weight))
     if (length(ind) != 0) { 
       dens=density(x[ind],weight=weight[ind],...)
       return=dens
     } else { 
       warning('There are no finite values!')
       return=data.frame(seq(0,1,len=10),rep(0,10))
     }
  } else {
     ind<-which(is.finite(x))
     if (length(ind) != 0) { 
       dens=density(x[ind],...)
       return=dens
     } else { 
       warning('There are no finite values!')
       return=data.frame(seq(0,1,len=10),rep(0,10))
     }
  }
}
