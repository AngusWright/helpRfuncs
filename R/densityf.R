#
#
#

#Create the histf function (for when not plotting density)
histf<-function(X,bw,from,to,kern,weights,...){ 
  ind<-which(X > from & X < to )
  breaks<-seq(from-bw,to+bw,by=bw)
  if (missing(weights)) { 
    histr=hist(X[ind],breaks=breaks,plot=F,...)
  } else { 
    histr=weighted.hist(X[ind],weights[ind],breaks=breaks,plot=F,...)
  } 
  histr$x<-histr$mids
  histr$y<-histr$density#/max(histr$density)
  return=data.frame(x=histr$x,y=histr$y)
}

#Create the densityf function (for when not plotting hist)
densityf<-function(x,weight,...) {

  if (!missing(weight)){
     if (length(weight)==1) { 
       weight<-rep(weight,length(x))
     }
     ind<-which(is.finite(x) & is.finite(weight))
     if (length(ind) != 0) { 
       return=density(x[ind],weight=weight[ind],...)
     } else { 
       warning('There are no finite values!')
       return=data.frame(seq(0,1,len=10),rep(0,10))
     }
  } else {
     ind<-which(is.finite(x))
     if (length(ind) != 0) { 
       return=density(x[ind],...)
     } else { 
       warning('There are no finite values!')
       return=data.frame(seq(0,1,len=10),rep(0,10))
     }
  }
}
