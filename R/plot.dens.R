plot.dens<-function(dens,side=1:4,labels=c(T,T,F,T),
                    cdf.ylim=c(0,1),cdf.col='grey50',cdf.lwd=1,cdf.lty=2,
                    cdf.majorn=3,cdf.ylab='CDF',
                    show.bw=TRUE,bw.col='blue',...) { 
  #
  #A better default density plot 
  #
  require(magicaxis)

  #List of arguments to helpRfuncs::showbw
  bw.dotslist<-c("loc","scale","inset","logbw","as.bw")

  dots<-list(...)
  if (length(dots)>0) { 
    #Extract the CDF dots
    dots.cdf<-dots[grepl("cdf.",names(dots),fixed=T)]
    names(dots.cdf)<-gsub('cdf.','',names(dots.cdf))
    dots<-dots[!grepl("cdf.",names(dots),fixed=T)]
    #Extract the bw dots
    dots.bw<-dots[names(dots)%in%bw.dotslist | grepl("bw.",names(dots),fixed=T)]
    names(dots.bw)<-gsub('bw.','',names(dots.bw))
    dots<-dots[!names(dots)%in%bw.dotslist | grepl("bw.",names(dots),fixed=T)]
  } else { 
    dots.cdf<-{}
    dots.bw<-{}
  }

  #CDF
  cumul<-dens
  cumul$y<-cumsum(cumul$y)
  cumul$y<-cumul$y/max(cumul$y)
  #Base plot
  do.call(magicaxis::magplot,args=c(list(x=dens,labels=labels[side!=4],side=side[side!=4],
                     type='n'),dots))
  #CDF in background
  pusr<-par('usr')
  #Get a nice 1-level for the CDF
  #Distance between tickmark and axis 
  if (par('xaxs')=='r'){
    #Remove the expansion for the buffer calculation
    parlim<-pusr[3:4]+c(1,-1)*diff(pusr[3:4])/1.08*0.04
  } else {
    parlim<-pusr[3:4]
  }
  #Get the largest tickmark
  prettymax<-pretty(parlim,5)
  prettymax<-max(prettymax[prettymax<=max(parlim)])
  max.buffer<-parlim[2]-prettymax
  pardiff<-diff(parlim)
  fbuff<-max.buffer/pardiff
  #Move the upper limit to align the 1-level tickmark
  cdf.ylim[2]<-(cdf.ylim[2]+fbuff*cdf.ylim[1])/(1-fbuff)
  #Get the correct 0-level for the CDF
  if (par('xaxs')=='r') { 
    cdf.ylim=cdf.ylim+c(-1,1)*diff(cdf.ylim)*0.04
  } 
  par(usr=c(pusr[1:2],cdf.ylim))
  do.call(lines,args=c(list(x=cumul$x,y=cumul$y,lwd=cdf.lwd,col=cdf.col,lty=cdf.lty),dots.cdf))
  #CDF axis
  if (any(side==4)) { 
    do.call(magicaxis::magaxis,args=c(list(side=4,labels=labels[side==4],ylab=cdf.ylab,
                       col=cdf.col,col.axis=cdf.col,axis.col=cdf.col,majorn=cdf.majorn),dots.cdf))
  }
  #PDF
  par(usr=pusr)
  do.call(lines,args=c(list(x=dens$x,y=dens$y),dots))
  #bw 
  if (show.bw) { 
    do.call(helpRfuncs::showbw,args=c(list(dens=dens,col=bw.col),dots.bw))
  }
}

