#=========================================
#
# File Name : scatter.plot.R
# Created By : awright
# Creation Date : 14-11-2023
# Last Modified : Tue 14 Nov 2023 11:35:17 PM CET
#
#=========================================

scatter.plot<-function(x,y,...,xlim=NULL,ylim=NULL,pch=1) { 
  magicaxis::magplot(x=x,y=y,...,xlim=xlim,ylim=ylim,pch=pch)
  if (!is.null(xlim)) { 
    xind<-which(x>max(xlim))
    points(rep(max(xlim),length(xind)),y[xind],pch=ifelse(xlim[1]>xlim[2],"<",">"),...)
    xind<-which(x<min(xlim))
    points(rep(min(xlim),length(xind)),y[xind],pch=ifelse(xlim[1]>xlim[2],">","<"),...)
  }
  if (!is.null(ylim)) { 
    yind<-which(y>max(ylim))
    points(y=rep(max(ylim),length(yind)),x[yind],pch=ifelse(ylim[1]>ylim[2],"v","^"),...)
    yind<-which(y<min(ylim))
    points(y=rep(min(ylim),length(yind)),x[yind],pch=ifelse(ylim[1]>ylim[2],"^","v"),...)
  }
}
