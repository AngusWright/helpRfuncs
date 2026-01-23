#=========================================
#
# File Name :
# Created By : awright
# Creation Date : 27-03-2024
# Last Modified : Wed 27 Mar 2024 12:22:49 PM CET
#
#=========================================

weighted.var<-function(x,wt,...) { 
  xm<-weighted.mean(x,wt,...)
  v<-weighted.mean((x-xm)^2, wt,...)
  return=v
}

weighted.sd<-function(x,wt,...) { 
  return=sqrt(weighted.var(x,wt,...))
}


