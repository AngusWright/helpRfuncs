#=========================================
#
# File Name : grid.R
# Created By : awright
# Creation Date : 16-12-2023
# Last Modified : Tue 11 Jun 2024 04:47:26 PM CEST
#
#=========================================

make_grid<-function() { 

plot(1,xlim=c(0,360),ylim=c(-90,90),type='n',asp=1,xlab='ecliptic longitude',ylab='ecliptic latitude')
abline(h=c(-90,90),lty=2)
for (i in seq(0,360,by=10)) {
latlong<-eq2ecl(rep(i,len=length(seq(-90,90))),seq(-90,90))
points(latlong[,1],latlong[,2],col=rainbow(361)[i+1],pch='.',cex=0.5)
}

#plot(c(360,0),c(-90,90),type='n',asp=1)
#abline(h=c(-90,90),lty=2)
#declist<-seq(-90,90,by=10)
for (i in declist) {
latlong<-eq2ecl(seq(0,360),rep(i,len=length(seq(0,360))))
points(latlong[,1],latlong[,2],col=rainbow(length(declist))[which(declist==i)],pch='.',cex=0.5)
}

}
