#=========================================
#
# File Name :
# Created By : awright
# Creation Date : 12-07-2024
# Last Modified : Fri 12 Jul 2024 09:19:14 AM UTC
#
#=========================================

aitoff_base<-function(...,col='black',pointsFirst=FALSE,add=FALSE) { 
  if (add==TRUE) warning("add should not be passed to aitoff_base, and will be ignored") 
  if (pointsFirst) { 
    magicaxis::magproj(...,col=col)
  } else { 
    magicaxis::magproj(...,col=NA)
  }
  magicaxis::magecliptic(width=10,col=hsv(1/12,alpha=0.3),border=NA)
  magicaxis::magecliptic(width=0,col='orange')
  magicaxis::magMWplane(width=20,col=hsv(v=0,alpha=0.1),border=NA)
  magicaxis::magMWplane(width=0,col='darkgrey')
  magicaxis::magMW(pch=16, cex=2, col='darkgrey')
  magicaxis::magsun(c(4,19), pch=16, cex=2, col='orange2') #An important date!
  legend('bottomright', legend=c('Ecliptic','MW Plane'), col=c(hsv(c(1/12,0), v=c(1,0),
                                                                alpha=0.5)), pch=c(15,15), lty=c(1,1), bty='n')
  legend('bottomleft', legend=c('Sun [19/04]', 'MW Centre'), col=c('orange2','darkgrey'), pch=16,
         bty='n')
  if (!pointsFirst) { 
    magicaxis::magproj(...,col=col,add=TRUE)
  }
}
