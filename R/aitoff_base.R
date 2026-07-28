#=========================================
#
# File Name :
# Created By : awright
# Creation Date : 12-07-2024
# Last Modified : Wed Jun  3 07:34:14 2026
#
#=========================================

aitoff_base<-function(lon,lat,...,col='black',pointsFirst=FALSE,add=FALSE,type='p') { 
  if (add==TRUE) warning("add should not be passed to aitoff_base, and will be ignored") 
  if (pointsFirst) { 
    if (type %in% c("b","pl")) { 
      if (length(dim(lon))!=2) stop("incorrect number of dimensions in lon")
      if (length(dim(lat))!=2) stop("incorrect number of dimensions in lat")
      pb<-txtProgressBar(style=3,min=1,max=nrow(lon))
      for (i in 1:nrow(lon)) { 
        magicaxis::magproj(lon[i,],lat[i,],...,col=col[i],type=type,add=(i!=1))
        setTxtProgressBar(pb,i)
      }
      close(pb)
    } else { 
      magicaxis::magproj(lon,lat,...,col=col,type=type)
    }
  } else { 
    if (type %in% c("b","pl")) { 
      if (length(dim(lon))!=2) stop("incorrect number of dimensions in lon")
      if (length(dim(lat))!=2) stop("incorrect number of dimensions in lat")
      magicaxis::magproj(lon[1,],lat[1,],...,col=NA,type=type)
    } else { 
      magicaxis::magproj(lon,lat,...,col=NA,type=type)
    }
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
    if (type %in% c("b","pl")) { 
      if (length(dim(lon))!=2) stop("incorrect number of dimensions in lon")
      if (length(dim(lat))!=2) stop("incorrect number of dimensions in lat")
      pb<-txtProgressBar(style=3,min=1,max=nrow(lon))
      for (i in 1:nrow(lon)) { 
        magicaxis::magproj(lon[i,],lat[i,],...,col=col[i],type=type,add=TRUE)
        setTxtProgressBar(pb,i)
      }
      close(pb)
    } else { 
      magicaxis::magproj(lon,lat,...,col=col,type=type,add=TRUE)
    }
  }
}
