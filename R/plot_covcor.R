#=========================================
#
# File Name :
# Created By : awright
# Creation Date : 29-07-2024
# Last Modified : Wed Jan 29 15:25:50 2025
#
#=========================================


plot_covcor<-function(X,type='cor',text=TRUE,col=hcl.colors(100),dx=0,...) { 
  if (is.list(X)) { 
    if (type!='cor') { 
      stop("list format X can only be used with correlation plot") 
    }
    if (length(X)>2) { 
      stop("list format requires precisely 2 matrices/datasets") 
    }
    Y<-X[[2]]
    X<-X[[1]]
  }
  if (exists("Y")) { 
    if (ncol(Y)!=nrow(Y)) { 
      Y<-cor(Y)
    }
  }
  if (ncol(X)!=nrow(X)) { 
    if (type=='cov') { 
      X<-cov(X)
    } else { 
      X<-cor(X)
    } 
  }
  if (exists("Y")) { 
    ind<-which(!lower.tri(Y),arr.ind=T)
    XTMP<-X
    XTMP[ind]<-NA
    ind.y<-which(!upper.tri(Y),arr.ind=T)
    Y[ind.y]<-NA
    ind.y<-which(upper.tri(Y),arr.ind=T)
    image(x=1:ncol(X)+dx,y=1:nrow(X)-dx,z=XTMP,col=col,xlim=c(0.5-dx,ncol(X)+0.5+dx),ylim=c(nrow(X)+0.5,0.5),...)
    image(x=1:ncol(X)-dx,y=1:nrow(X)+dx,z=Y,col=col,add=TRUE,...)
  } else { 
    image(x=1:ncol(X),y=1:nrow(X),z=X,col=col,xlim=c(0.5,ncol(X)+0.5),ylim=c(nrow(X)+0.5,0.5),...)
  }
  xy<-expand.grid(1:ncol(X),1:nrow(X))
  if (text) { 
    if (exists("Y")) { 
      ind<-which(!is.na(XTMP))
      text(x=xy[ind,1]+dx,y=xy[ind,2]-dx,labels=sprintf("%0.2f",round(XTMP[ind],digits=2)))
      ind<-which(!is.na(Y))
      text(x=xy[ind,1]-dx,y=xy[ind,2]+dx,labels=sprintf("%0.2f",round(Y[ind],digits=2)))
    } else { 
      text(x=xy[,1],y=xy[,2],labels=sprintf("%0.2f",round(X,digits=2)))
    }
  }
  if (exists("Y")) { 
    return=list(X,Y)
  } else { 
    return=X
  }
}
