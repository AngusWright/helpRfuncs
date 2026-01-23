#=========================================
#
# File Name : plot.chain.R
# Created By : awright
# Creation Date : 24-07-2023
# Last Modified : Wed Feb  5 06:30:32 2025
#
#=========================================

#plot.chain<-function(chain,colkeep="COSMOL|weight",coldel="COSMOMC",labelrm="COSMOLOGICAL_PARAMETERS--",...) { 
plot.chain<-function(chainlist,colkeep,coldel,labelrm="COSMOLOGICAL_PARAMETERS--",drop_fixed=TRUE,...) { 
  if (class(chainlist)!='list') { 
    chainlist<-list(chainlist)
  }
  for (i in 1:length(chainlist)) { 
    chain<-chainlist[[i]]
    if (any(colnames(chain)=='log_weight')) { 
      warning(paste('replacing log_weight column with linear weight'))
      chain$weight<-exp(chain$log_weight)
      chain$log_weight<-NULL
    }
    if (!missing(colkeep)) { 
      chain<-chain[,grepl(paste0(colkeep,"|weight"),colnames(chain)),with=F]
    }
    if (!missing(coldel)) { 
      if (length(coldel)>0) { 
        chain<-chain[,!grepl(coldel,colnames(chain)),with=F]
      }
    }
    colnames(chain)<-gsub(labelrm,"",colnames(chain))
    mfun<-function(X) mean(as.numeric(X),na.rm=T)
    class<-unlist(lapply(chain,FUN=class))
    if (any(class!='numeric')) { 
      warning(paste('typecasting non-numeric columns:',paste(collapse=' ',colnames(chain)[which(class!='numeric')])))
      for (col in colnames(chain)[which(class!='numeric')]) 
        chain[[col]]<-as.numeric(chain[[col]])
    }
    val<-unlist(lapply(chain,FUN=mfun))
    if (any(is.nan(val))) { 
      warning(paste('removing fully missing columns:',paste(collapse=' ',colnames(chain)[which(is.nan(val))])))
      chain<-chain[,which(!is.nan(val)),with=F]
    }
    rdiff<-function(X) diff(range(X,na.rm=T))
    range<-unlist(lapply(chain,FUN=rdiff))
    if (any(range==0) & drop_fixed) { 
      warning(paste('removing columns with single-valued columns:',paste(collapse=' ',colnames(chain)[which(range==0)])))
      chain<-chain[,which(range!=0),with=F]
    }
    chainlist[[i]]<-chain
  }
  res=triplot(chainlist,...)
  return=res
}
