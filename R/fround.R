

fround<-function(X,digits=3,leading,...) { 
  if (!missing(leading)) { 
    leading<-leading+digits+1
  } else {
    leading<-""
  }
  return(sprintf(fmt=paste0('%',leading,'.',digits,'f'),round(X,digits=digits,...)))
}
