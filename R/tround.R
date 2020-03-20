
#Nice plot rounding /*fold*/{{{
tround<-function(val,digits,lim=TRUE) { 
  str<-round(val,digits)
  index<-which(abs(val) < 10^(-digits))
  str<-paste(str)
  if (length(index)>0) { 
    if (lim) { 
      str[index]<-paste0(" < 10^{-",digits,"}")
    } else { 
      str[index]<-paste0("0.",paste(rep("0",digits-1),collapse=''))
    }
  }
  index<-which(!grepl(".",str[index]))
  if (length(index)>0) { 
    str[index]<-paste0(str[index],'.',paste0(rep(0,digits),collapse=''))
  }
  index<-which(nchar(str) <= digits+1)
  if (length(index)>0) { 
    str[index]<-paste0(str[index],paste0(rep('0',digits+2-nchar(str[index])),collapse=''))
  }
  return=str
}
#/*fend*/}}}
