#
#
# Append a string to an input filename 
#
#

filename.append<-function(name,string,new.suffix) { 
  #Get the file suffix 
  suffix<-helpRfuncs::vecsplit(name,by='.',fixed=TRUE,n=-1)
  #Strip out the suffix 
  name<-sub(paste0(".",suffix,"$"),"",name)
  #Append the string 
  if (!missing(new.suffix)) { 
    name<-paste0(name,string,'.',new.suffix)
  } else { 
    name<-paste0(name,string,'.',suffix)
  }
  #return
  return=name
}
