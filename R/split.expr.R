#
# Function strips hanging brackets from text expressions
#

split.expr<-function(expr,splitchar='+-=/*^~<>()',outvar="V") { 
  if (grepl("#",expr)) stop("expr cannot have hashes (#) in it!") 
  #Strip whitespace from expression
  expr<-gsub(" ","",expr)
  expr.tmp<-expr
  for (char in helpRfuncs::vecsplit(splitchar,""))
    expr.tmp<-gsub(paste0("\\",char),paste0(" ",char," "),expr.tmp,perl=T)
  comps<-helpRfuncs::vecsplit(expr.tmp,' ')
  comps<-comps[which(comps!="")]
  classes<-unlist(lapply(comps,FUN=function(X) class(try(eval(parse(text=X)),silent=T))))
  if (any(classes=='function')) { 
    close.brace<-0
    combset<-NULL
    for (i in which(classes=='function')) {
      #If the last close.brace was after this function, skip it
      if (close.brace>i) next
      #combine functions into single components
      close.brace<-i+which(cumsum(ifelse(grepl("\\(",comps[-(1:i)]),1,0)+
                                  ifelse(grepl("\\)",comps[-(1:i)]),-1,0))==0)[1]
      if (is.na(close.brace)) stop("There is unmatched braces in expression!")
      comb<-paste(comps[i:close.brace],collapse="")
      combset<-c(combset,comb)
    }
    #remove the functions from the strings
    expr.tmp<-expr
    for (i in 1:length(combset)) { 
      expr.tmp<-gsub(combset[i],"",expr.tmp,fixed=T)
    }
    #for (char in helpRfuncs::vecsplit(splitchar,""))
    #  expr.tmp<-gsub(paste0("\\",char),paste0(" ",char," "),expr.tmp,perl=T)
    #Recalculate the separated components
    comps<-helpRfuncs::vecsplit(gsub('[-+*\\/()]'," ",expr.tmp),' ')
    comps<-comps[which(comps!="")]
    classes<-unlist(lapply(comps,FUN=function(X) class(try(eval(parse(text=X))))))
    if (any(classes=='function')) { stop("function removal failed") } 
    comps<-c(combset,comps)
  }
  expr.tmp<-expr
  for (i in 1:length(comps)) { 
    expr.tmp<-gsub(comps[i],paste0(outvar,i),expr.tmp,fixed=T)
  }
  names(comps)<-paste0(outvar,1:length(comps))
  return=list(comps=comps,replace.expr=expr.tmp)
}
