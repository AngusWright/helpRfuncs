#
# Function strips hanging brackets from text expressions
#

split.expr<-function(expr,splitchar='+-=/*^~<>()',outvar="V",ignore=c("abs","sqrt")) { 
  if (grepl("#",expr)) stop("expr cannot have hashes (#) in it!") 
  #Strip whitespace from expression
  expr<-gsub(" ","",expr)
  expr.tmp<-expr
  #Remove any of the ignorable functions 
  for (func in ignore)
    expr.tmp<-gsub(func,"",expr.tmp,fixed=T)
  #Pad the split characters with spaces
  for (char in helpRfuncs::vecsplit(splitchar,""))
    expr.tmp<-gsub(paste0("\\",char),paste0(" ",char," "),expr.tmp,perl=T)
  #Split everything on spaces 
  comps<-helpRfuncs::vecsplit(expr.tmp,' ')
  #remove empty comoponents
  comps<-comps[which(comps!="")]
  #Get the classes of the components
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
    #remove the ignore functions
    for (func in ignore)
      expr.tmp<-gsub(func,"",expr.tmp,fixed=T)
    #Recalculate the separated components
    for (char in helpRfuncs::vecsplit(splitchar,""))
      expr.tmp<-gsub(paste0("\\",char),paste0(" "),expr.tmp,perl=T)
    #Split everything on spaces 
    comps<-helpRfuncs::vecsplit(expr.tmp,' ')
    comps<-comps[which(comps!="")]
    classes<-unlist(lapply(comps,FUN=function(X) class(try(eval(parse(text=X)),silent=T))))
    if (any(classes=='function')) { stop("function removal failed") } 
    comps<-c(combset,comps)
  }
  expr.tmp<-expr
  for (i in 1:length(comps)) { 
    expr.tmp<-gsub(comps[i],paste0(outvar,i),expr.tmp,fixed=T)
  }
  names(comps)<-paste0(outvar,1:length(comps))
  return=list(components=comps,replace.expr=expr.tmp)
}
