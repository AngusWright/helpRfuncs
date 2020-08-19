#
# Function strips hanging brackets from text expressions
#

split.expr<-function(expr,splitchar='+-=/*^~<>()&!|,',outvar="V",
                      matchchars=rbind(c("[[","]]")),ignore=c("abs","sqrt",'na.rm')) { 
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
  #Check for broken array subsets {{{ 
  combset<-NULL
  for (match.ind in 1:nrow(matchchars)) {  
    combset<-.find.match(matchchars[match.ind,],comps,returnAll=FALSE)
    comps<-.find.match(matchchars[match.ind,],comps)
  }
  #}}}
  #Get the classes of the components
  classes<-unlist(lapply(comps,FUN=function(X) class(try(eval(parse(text=X)),silent=T))))
  if (any(classes=='function')) { 
    close.brace<-0
    func.ind<-which(classes=="function")
    #Merge the functions with their trailing bracket
    for (i in rev(func.ind)) { 
      if (i==1 & length(comps)>=i+2) { 
        comps<-c(paste0(comps[i],comps[i+1]),comps[(i+2):length(comps)])
      } else if (i==1) { 
        comps<-paste0(comps[i],comps[i+1])
      } else if (length(comps)==i+1) { 
        comps<-c(comps[1:(i-1)],paste0(comps[i],comps[i+1]))
      } else {
        comps<-c(comps[1:(i-1)],paste0(comps[i],comps[i+1]),comps[(i+2):length(comps)])
      }
      classes<-classes[-(i+1)]
    }
    combset<-c(combset,.find.match(c("(",")"),comps,subset=which(classes=='function'),returnAll=FALSE,nestOrder=TRUE))
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
    if (any(comps=="")) {
      comps<-comps[which(comps!="")]
    }
    #Check for lone numbers
    classes<-unlist(lapply(comps,FUN=function(X) class(try(eval(parse(text=X)),silent=T))))
    if (any(classes=="logical")) { 
      #Some of the components are just numbers
      comps<-comps[which(classes!="logical")]
      classes<-unlist(lapply(comps,FUN=function(X) class(try(eval(parse(text=X)),silent=T))))
    }
    if (any(classes=="numeric")) { 
      #Some of the components are just numbers
      comps<-comps[which(classes!="numeric")]
      classes<-unlist(lapply(comps,FUN=function(X) class(try(eval(parse(text=X)),silent=T))))
    }
    #Remove any wrapping functions 
    if (any(classes=="function")) { 
      comps<-comps[which(classes!='function')]
      classes<-unlist(lapply(comps,FUN=function(X) class(try(eval(parse(text=X)),silent=T))))
    }
    comps<-c(combset,comps)
  }
  expr.tmp<-expr
  for (i in 1:length(comps)) { 
    expr.tmp<-gsub(comps[i],paste0(outvar,i),expr.tmp,fixed=T)
  }
  names(comps)<-paste0(outvar,1:length(comps))
  return=list(components=comps,replace.expr=expr.tmp)
}

.find.match<-function(matchchars,comps,subset,returnAll=TRUE,nestOrder=FALSE) {
  if (length(matchchars)>2)  
    stop("expecting two sets of characters to match") 
  match.open<-matchchars[1]
  match.close<-matchchars[2]
  #Count the number of brace sets per component
  close.brace<-open.brace<-rep(0,length(comps))
  tmp.comps<-comps
  while (any(grepl(match.open,tmp.comps,fixed=T)|grepl(match.close,tmp.comps,fixed=T))) {
    open.brace<-open.brace+ifelse(grepl(match.open,tmp.comps,fixed=T),1,0)
    close.brace<-close.brace+ifelse(grepl(match.close,tmp.comps,fixed=T),1,0)
    tmp.comps<-sub(match.open,'',tmp.comps,fixed=T)
    tmp.comps<-sub(match.close,'',tmp.comps,fixed=T)
  }
  if (sum(open.brace)!=sum(close.brace)) { 
    stop(paste0("There are mis-matched brackets ",match.open,"...",match.close,":\n",paste(comps,collapse=''))) 
  }
  if (any(open.brace!=close.brace)) { 
    open.brace<-cumsum(open.brace)
    close.brace<-cumsum(close.brace)
    new.comps<-NULL
    running.end<-tmp.close<-0
    if (missing(subset)) { 
      subset<-1:length(open.brace)
    }
    if (nestOrder) { 
      subset<-subset[order(decreasing=TRUE,(open.brace-close.brace)[subset])]
      returnAll<-FALSE
    }
    for (start in subset) {
      #If the last close.brace was after this function, skip it
      if (tmp.close>=start & !nestOrder) next
      if ((open.brace-close.brace)[start]==0) { 
        if (returnAll) { 
          new.comps<-c(new.comps,comps[start])
          running.end<-start
        }
        next
      }
      #combine functions into single components
      tmp.close<-start+which((open.brace-close.brace)[-(1:start)]==(open.brace-close.brace)[start]-1)[1]
      if (is.na(tmp.close)) stop("There is unmatched braces in expression!")
      join<-paste(comps[start:tmp.close],collapse="")
      if (running.end!=(start-1) && returnAll) {
        new.comps<-c(new.comps,comps[(running.end+1):(start-1)])
      }
      new.comps<-c(new.comps,join)
      running.end<-tmp.close
    }
    if (running.end<length(open.brace) && returnAll) {
      new.comps<-c(new.comps,comps[(running.end+1):length(open.brace)])
    }
    return=new.comps
  } else { 
    if (returnAll) { 
      return=comps
    } else { 
      return=NULL
    }
  }
}
