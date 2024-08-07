#
# Function for splitting a vector of strings
# Created by A.H.Wright (2018-10-22)
#

vecsplit<-function(X,by,n,recollapse=!missing(n),fixed=TRUE) {
  if (missing(n)) { 
    #Return all values {{{
    if (recollapse) { 
      return=unlist(lapply(X,function(Y) paste(strsplit(Y,by,fixed=fixed)[[1]],collapse=by)))
    } else { 
      return=unlist(lapply(X,function(Y) strsplit(Y,by,fixed=fixed)[[1]]))
    } 
    #}}}
  } else if(is.function(n)) { 
    #N is a function to evaluate {{{ 
    if (recollapse) { 
      return=unlist(lapply(X,function(Y) { 
                             vals<-strsplit(Y,by,fixed=fixed)[[1]]
                             index<-n(vals)
                             if (!is.numeric(index)) stop("index function does not evaluate to numeric!")
                             return=paste(vals[index],collapse=by)}))
    } else { 
      return=unlist(lapply(X,function(Y) { 
                             vals<-strsplit(Y,by,fixed=fixed)[[1]]
                             index<-n(vals)
                             if (!is.numeric(index)) stop("index function does not evaluate to numeric!")
                             return=vals[index]}))
    } 
    #}}}
  } else if(length(n) > 1) { 
    #If there is more than 1 n value {{{
    #Check for syntax {{{
    if (any(n<=0)) { 
      stop('multiple n values must all be positive')
    }
    #}}}
    if (recollapse) { 
      return=unlist(lapply(X,function(Y) paste(strsplit(Y,by,fixed=fixed)[[1]][n],collapse=by)))
    } else { 
      return=unlist(lapply(X,function(Y) strsplit(Y,by,fixed=fixed)[[1]][n]))
    } 
    #}}}
  } else if(n < 0) { 
    #If n is negative, select from the end {{{
    n<-abs(n)
    return=unlist(lapply(X,function(Y) rev(strsplit(Y,by,fixed=fixed)[[1]])[n]))
    #}}}
  } else if (n > 0) {  
    #If n is positive, select from the front {{{
    return=unlist(lapply(X,function(Y) strsplit(Y,by,fixed=fixed)[[1]][n]))
    #}}}
  } else if (n == 0) { 
    #If n==0, stop {{{
    stop("n must be non-zero or missing (i.e. return all)") 
    #}}}
  } 
}

