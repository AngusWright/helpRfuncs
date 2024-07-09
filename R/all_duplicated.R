#
# Function that returns all duplicated entries, not just the duplicates 
#

duplicated<-function(X,from_type='all') {
  if (from_type == 'first') { 
    return = duplicated(X)
  } else if (from_type == 'last') {  
    return = duplicated(X,fromLast=TRUE)
  } else if (from_type == 'all') { 
    return = (duplicated(X)|duplicated(X,fromLast=TRUE))
  } else { 
    stop("Unknown duplicate selection from_type: must be first/last/all")
  }
}
