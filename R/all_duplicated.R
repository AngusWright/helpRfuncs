#
# Function that returns all duplicated entries, not just the duplicates 
#

duplicated<-function(X,from_type='all') {
  if (from_type == 'first') { 
    return = base::duplicated(X)
  } else if (from_type == 'last') {  
    return = base::duplicated(X,fromLast=TRUE)
  } else if (from_type == 'all') { 
    return = (base::duplicated(X)|base::duplicated(X,fromLast=TRUE))
  } else { 
    stop("Unknown duplicate selection from_type: must be first/last/all")
  }
}
