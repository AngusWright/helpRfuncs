#=========================================
#
# File Name :
# Created By : awright
# Creation Date : 17-01-2025
# Last Modified : Sat 18 Jan 2025 12:26:19 AM CET
#
#=========================================

weighted.quantile <- function(x, weights, probs, na.rm=FALSE) {

  if (length(x)==0) stop("no data provided!") 
  
  if (any(probs > 1) | any(probs < 0)) stop("probability should be in the interval [0,1]")

  #If no weights, use simple quantile 
  if(missing(weights)) {

    return = quantile(x, probs, na.rm = na.rm)

  } else { 

    #If desired, remove bad data 
    bad <- (!is.finite(weights) | !is.finite(x))
    if (na.rm==TRUE) { 
      weights <- weights[!bad]
      x <- x[!bad]
      if (length(x)==0) stop("no valid data provided! All removed by na.rm=TRUE!") 
    } else if (any(bad)) { 
      stop("there are non-finite elements in the weights or x values; must use na.rm=TRUE")
    }

    #Order the data 
    ind <- order(x)
    weights <- weights[ind]
    x <- x[ind]

    #Weighted CDF 
    wcdf <- cumsum(weights) / sum(weights)

    .one_quan <- function(x,weights,wcdf,probs) { 
      #Get the data point(s) that bracket the requested probability (and check for boundary cases)
      if (min(wcdf) >= probs) {
        warning("Requested probability is below the first wCDF data point; that's probably not good (or you requested probs=0).")
        lower <- 1
      } else {
        lower <- max(which(wcdf <= probs))
      }

      if (max(wcdf) <= probs) {
        warning("Requested probability is above the last wCDF data point; that's probably not good (or you requested probs=1).")
        upper <- length(wcdf)
      } else {
        upper <- min(which(wcdf >= probs))
      }
      
      if (upper == lower) { 
        return=x[upper]
      } else {
        return=(weights[lower] * x[lower] + weights[upper] * x[upper]) / (weights[lower] + weights[upper])
      }
    }

    val<-rep(NA,length(probs))
    for (p in 1:length(probs)) val[p]<-.one_quan(x,weights,wcdf,probs[p])
    names(val)<-probs

    return=val
  }
}
