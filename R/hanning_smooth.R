
hanning.smooth <-
function(arr1,degree=3) {
#Perform Hanning smoothing on 2D data

  #Determine degree {{{
  degree<-abs(degree)
  #}}}
  #If degree is valid, smooth {{{
  if (degree%%2!=1) {
    #Degree is even -> Error {{{
    stop("Hanning degree must be odd")
    #}}}
  } else if (degree > 1) {
    #Initialise {{{
    fact<-1
    cumul<-1
    sm.arr1<-arr1
    #}}}
    #Perform Unnormalised Smoothing over data {{{
    for (i in 2:ceiling(degree/2)) {
      fact<-0.5+0.5*(cos((pi*abs(i))/ceiling(degree/2)))
      cumul<-cumul+fact
      index<-1-i
      sm.arr1<-sm.arr1+fact*elementshift(arr1,index)
      sm.arr1<-sm.arr1+fact*elementshift(arr1,-index)
    }#}}}
    #Normalise and return {{{
    return=sm.arr1/cumul
    #}}}
  } else {
    #Smoothing does nothing: Return {{{
    return=arr1
    #}}}
  }
  #}}}
}
