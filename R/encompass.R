#
# Function returns the interval enclosing the provided data rounded to 'digits' decimal places
#
encompass<-function(X,digits=2) { 
  range<-range(X)
  range[1]<-floor(range[1]*10^digits)/10^digits
  range[2]<-floor(range[2]*10^digits)/10^digits
  return=range
}
