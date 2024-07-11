#=========================================
#
# File Name : ellipticities.R
# Created By : awright
# Creation Date : 04-07-2024
# Last Modified : Thu 04 Jul 2024 11:51:47 PM CEST
#
#=========================================

draw.ellipses<-function(x0,y0,e1,e2,lineLength=0.05,add=TRUE,...) {
  #If needed draw the base plot 
  if (!add) { 
    #Draw the base plot 
    magicaxis::magplot(x0,y0,pch=NA,...)
  }
  #Get the stick angles from the ellipticity components 
  angles<-atan2(y=e2,x=e1)/2
  #Get the stick magnitudes from the axis ratio 
  axisRatio<-(sqrt(e1^2+e2^2))
  #Draw the ellipses 
  plotrix::draw.ellipse(x0, y0, a=lineLength,b=lineLength*(1-axisRatio),
                        angle=angles,deg=FALSE,...)
  #Return nothing 
  return=NULL
}

draw.sticks<-function(x0,y0,e1,e2,lineLength=0.05,add=TRUE,...) {
  #If needed draw the base plot 
  if (!add) { 
    #Draw the base plot 
    magicaxis::magplot(x0,y0,pch=NA,...)
  }
  #Get the stick angles from the ellipticity components 
  angles<-atan2(y=e2,x=e1)/2
  #Get the stick magnitudes from the axis ratio 
  axisRatio<-(sqrt(e1^2+e2^2))
  #Construct the line segments, centered on x0,y0
  x1 <- x0 + lineLength * axisRatio * cos(angles) / 2
  y1 <- y0 + lineLength * axisRatio * sin(angles) / 2
  x0 <- x0 - lineLength * axisRatio * cos(angles) / 2
  y0 <- y0 - lineLength * axisRatio * sin(angles) / 2
  #Draw the segments
  segments(x0, y0, x1, y1,...)
  #Return nothing 
  return=NULL
}
