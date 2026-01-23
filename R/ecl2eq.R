#=========================================
#
# File Name : eq2ecl.R
# Created By : awright
# Creation Date : 16-12-2023
# Last Modified : Sat 16 Dec 2023 01:55:52 PM CET
#
#=========================================

#Convert ecliptic coordinates to equatorial coordinates 
ecl2eq<-function(lambda,beta,equinox='J2000') { 
  if (equinox=='J2000') { 
    eps0<-23.439
    lambda<-lambda-0.698
  } else if (equinox=='B1950') { 
    eps0<-23.446
  } else { 
    stop("unknown equinox provided: must be either J2000 or B1950") 
  }

  dcos<-function(theta) cos(theta*pi/180)
  dsin<-function(theta) sin(theta*pi/180)
  datan<-function(f) atan(f)*180/pi
  dasin<-function(f) asin(f)*180/pi
  dacos<-function(f) acos(f)*180/pi

  delta = dasin(dsin(beta)*dcos(eps0)+dcos(beta)*dsin(eps0)*dsin(lambda))
  alpha = rep(NA,length(lambda))
  #lambda <= 180
  alpha = dacos((dcos(lambda)*dcos(beta))/(dcos(delta)))
  ###lambda > 180
  ##ind<-which(lambda>180)
  #ind<-1:length(lambda)
  #alpha[ind] = dasin((-dsin(beta[ind])*dsin(eps0)+dcos(beta[ind])*dcos(eps0)*dsin(lambda[ind]))/(dcos(delta[ind])))

  plot(beta,delta,type='p',pch='.',xlim=c(-1,1)*360,ylim=c(-1,1)*180)
  points(lambda,alpha,type='p',pch='.',col='red')

  return=cbind(alpha,delta)

}

