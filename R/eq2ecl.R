#=========================================
#
# File Name : eq2ecl.R
# Created By : awright
# Creation Date : 16-12-2023
# Last Modified : Sat 16 Dec 2023 11:00:58 PM CET
#
#=========================================

#Convert equatorial coordinates to ecliptic coordinates 
eq2ecl<-function(alpha,delta,equinox='J2000',plot=FALSE) { 
  if (equinox=='J2000') { 
    alpha0<-270.0  
    delta0<-90-23.439 
    ell0<-90.0+0.698
  } else if (equinox=='B1950') { 
    alpha0<-270.0  
    delta0<-90-23.446
    ell0<-90.0
  } else { 
    stop("unknown equinox provided: must be either J2000 or B1950") 
  }

  dcos<-function(theta) cos(theta*pi/180)
  dsin<-function(theta) sin(theta*pi/180)
  datan2<-function(f,x) atan2(f,x)*180/pi
  dasin<-function(f) asin(f)*180/pi

  alpha_prime=alpha-alpha0
  delta_prime=delta-delta0

  b = (dasin(dsin(delta)*dsin(delta0)+dcos(delta)*dcos(delta0)*dcos(alpha_prime)))

  calcy = (dcos(delta)*dsin(alpha_prime))
  calcx = (dsin(delta)*dcos(delta0)-dcos(delta)*dsin(delta0)*dcos(alpha_prime))

  ell = -datan2(calcy,calcx)+ell0

  ell[which(ell<0)]<-ell[which(ell<0)]+360
  ell[which(ell>360)]<-ell[which(ell>360)]-360

  return=cbind(ell,b)

}

