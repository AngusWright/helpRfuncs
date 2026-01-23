#=========================================
#
# File Name : eq2gal.R
# Created By : awright
# Creation Date : 16-12-2023
# Last Modified : Sat 16 Dec 2023 10:46:29 PM CET
#
#=========================================

#Convert equatorial coordinates to galactic coordinates 
eq2gal<-function(alpha,delta,equinox='J2000') { 
  if (equinox=='J2000') { 
    alpha0<-192.8595
    delta0<-27.1284
    delta0<-45
    ell0<-122.9320
  } else if (equinox=='B1950') { 
    alpha0<-192.25
    delta0<-27.40
    ell0<-123.000
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

