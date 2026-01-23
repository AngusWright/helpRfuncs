#=========================================
#
# File Name : sph2sph.R
# Created By : awright
# Creation Date : 16-12-2023
# Last Modified : Sat 16 Dec 2023 10:26:59 PM CET
#
#=========================================


sph2sph<-function(alpha,delta,alpha0=187,delta0=47) { 

  car<-celestial::sph2car(r=1,alpha,delta)
  sph<-celestial::car2sph(car)
  return=sph

}
