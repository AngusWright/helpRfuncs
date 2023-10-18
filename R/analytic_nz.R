#=========================================
#
# File Name :
# Created By : awright
# Creation Date : 18-10-2023
# Last Modified : Wed 18 Oct 2023 08:36:24 AM UTC
#
#=========================================

#The magnitude-limited Nz distribution /*fold*/ {{{
analytic_nz<-function(filter,maglim,safe=TRUE) {
  #Coefficients from fitting to SURFS+Shark lightcone
  if (filter=='u'){
    amp  <-c( 44106.97570, -8561.65951 ,  623.7589957, -20.31245697,  0.2512547888)
    slope<-c(   288.09721,   -43.84690 ,    2.5104880,  -0.06386803,  0.0006083717)
  } else if (filter=='g') {
    amp  <-c( 37034.83826, -6751.01270 ,  458.3057409, -13.86063982,  0.1607176833)
    slope<-c(   366.65872,   -62.24724 ,    3.9881168,  -0.11386033,  0.0012203258)
  } else if (filter=='r') {
    amp  <-c(-138832.81513, 26770.76527, -1924.5111224 , 60.94549390, -0.7132703806)
    slope<-c(    274.09568,   -46.85320,     3.0165561 , -0.08634719,  0.0009260457)
  } else if (filter=='i') {
    amp  <-c(-313487.40512, 59435.82041, -4201.7171511, 131.06049530, -1.5174070269)
    slope<-c(    135.26036,   -22.18581,     1.3763554,  -0.03801668,  0.0003938221)
  } else if (filter=='Z') {
    amp  <-c(-402537.94475, 76055.74692 ,-5359.1043631, 166.71312498, -1.9273789539)
    slope<-c(     95.96844,   -15.36934 ,    0.9333136,  -0.02524991,  0.0002562967)
  } else { 
    stop(paste("Uknown filter:",filter))
  }

  #Estimate the Nz for this sample with these parameters 
  polyn<-function(x,c) return=c[1]+c[2]*x+c[3]*x^2+c[4]*x^3+c[5]*x^4
  z<-seq(0,7,by=0.001)
  #Check that the requested magnitude limits are within the modelled range
  if (maglim>17 | !safe) { 
    if (maglim>27) { 
      warning("Magnitude limit provided is outside modelled limits [18,26]")
      if (safe) { 
        cat("Forcing magnitude limit to 27 to minimise extrapolation...") 
        maglim<-27
      }
    }
    #Get the amplitude value 
    amp0<-polyn(maglim,amp)
    #Get the slope value 
    slope0<-polyn(maglim,slope)
    #Estimate the Nz
    nz<-approxfun(x=z,y=amp0*z^2*exp(-(z/0.1)^slope0))
  } else if (maglim<=17) { 
    #Set the Nz to zero 
    warning("Magnitude limit provided is brighter than the modelled limits [18,26]. Setting Nz to zero!")
    nz<-approxfun(x=z,y=rep(0,length(z)))
  }
  return=nz
}

