#=========================================
#
# File Name : maglim_weight.R
# Created By : awright
# Creation Date : 17-10-2023
# Last Modified : Tue 17 Oct 2023 11:53:31 AM UTC
#
#=========================================


#Compute Prior Volume weights function /*fold*/ {{{
maglim_weight<-function(zspec,mag.lim=23.5,filter='r',ref=737) { 
  #z limits 
  z.limits<-seq(0,max(zspec),by=0.1)
  #Get the analytic Nz
  nz_theory<-analytic_nz(z.limits,mag.lim,filter,ref)
  #Convert to a PDF 
  nz_theory$N<-nz_theory$N/sum(nz_theory$N)
  nz_theory<-approxfun(nz_theory$zmid,nz_theory$N)
  #Get the observed Nz 
  nz_obs<-density(zspec,bw=0.1/sqrt(12),kern='rect',from=0,to=max(zspec))
  nz_obs<-approxfun(x=nz_obs$mids,y=nz_obs$density)
  #Get the weights 
  maglim_weight<-nz_theory(zspec)/nz_obs(zspec)
  #Return the weights 
  return=maglim_weight
}

analytic_nz<-function(z.limits,mag.lim=23.5,filter='r',ref=737,area=180) { 
  #Define the magnitude Schechter function 
  MF2=function(M,phi,Ms,a){0.4*log(10)*phi*exp(-10^(0.4*(Ms-M)))*10^(0.4*(a+1)*(Ms-M))}
  #Define the Luminosity function 
  LumFunc<-function(z,Mabs,dist.mod) {
    phistar<-function(z) return=phistar0*10^(0.4*P*z)
    mstar<-function(z) return=mstar0-Q*(z-z0)
    alpha<-function(z) return=alpha0
    return=data.table::data.table(m=Mabs+dist.mod,N=MF2(Mabs,Ms=mstar(z),a=alpha(z),phi=phistar(z)))
  }
  #Define the parameters values {{{
  if (filter=='u') { 
    z0=0.1
    alpha0<- -1.10 
    mstar0<- -17.98
    phistar0<- 1.96*1E-2
    Q<- 6.2 
    P<- -8.5
  } else if (filter=='g') { 
    z0=0.1
    alpha0<- -1.10 
    mstar0<- -19.58
    Q<- 2.9 
    P<- -1.5
    phistar0<- 1.80*1E-2
  } else if (filter=='r') { 
    z0=0.1
    alpha0<- -1.23 
    mstar0<- -20.70
    Q<- 0.7 
    P<-  1.8
    phistar0<- 0.94*1E-2
  } else if (filter=='i') { 
    z0=0.1
    alpha0<- -1.12 
    mstar0<- -20.97
    Q<- 1.5 
    P<- 0.0
    phistar0<- 1.16*1E-2
  } else if (filter=='Z') { 
    z0=0.1
    alpha0<- -1.07 
    mstar0<- -20.97
    Q<- 1.5 
    P<- -0.5
    phistar0<- 1.26*1E-2
  } else { 
    stop(paste("Unkown filter",filter))
  }
  #}}}
  #Initialise the Nz array 
  Nz<-data.frame(zmid=(z.limits[-length(z.limits)]+z.limits[-1])/2,N=0)
  #For each slice of redshift 
  for (i in 1:(length(z.limits)-1)) { 
    #Compute the volume of this slice of redshift 
    slice.vol<-celestial::cosvol(zmin=z.limits[i],zmax=z.limits[i+1],area=area,ref=ref)['voltot']*1E6
    #Calculate the distance modulus for this redshift slice 
    dist.mod<-celestial::cosdistDistMod(z=(z.limits[i]+z.limits[i+1])/2,ref=ref)
    #Compute the number counts 
    counts<-LumFunc(z=(z.limits[i]+z.limits[i+1])/2,Mabs=seq(-25,-15,by=0.01),dist.mod=dist.mod) 
    #Truncate to the magnitude limit 
    counts<-counts[m<=mag.lim,]
    if (i==1) { 
      magicaxis::magplot(counts,type='l',log='y',ylim=c(1e-3,1e-1),xlim=c(14,mag.lim))
    } else { 
      lines(counts)
    }
    #multiply by the volume 
    counts$N<-counts$N*slice.vol
    #Add to the Nz 
    Nz$N[i]<-sum(counts$N)
    if (length(counts$N)==0) { break } 
  }
  return=Nz
}
#/*fend*/}}}
