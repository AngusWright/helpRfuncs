.overlaygrid<-function(ralim,declim,zlim,H0=100,OmegaM=0.25,OmegaL=0.75, text=c(1,2,3),textsize = 0.5, textangle=c(0,0),
                       labels = F, ramean=0, ramult=1,main='',mainloc='bottom',main.inset=0.1,trace=FALSE,buff.percent=0.05,titleshift=0.15,
                       ralab='RA (deg)',zlab=expression(italic(z)),orient='v'){
  
  require(celestial)

  if (length(buff.percent)==1) { 
    buff.percent.ra<-buff.percent.z<-buff.percent
  } else { 
    buff.percent.ra<-buff.percent[1]
    buff.percent.z<-buff.percent[2]
  } 

  #Define the text buffers 
  if (orient=='v') { 
    #Define the RA rotation
    rotation<-90
  }else if (orient=='d') { 
    #Define the RA rotation
    rotation<--90
  } else { 
    #Define the RA rotation
    rotation<-0
  } 

  #Get prety steps in true RA
  rasteps = pretty((ralim-rotation)/ramult+ramean,n=3)
  #Reproject into plot RA
  rasteps = (rasteps-ramean)*ramult+rotation
  #Get pretty steps in z 
  zsteps = pretty(zlim,n=3)

  rasteps<-rev(c(min(ralim),rasteps[which(rasteps>min(ralim) & rasteps<max(ralim))],max(ralim)))
  zsteps<-c(min(zlim),zsteps[which(zsteps>min(zlim) & zsteps<max(zlim))],max(zlim))

  toplines = celestial::sph2car(rasteps,mean(declim),celestial::cosdistCoDist(z=max(zlim),H0=H0,OmegaM=OmegaM))
  bottomlines = celestial::sph2car(rasteps,mean(declim),celestial::cosdistCoDist(z=min(zlim),H0=H0,OmegaM=OmegaM))
  topright = celestial::sph2car(max(ralim),mean(declim),celestial::cosdistCoDist(z=max(zlim),H0=H0,OmegaM=OmegaM))
  topleft = celestial::sph2car(min(ralim),mean(declim),celestial::cosdistCoDist(z=max(zlim),H0=H0,OmegaM=OmegaM))
  bottomright = celestial::sph2car(max(ralim),mean(declim),celestial::cosdistCoDist(z=min(zlim),H0=H0,OmegaM=OmegaM))
  bottomleft = celestial::sph2car(min(ralim),mean(declim),celestial::cosdistCoDist(z=min(zlim),H0=H0,OmegaM=OmegaM))

  #Define the text buffers 
  if (orient=='v') { 
    #Define horizontal buffers for RA labels
    xbuff<-0
    ybuff<-diff(range(par('usr')[3:4]))*buff.percent.ra
    pos<-3
  }else if (orient=='d') { 
    #Define horizontal buffers for RA labels
    xbuff<-0
    ybuff<--diff(range(par('usr')[3:4]))*buff.percent.ra
    pos<-1
  } else { 
    #Define vertical buffers for RA labels
    xbuff<-diff(range(par('usr')[1:2]))*buff.percent.ra
    ybuff<-0
    pos<-4
  } 

  for(i in 1:length(rasteps)){
    lines(x=c(toplines[i,1],bottomlines[i,1]),
          y=c(toplines[i,2],bottomlines[i,2]),
          col=hsv(h=0,s=0,v=0.75,alpha=0.75),lwd=0.75)
    if(any(text==1) & labels){
      if (trace) { 
        points(toplines[i,1]+xbuff,toplines[i,2]+ybuff,cex=2,col='purple',pch=0)
      }
      if (i!=1 & i!=length(rasteps)) {
        text(toplines[i,1]+xbuff,toplines[i,2]+ybuff,(rasteps[i]-rotation)/ramult+ramean,
            cex=textsize, srt=textangle[1],pos=pos)
      }
    }
  }
  lines(x=c(topright[1],bottomright[1]),y=c(topright[2],bottomright[2]))
  lines(x=c(topleft[1],bottomleft[1]),y=c(topleft[2],bottomleft[2]))

  toparc = celestial::sph2car(seq(min(ralim),max(ralim),len=1e3),mean(declim),celestial::cosdistCoDist(z=max(zlim),H0=H0,OmegaM=OmegaM))
  bottomarc = celestial::sph2car(seq(min(ralim),max(ralim),len=1e3),mean(declim),celestial::cosdistCoDist(z=min(zlim),H0=H0,OmegaM=OmegaM))

  #Define the text buffers 
  if (orient=='v') { 
    #Define horizontal buffers for z labels
    xbuff<-diff(range(par('usr')[1:2]))*buff.percent.z
    ybuff<-0
    titleshift.x<-0
    titleshift.y<-diff(range(par('usr')[3:4]))*titleshift
    pos<-4
    headtail<-head
  }else if (orient=='d') { 
    #Define horizontal buffers for z labels
    xbuff<--diff(range(par('usr')[1:2]))*buff.percent.z
    ybuff<-0
    titleshift.x<-0
    titleshift.y<-diff(range(par('usr')[3:4]))*titleshift
    pos<-4
    headtail<-tail
  } else { 
    #Define vertical buffers for z labels
    xbuff<-0
    ybuff<-diff(range(par('usr')[3:4]))*buff.percent.z
    titleshift.x<-diff(range(par('usr')[1:2]))*titleshift
    titleshift.y<-0
    pos<-3
    headtail<-head
  } 

  for(i in 1:length(zsteps)){
    arc = celestial::sph2car(seq(min(ralim),max(ralim),len=1e3),mean(declim),celestial::cosdistCoDist(z=zsteps[i],H0=H0,OmegaM=OmegaM))
    lines(x=arc[,1],y=arc[,2],col=hsv(h=0,s=0,v=0.75,alpha=0.75),lwd=0.75)
    if(any(text==2) & labels){
      if (trace) { 
        points(headtail(arc[,1],1)+xbuff,tail(arc[,2],1)+ybuff,cex=2, col='purple',pch=0)
      }
      if (i!=1 & i!=length(zsteps)) {
        text(headtail(arc[,1],1)+xbuff,tail(arc[,2],1)+ybuff,zsteps[i],cex=textsize, srt=textangle[2],pos=pos)
      }
    }
  }

  lines(x=toparc[,1],y=toparc[,2])
  lines(x=bottomarc[,1],y=bottomarc[,2])

  if(any(text==3) & labels){
    if (trace) { 
      points(x=(par('usr')[1]+par('usr')[2])/2+titleshift.x,y=par('usr')[3]+titleshift.y,pch=0,col='purple',cex=2)
    }
    text(x=(par('usr')[1]+par('usr')[2])/2+titleshift.x,y=par('usr')[3]+titleshift.y,label=paste(min(zlim), " < z < ", max(zlim), "\n",min(declim), " < Dec < ", max(declim),sep=""),cex=textsize, srt=textangle[2])
  }

  if(labels){
    if (orient=='h') { 
      text(x=mean(par('usr')[1:2]), y=par('usr')[4]-titleshift.y, label=zlab,cex=textsize*1.05)
      text(x=par('usr')[2]-titleshift.x, y=mean(par('usr')[3:4]), label=ralab,cex=textsize*1.05)
    } else { 
      text(x=mean(par('usr')[1:2]), y=par('usr')[4]-titleshift.y, label=ralab,cex=textsize*1.05)
      text(x=par('usr')[2]-titleshift.x, y=mean(par('usr')[3:4]), label=zlab,cex=textsize*1.05)
    }
  }
  text(helpRfuncs::text.coord(mainloc,inset=main.inset),labels=main,font=2)

}

plotCone <- function (ra, dec, z, OmegaM=0.25, OmegaL=0.75, H0=100,add=FALSE,col='black',asp=1,limits=NULL,ramult=1,main='',orient='v',
                      side=1:3,trace=FALSE,textsize=1,labels=TRUE,buffer=0.1,mainloc='bottom',main.inset=0.1,
                      titleshift=0.2,ralab='RA (deg)',zlab=expression(italic(z)),textangle=c(0,0),...) {

  #Define the plot orientation
  if (orient=='v') { 
    plot_rotation<- 90
  } else if (orient=='h') { 
    plot_rotation<- 0
  } else if (orient=='d') { 
    plot_rotation<- -90
  } else { 
    stop(paste("unknown orientation",orient))
  }

  #Cut data to provided limits 
  if (!is.null(limits)) {
    index<-which(ra>min(limits$ralim) & ra<max(limits$ralim) & 
                 z>min(limits$zlim) & z<max(limits$zlim))
    ra<-ra[index]
    dec<-dec[index]
    z<-z[index]
  }

  if (!is.null(limits)) {
    #Incorporate provided limits if needed
    ra_use<-(ra-mean(limits$ralim))*ramult+plot_rotation
    ramean<-mean(limits$ralim)
    ralims_use<-(limits$ralim-ramean)*ramult+plot_rotation
    plotgrid<-expand.grid(ralims_use,limits$declim,limits$zlim)
    z2d<-celestial::cosmapfunc("z","CoDist", OmegaM=OmegaM, OmegaL=OmegaL, H0=H0,zrange=limits$zlim)
    plotlim<-celestial::sph2car(plotgrid[,1],plotgrid[,2],z2d(plotgrid[,3]))
    vals<-celestial::sph2car(ra_use,dec,z2d(z))
  } else {
    #Rescale data for desired orientation 
    ramean<-mean(ra)
    ra_use<-(ra-ramean)*ramult+plot_rotation
    z2d<-celestial::cosmapfunc("z","CoDist", OmegaM=OmegaM, OmegaL=OmegaL, H0=H0,zrange=range(z))
    vals<-celestial::sph2car(ra_use,dec,z2d(z))
    plotlim<-cbind(range(vals[,1]),range(vals[,2]))
  }
  #If needed, draw the plot 
  if (!add) { plot(vals[,1],vals[,2],asp=asp,axes=trace,col='white', xlab='',ylab='',xlim=c(range(plotlim[,1])),...) }

  #Add the data 
  points(vals[,1],vals[,2],col=col,...)

  #Overlay the plot grid 
  if (add) {
    #if adding, do not annotate 
    side<-c(0,0,0)
    textsize<-0
  }
  if (!is.null(limits)) {
    #use limits if provided 
    .overlaygrid(ralim=ralims_use,declim=limits$declim,zlim=limits$zlim,
                orient=orient,text=side,textsize=textsize,labels=labels,textangle=textangle,
                buff.percent=buffer,titleshift=titleshift,mainloc=mainloc,main.inset=main.inset,
                ralab=ralab,zlab=zlab,
                OmegaM=OmegaM, OmegaL=OmegaL, H0=H0,
                ramean=ramean,ramult=ramult,trace=trace,main=main)
    return=data.frame(ralim=range(ralims),declim=limits$declim,zlim=limits$zlim)
  } else {
    #Use data limits 
    if (add) { warning("Adding without specified plot limits! Plot will likely be misaligned") }
    .overlaygrid(ralim=range(ra_use),
                declim=range(dec),
                zlim=c(min(z)-0.01,max(z)+0.01),
                orient=orient,
                buff.percent=buffer,titleshift=titleshift,mainloc=mainloc,main.inset=main.inset,
                text=side,textsize=textsize,labels=labels,textangle=textangle,
                ralab=ralab,zlab=zlab,
                OmegaM=OmegaM, OmegaL=OmegaL, H0=H0,
                ramean=ramean,ramult=ramult,main=main,trace=trace)
    return=data.frame(ralim=range(ra_use),declim=range(dec),zlim=c(min(z)-0.01,max(z)+0.01))
  }
}
