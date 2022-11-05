
smooth.im<-function(im,filter.sd.pix,normalise=TRUE) { 
  require(LAMBDAR)
  if (length(filter.sd.pix)==1) { 
    filter.sd.pix<-rep(filter.sd.pix,2)
  }
  psf.x = matrix(1:ncol(im), nrow = nrow(im), ncol = ncol(im),byrow=T)
  psf.y = matrix(1:nrow(im), nrow = nrow(im), ncol = ncol(im))
  psf <- exp(-(((psf.x - ncol(im)/2)^2/(2 * filter.sd.pix[1]^2)) + ((psf.y - 
      nrow(im)/2)^2/(2 * filter.sd.pix[2]^2))))

  conv<-convolve.psf(psf,im)

  if (normalise) { 
    conv<-conv/sum(psf)
  }
  return=conv
}

smooth.im.par<-function(im,filter.sd.pix,normalise=TRUE,n=c(1,1)) { 

  if (length(filter.sd.pix)==1) { 
    filter.sd.pix<-rep(filter.sd.pix,2)
  }

  registerDoParallel(cores=prod(n))
  #split the image into n.cores*2-ish chunks with overlap
  dim<-dim(im)
  xstep<-round(seq(1,dim[1],length=n[1]+1))
  ystep<-round(seq(1,dim[2],length=n[2]+1))
  nchunkx<-diff(xstep)+1
  nchunky<-diff(ystep)+1

  xmin<-xstep[1:n[1]]
  xmax<-xstep[-1]

  ymin<-ystep[1:n[1]]
  ymax<-ystep[-1]

  min.buffer.x<-c(0,rep(round(20*filter.sd.pix[1]),n[1]-1))
  max.buffer.x<-c(rep(round(20*filter.sd.pix[1]),n[1]-1),0)
  min.buffer.y<-c(0,rep(round(20*filter.sd.pix[2]),n[2]-1))
  max.buffer.y<-c(rep(round(20*filter.sd.pix[2]),n[2]-1),0)
  xmin<-xmin-min.buffer.x
  xmax<-xmax+max.buffer.x
  if (any(diff(xmin)<0)) { stop("image is too small to be used in parallel") }
  if (any(diff(ymin)<0)) { stop("image is too small to be used in parallel") }

  ymin<-ymin-min.buffer.y
  ymax<-ymax+max.buffer.y

  ims<-foreach(i=1:n[1])%:% 
    foreach(j=1:n[2]) %dopar% { 
      tmpim<-im[xmin[i]:xmax[i],ymin[j]:ymax[j]]
      tmpim[1:(8*filter.sd.pix),]<-0
      tmpim[,1:(8*filter.sd.pix)]<-0
      tmpim[nrow(tmpim)-1:(10*filter.sd.pix)+1,]<-0
      tmpim[,ncol(tmpim)-1:(10*filter.sd.pix)+1]<-0
      smooth.im(tmpim,filter.sd.pix,normalise)
  }
  smth.im<-im
  for (i in 1:n[1]) { 
    for (j in 1:n[2]) { 
      subx.min<-xmin[i]+min.buffer.x[i]
      subx.max<-xmax[i]-max.buffer.x[i]
      suby.min<-ymin[j]+min.buffer.y[j]
      suby.max<-ymax[j]-max.buffer.y[j]
      smth.im[subx.min:subx.max,suby.min:suby.max]<-ims[[i]][[j]][1:nchunkx[i]+min.buffer.x[i],1:nchunky[j]+min.buffer.y[j]]
    }
  }
  return=smth.im
}

