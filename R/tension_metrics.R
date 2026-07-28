#=========================================
#
# File Name : tension_metrics.R
# Created By : awright
# Creation Date : 15-11-2024
# Last Modified : Tue Jul 28 11:41:38 2026
#
#=========================================


tension_metrics<-function(sample1,sample2,weight1,weight2,bw,kern='gauss',from,to,n=1e4,verbose=FALSE,tolerance=1e-5) { 
  #Check for supplied weight1 
  if (missing(weight1)) 
    weight1<-rep(1,length(sample1))
  #Check for supplied weight2
  if (missing(weight2)) 
    weight2<-rep(1,length(sample2))
  #Select valid samples and weights 
  ind1<-which(is.finite(weight1) & is.finite(sample1))
  ind2<-which(is.finite(weight2) & is.finite(sample2))
  #Check for supplied from
  if (missing(from)) 
    from<-min(sample1[ind1],sample2[ind2])
  #Check for supplied to
  if (missing(to)) 
    to<-max(sample1[ind1],sample2[ind2])
  #Check for supplied bw 
  if (missing(bw)) 
    bw<-bw.SJ(c(sample1[ind1],sample2[ind2]))
  #Compute sample means 
  mean1<-weighted.mean(sample1[ind1],w=weight1[ind1]/sum(weight1[ind1]))
  mean2<-weighted.mean(sample2[ind2],w=weight2[ind2]/sum(weight2[ind2]))
  #Compute sample sdevs
  sdev1<-weighted.sd(sample1[ind1],wt=weight1[ind1]/sum(weight1[ind1]))
  sdev2<-weighted.sd(sample2[ind2],wt=weight2[ind2]/sum(weight2[ind2]))
  #Compute sample PDFs
  dens1<-density(sample1[ind1],weight=weight1[ind1]/sum(weight1[ind1]),kern=kern,from=from,to=to,n=n,bw=bw)
  df1<-approxfun(dens1$x,dens1$y,rule=1)
  dens2<-density(sample2[ind2],weight=weight2[ind2]/sum(weight2[ind2]),kern=kern,from=from,to=to,n=n,bw=bw)
  df2<-approxfun(dens2$x,dens2$y,rule=1)
  
  #Hellinger distance for these pdfs: 0.5 * int(dx [ sqrt(p(x)) - sqrt(q(x)) ]^2)
  steps<-seq(from,to,len=n)
  sum_px<-sum(df1(steps))
  sum_qx<-sum(df2(steps))
  #hell_dist<-0.5*sum((sqrt(df1(steps)*diff(steps)[1])-sqrt(df2(steps)*diff(steps)[1]))^2,na.rm=T)
  hell_dist<-dist(rbind(sqrt(df1(steps)/sum_px),sqrt(df2(steps)/sum_qx)))/sqrt(2)
  #Hellinger distance under gaussian approximation
  hell_dist_gauss <- sqrt(1 - sqrt((2*sdev1*sdev2)/(sdev1^2+sdev2^2)) * exp(-(mean1-mean2)^2/(4*(sdev1^2+sdev2^2))))
  #Implied mean deviation of these PDFs under gaussianity
  meandiff <- sqrt(-log((1 - hell_dist^2)/sqrt((2*sdev1*sdev2)/(sdev1^2+sdev2^2)))*(4*(sdev1^2+sdev2^2)))
  meandiff_gauss <- sqrt(-log((1 - hell_dist_gauss^2)/sqrt((2*sdev1*sdev2)/(sdev1^2+sdev2^2)))*(4*(sdev1^2+sdev2^2)))

  #Tension measures: 
  gauss_tension <- abs(mean1 - mean2) / sqrt(sdev1^2+sdev2^2)
  hell_tension  <- meandiff / sqrt(sdev1^2+sdev2^2)
  hell_gauss_tension  <- meandiff_gauss / sqrt(sdev1^2+sdev2^2)
  diff_wrt_one <- abs(mean1 - mean2) / sdev1
  diff_wrt_two <- abs(mean1 - mean2) / sdev2
  diff_wrt_max <- abs(mean1 - mean2) / max(c(sdev1,sdev2))
  interval_one <- HDInterval::hdi(dens1,credMass=diff(pnorm(c(-1,1))),allowSplit=FALSE)
  interval_two <- HDInterval::hdi(dens2,credMass=diff(pnorm(c(-1,1))),allowSplit=FALSE)
  heymans_metric <- abs(mean(interval_one) - mean(interval_two)) / max(abs(diff(interval_one)),abs(diff(interval_two)))

  out=c(diff_wrt_one=diff_wrt_one,diff_wrt_two=diff_wrt_two,diff_wrt_max=diff_wrt_max,heymans_metric=heymans_metric,
        gaussian=gauss_tension,hellinger_gauss=hell_gauss_tension,hellinger=hell_tension)
  if (verbose) out=c(sum_px=sum(df1(steps)*diff(steps)[1]),
                     sum_qx=sum(df2(steps)*diff(steps)[1]),
                     mean1=mean1,mean2=mean2,
                     sdev1=sdev1,sdev2=sdev2,
                     hell_dist=hell_dist,
                     hell_dist_gauss=hell_dist_gauss,
                     meandiff=meandiff,
                     meandiff_gauss=meandiff_gauss,
                     out)
  return(out)

}

#Weighted stdev function {{{
weighted.sd<-function(x,wt,...) {
    return=sqrt(weighted.var(x,wt,...))
}
#}}}

#Weighted var function {{{
weighted.var<-function(x,wt,...) {
    xm<-weighted.mean(x,wt,...)
  v<-weighted.mean((x-xm)^2, wt,...)
    return=v
}
#}}}




  
