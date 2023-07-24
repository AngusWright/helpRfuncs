#=========================================
#
# File Name : plot.chain.R
# Created By : awright
# Creation Date : 24-07-2023
# Last Modified : Mon 24 Jul 2023 11:35:10 AM CEST
#
#=========================================

plot.chain<-function(chain,colkeep="COSMOL|weight",coldel="COSMOMC",labelrm="COSMOLOGICAL_PARAMETERS--",...) { 
  chain<-chain[,grepl(colkeep,colnames(chain))&!grepl(coldel,colnames(chain)),with=F]
  colnames(chain)<-gsub(labelrm,"",colnames(chain))
  res=helpRfuncs::triplot(chain,...)
  return=res
}
