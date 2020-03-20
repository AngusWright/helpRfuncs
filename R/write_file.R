
#All-in-one function for writing output files /*fold*/{{{ 
write.file<-function(file,cat,quote=FALSE,row.names=FALSE,...) { 
  #Determine the desired file type and output it
  if (grepl('\\.fits',file,ignore.case=TRUE)){
    Rfits::Rfits_write_table(file=file,cat,...)
  } else if (grepl('\\.cat',file,ignore.case=TRUE)){
    warning("Cannot write to LDAC; writing out as FITS instead!") 
    file<-sub(".cat",".fits",file)
    Rfits::Rfits_write_table(file=file,cat,...)
  } else if (grepl('\\.asc',file,ignore.case=TRUE)){
    write.table(file=file,cat,quote=quote,row.names=row.names,...)
  } else if (grepl('\\.csv',file,ignore.case=TRUE)){
    write.csv(file=file,cat,quote=quote,row.names=row.names,...)
  } else if (grepl('\\.Rdata',file,ignore.case=TRUE)){
    save(file=file,cat,...)
  } else { 
    stop("Unknown extension (not fits/asc/csv/Rdata)")
  }
  return=NULL
}
#/*fend*/}}}
