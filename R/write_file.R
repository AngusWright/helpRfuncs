
#All-in-one function for writing output files /*fold*/{{{ 
write.file<-function(file,cat,quote=FALSE,row.names=FALSE,col.names=TRUE,...) { 
  #Determine the desired file type and output it
  if (grepl('\\.fits',file,ignore.case=TRUE)){
    #Check for factors /*fold*/ {{{
    if (any(unlist(lapply(cat,class))=='factor')) { 
      for (col in which(unlist(lapply(cat,class))=='factor')) { 
        cat[[col]]<-levels(cat[[col]])[cat[[col]]]
      }
    }
    #/*fend*/}}}
    if ("Rfits" %in% rownames(installed.packages())) { 
      Rfits::Rfits_write_table(file=file,cat,...)
    } else if ("astro" %in% rownames(installed.packages())) {
      astro::write.fits(file=file,cat,...)
    } else { 
      stop("There is no FITS package installed (Rfits or FITSio)")
    }
  } else if (grepl('\\.cat',file,ignore.case=TRUE)){
    warning("Cannot write to LDAC; writing out as FITS instead!") 
    file<-sub(".cat",".fits",file)
    #Check for factors /*fold*/ {{{
    if (any(unlist(lapply(cat,class))=='factor')) { 
      for (col in which(unlist(lapply(cat,class))=='factor')) { 
        cat[[col]]<-levels(cat[[col]])[cat[[col]]]
      }
    }
    #/*fend*/}}}
    if ("Rfits" %in% rownames(installed.packages())) { 
      Rfits::Rfits_write_table(file=file,cat,...)
    } else if ("astro" %in% rownames(installed.packages())) {
      astro::write.fits(file=file,cat,...)
    } else { 
      stop("There is no FITS package installed (Rfits or FITSio)")
    }
  } else if (grepl('\\.asc',file,ignore.case=TRUE)){
    write.table(file=file,cat,quote=quote,row.names=row.names,col.names=col.names,...)
  } else if (grepl('\\.csv',file,ignore.case=TRUE)){
    write.csv(file=file,cat,quote=quote,row.names=row.names,col.names=col.names,...)
  } else if (grepl('\\.Rdata',file,ignore.case=TRUE)){
    save(file=file,cat,...)
  } else { 
    stop("Unknown extension (not fits/asc/csv/Rdata)")
  }
  return=NULL
}
#/*fend*/}}}
