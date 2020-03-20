
#All-in-one function for reading input files /*fold*/{{{ 
read.file<-function(file,...) { 
  #Check for file 
  if (!file.exists(file)) { 
    stop("File ",file," does not exist!\n")
  }
  #Determine the desired file type and output it
  if (grepl('\\.fits',file,ignore.case=TRUE)){
    cat<-Rfits::Rfits_read_table(file=file,...)
  } else if (grepl('\\.cat',file,ignore.case=TRUE)){
    cat<-Rfits::Rfits_read_table(file=file,ext=3,...)
  } else if (grepl('\\.asc',file,ignore.case=TRUE)){
    cat<-data.table::fread(file=file,...)
  } else if (grepl('\\.csv',file,ignore.case=TRUE)){
    cat<-data.table::fread(file=file,...)
  } else if (grepl('\\.Rdata',file,ignore.case=TRUE)){
    nam<-load(file=file,...)
    if (nam=='nam') { 
      #Whoops, we overwrote the catalogue...
      tmp.nam<-load(file=file,...)
      cat<-get(tmp.nam)
    } else if (nam!='cat') { 
      cat<-get(nam)
    }
  } else { 
    stop("Unknown extension (not fits/asc/csv/Rdata) on file:\n",file)
  }
  return=cat
}
#/*fend*/}}}
