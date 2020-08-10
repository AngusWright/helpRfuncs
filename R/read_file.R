
#All-in-one function for reading input files /*fold*/{{{ 
read.file<-function(file,extname,...) { 
  #Check for file 
  if (!file.exists(file)) { 
    stop("File ",file," does not exist!\n")
  }
  #Determine the desired file type and output it
  if (grepl('\\.fits',file,ignore.case=TRUE)){
    if ("Rfits" %in% rownames(installed.packages())) { 
      cat<-Rfits::Rfits_read_table(file=file,...)
    } else if ("FITSio" %in% rownames(installed.packages())) {
      cat<-FITSio::readFITS(file=file,...)
    } else { 
      stop("There is no FITS package installed (Rfits or FITSio)")
    }
  } else if (grepl('\\.cat',file,ignore.case=TRUE)){
    hdr<-list(keyvalues=list(NAXIS=0))
    exten=1
    while (class(hdr)!="try-error") {
      if ((length(hdr$keyvalues$NAXIS)!=0 && hdr$keyvalues$NAXIS > 0) & 
         !(!missing(extname) && length(hdr$keyvalues$EXTNAME)>0 && hdr$keyvalues$EXTNAME!=extname)) { 
        break
      }
      exten<-exten+1
      if ("Rfits" %in% rownames(installed.packages())) { 
        hdr<-try(Rfits::Rfits_read_header(file=file,ext=exten))
      } else if ("FITSio" %in% rownames(installed.packages())) {
        hdr<-try(FITSio::readFITS(file=file,ext=exten))
      } else { 
        stop("There is no FITS package installed (Rfits or FITSio)")
      }
    }
    if ("Rfits" %in% rownames(installed.packages())) { 
      cat<-Rfits::Rfits_read_table(file=file,ext=exten,...)
    } else if ("FITSio" %in% rownames(installed.packages())) {
      cat<-FITSio::readFITS(file=file,ext=exten,...)
    } else { 
      stop("There is no FITS package installed (Rfits or FITSio)")
    }
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
