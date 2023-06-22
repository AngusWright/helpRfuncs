
#All-in-one function for reading input files /*fold*/{{{ 
read.file<-function(file,extname="OBJECTS",...) { 
  #Check for file 
  if (!file.exists(file)) { 
    stop("File ",file," does not exist!\n")
  }
  #Determine the desired file type and output it
  if (grepl('\\.fits',file,ignore.case=TRUE)|grepl('\\.cat',file,ignore.case=TRUE)){
    hdr<-list(keyvalues=list(NAXIS=0))
    exten=1
    if (!"Rfits" %in% rownames(installed.packages())) { 
      stop("Cannot read FITS file: Rfits is not installed!")
    }
    extnames<-Rfits::Rfits_extnames(file)
    extnames[which(is.na(extnames))]<-""
    if (!extname %in% extnames) { 
      if (any(extnames!="")) { 
        exten=rev(which(extnames!=""))[1]
        warning("Did not find extension:",extname,".\nAssuming the last named extension (",exten,
                ", named",extnames[exten],") is correct...")
      } else { 
        warning("Did not find extension:",extname,".\nThere are no named extensions. Assuming that the last one  (",exten,
                ") is correct...")
        exten=length(extnames)
      } 
    } else if (length(which(extnames==extname))>1) { 
      warning("The requested extension",extname,"has multiple instances in the file?! Taking the first one...")
      exten<-which(extnames==extname)[1]
    } else { 
      exten<-which(extnames==extname)
    }
    cat<-Rfits::Rfits_read_table(file=file,ext=exten,...)
  } else if (grepl('\\.txt',file,ignore.case=TRUE)){
    if ("data.table" %in% rownames(installed.packages())) { 
      cat<-data.table::fread(file=file,...)
    } else { 
      stop("Cannot read txt file: data.table is not installed!")
    }
  } else if (grepl('\\.asc',file,ignore.case=TRUE)){
    if ("data.table" %in% rownames(installed.packages())) { 
      cat<-data.table::fread(file=file,...)
    } else { 
      stop("Cannot read ascii file: data.table is not installed!")
    }
  } else if (grepl('\\.csv',file,ignore.case=TRUE)){
    if ("data.table" %in% rownames(installed.packages())) { 
      cat<-data.table::fread(file=file,...)
    } else { 
      stop("Cannot read CSV file: data.table is not installed!")
    }
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
    stop(paste0("Unknown extension (not fits/cat/asc/txt/csv/Rdata) on file:\n",file))
  }
  #Check for bad header read 
  if (colnames(cat)[1]=='#') { 
    warning("The file header was read incorrectly due to a leading '#'. Correcting.")
    #We read the comment charachter as a column name. Shift all names across one 
    colnames(cat)<-c(colnames(cat)[-1],"#")
    cat[["#"]]<-NULL
  } 
  return=cat
}
#/*fend*/}}}

#All-in-one function for reading input files /*fold*/{{{ 
read.chain<-function(file,skip=200,...) { 
  #Check for file 
  if (!file.exists(file)) { 
    stop("File ",file," does not exist!\n")
  }
  #Read the header line 
  header<-data.table::fread(file=file,skip=0,nrows=1,header=FALSE)
  cat<-data.table::fread(file=file,skip=skip,...)
  #Check for bad header read 
  if (header[1]=='#') { 
    header<-header[-1]
  } 
  header<-helpRfuncs::vecsplit(header,by='#',n=-1)
  colnames(cat)<-header
  return=cat
}
#/*fend*/}}}
