
#All-in-one function for reading input files /*fold*/{{{ 
read.file<-function(file,extname="OBJECTS",cols,...) { 
  #Check for file 
  if (!file.exists(file)) { 
    stop("File ",file," does not exist!\n")
  }
  #Determine the desired file type and output it
  if (grepl('\\.fits',file,ignore.case=TRUE)|grepl('\\.cat',file,ignore.case=TRUE)){
    #FITS & LDAC {{{ 
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
                ", named ",extnames[exten],") is correct...")
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
    basecols=Rfits::Rfits_read_colnames(file,ext=exten)
    if (missing(cols)) { 
      cols=basecols
    } else if (any(!cols%in%basecols)) { 
      stop(paste("Requested columns not found in catalogue:",paste(collapse=' ',cols[which(!cols%in%basecols)])))
    }
    cat<-Rfits::Rfits_read_table(file=file,ext=exten,cols=cols,...)
    #}}}
  } else if (grepl('\\.txt',file,ignore.case=TRUE)|grepl('\\.dat',file,ignore.case=TRUE)){
    #Text {{{
    if ("data.table" %in% rownames(installed.packages())) { 
      if (!missing(cols)) { 
        warning("Cannot read column subset from TXT catalogue; reading all columns")
      }
      cat<-data.table::fread(file=file,...)
      if (!missing(cols)) {
        if (any(!cols%in%colnames(cat))) { 
          stop(paste("Requested columns were not found in the read catalogue:",paste(collapse=' ',cols[which(!cols%in%colnames(cat))])))
        }
      }
    } else { 
      stop("Cannot read txt file: data.table is not installed!")
    }
    #}}}
  } else if (grepl('\\.asc',file,ignore.case=TRUE)){
    #ASCII {{{
    if ("data.table" %in% rownames(installed.packages())) { 
      if (!missing(cols)) { 
        warning("Cannot read column subset from ASCII catalogue; reading all columns")
      }
      cat<-data.table::fread(file=file,...)
      if (!missing(cols)) {
        if (any(!cols%in%colnames(cat))) { 
          stop(paste("Requested columns were not found in the read catalogue:",paste(collapse=' ',cols[which(!cols%in%colnames(cat))])))
        }
      }
    } else { 
      stop("Cannot read ascii file: data.table is not installed!")
    }
    #}}}
  } else if (grepl('\\.csv',file,ignore.case=TRUE)){
    #CSV {{{
    if ("data.table" %in% rownames(installed.packages())) { 
      if (!missing(cols)) { 
        warning("Cannot read column subset from CSV catalogue; reading all columns")
      }
      cat<-data.table::fread(file=file,...)
      if (!missing(cols)) {
        if (any(!cols%in%colnames(cat))) { 
          stop(paste("Requested columns were not found in the read catalogue:",paste(collapse=' ',cols[which(!cols%in%colnames(cat))])))
        }
      }
    } else { 
      stop("Cannot read CSV file: data.table is not installed!")
    }
    #}}}
  } else if (grepl('\\.Rdata',file,ignore.case=TRUE)){
    #Rdata {{{
    if (!missing(cols)) { 
      warning("Cannot load column subset from Rdata catalogue; loading all columns")
    }
    nam<-load(file=file,...)
    if (nam=='nam') { 
      #Whoops, we overwrote the catalogue...
      tmp.nam<-load(file=file,...)
      cat<-get(tmp.nam)
    } else if (nam!='cat') { 
      cat<-get(nam)
    }
    if (!missing(cols)) {
      if (any(!cols%in%colnames(cat))) { 
        stop(paste("Requested columns were not found in the read catalogue:",paste(collapse=' ',cols[which(!cols%in%colnames(cat))])))
      }
    }
    #}}}
  } else if (grepl('\\.rds',file,ignore.case=TRUE)){
    #RDS {{{
    if (!missing(cols)) { 
      warning("Cannot load column subset from RDS catalogue; loading all columns")
    }
    cat<-readRDS(file=file,...)
    if (!missing(cols)) {
      if (any(!cols%in%colnames(cat))) { 
        stop(paste("Requested columns were not found in the read catalogue:",paste(collapse=' ',cols[which(!cols%in%colnames(cat))])))
      }
    }
    #}}}
  } else if (grepl('\\.feather',file,ignore.case=TRUE)){
    #Feather {{{
    if ("arrow" %in% rownames(installed.packages())) { 
      if (!missing(cols)) { 
        cat<-arrow::read_feather(file=file,col_select=cols,...)
      } else { 
        cat<-arrow::read_feather(file=file,...)
      }
    } else { 
      stop("Cannot read feather file: arrow is not installed!")
    }
    #}}}
  } else if (grepl('\\.parquet',file,ignore.case=TRUE)){
    #Parquet {{{
    if ("arrow" %in% rownames(installed.packages())) { 
      if (!missing(cols)) { 
        cat<-arrow::read_parquet(file=file,col_select=cols,...)
      } else { 
        cat<-arrow::read_parquet(file=file,...)
      }
    } else { 
      stop("Cannot read parquet file: arrow is not installed!")
    }
    #}}}
  } else { 
    stop(paste0("Unknown extension (not fits/cat/asc/txt/dat/csv/Rdata/RDS/feather/parquet) on file:\n",file))
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
