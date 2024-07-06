
#All-in-one function for reading input files /*fold*/{{{ 
read.file<-function(file,extname="OBJECTS",cols,type,...) { 
  #Check for file 
  if (!file.exists(file)) { 
    stop("File ",file," does not exist!\n")
  }
  #Automatically detect file type{{{
  if (missing(type)) { 
    if (grepl('\\.fits',file,ignore.case=TRUE)|grepl('\\.cat',file,ignore.case=TRUE)){
      type='fits'
    } else if (grepl('\\.txt',file,ignore.case=TRUE)|grepl('\\.dat',file,ignore.case=TRUE)){
      type='text'
    } else if (grepl('\\.asc',file,ignore.case=TRUE)){
      type='ascii'
    } else if (grepl('\\.csv',file,ignore.case=TRUE)){
      type='csv'
    } else if (grepl('\\.Rdata',file,ignore.case=TRUE)){
      type='rdata'
    } else if (grepl('\\.rds',file,ignore.case=TRUE)){
      type='rds'
    } else if (grepl('\\.feather',file,ignore.case=TRUE)|grepl('\\.arrow',file,ignore.case=TRUE)){
      type='feather'
    } else if (grepl('\\.parquet',file,ignore.case=TRUE)){
      type='parquet'
    } else { 
      stop(paste0("Cannot automatically detect type: Unknown extension (not fits/cat/ascii/txt/dat/csv/Rdata/RDS/arrow/feather/parquet) on file:\n",file))
    }
  } else { 
    type=match.arg(tolower(type),c("fits","cat","ascii","text","txt","dat","csv","rdata","rds","arrow","feather","parquet"))
  }
  #}}}
  #Read the desired file type and output it
  if (type%in%c('fits',"cat")) { 
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
  } else if (type%in%c('text',"txt","dat")) { 
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
  } else if (type=='ascii'){ 
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
  } else if (type=='csv') { 
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
  } else if (type=='Rdata') { 
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
  } else if (type=='rds') {
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
  } else if (type%in%c('feather',"arrow")) { 
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
  } else if (type=='parquet'){ 
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
    stop(paste0("Unknown extension (not fits/cat/asc/txt/dat/csv/Rdata/RDS/arrow/feather/parquet) on file:\n",file))
  }
  #Check for bad header read 
  if (colnames(cat)[1]=='#') { 
    warning("The file header was read incorrectly due to a leading '#'. Correcting.")
    #We read the comment charachter as a column name. Shift all names across one 
    colnames(cat)<-c(colnames(cat)[-1],"#")
    if (any(colnames(cat)==paste0("V",1:ncol(cat)+1))) { 
      ind<-which(colnames(cat)==paste0("V",1:ncol(cat)+1))
      colnames(cat)[ind]<-paste0("V",ind)
    }
    if (all(is.na(cat[["#"]]))) { 
      cat[["#"]]<-NULL
    } else { 
      colnames(cat)[ncol(cat)]<-paste0("V",ncol(cat))
    }
    if (any(duplicated(colnames(cat)))) { 
      ind<-which(duplicated(colnames(cat)))
      warning(paste("catalogue has",length(ind),"duplicated column name(s); these are appended with their column number"))
      colnames(cat)[ind]<-paste0(colnames(cat)[ind],"_","V",ind)
    }
  } 
  if (any(colnames(cat)==paste0("V",1:ncol(cat))) & !all(colnames(cat)==paste0("V",1:ncol(cat)))) { 
    #File was read with partial header information
    warning(paste0("The catalogue has been read with partial header information?!\nThe available column names are: ",
                   paste(collapse=' ',colnames(cat)[which(colnames(cat)!=paste0("V",1:ncol(cat)))]),"\nIs this file in Robenjamert format?!"))
  }
  return=cat
}
#/*fend*/}}}

#All-in-one function for reading input files /*fold*/{{{ 
read.chain<-function(file,skip=1,strip_labels=TRUE,...) { 
  #Check for file 
  if (!file.exists(file)) { 
    stop("File ",file," does not exist!\n")
  }
  #Read the header line 
  header<-data.table::fread(file=file,skip=0,nrows=1,header=FALSE)
  if (any(dim(header)==0)) return(header)
  cat<-data.table::fread(file=file,skip=skip,nrow=1,header=FALSE)
  while (grepl("#",cat[[1]][1])) { 
    skip=skip+1
    cat<-data.table::fread(file=file,skip=skip,nrow=1,header=FALSE)
    if (any(dim(cat)<1)) cat<-data.frame(dummy="#")
  }
  cat<-data.table::fread(file=file,skip=skip-1,header=FALSE,...)
  #Check for bad header read 
  if (header[1]=='#') { 
    header<-header[-1]
  } 
  header<-helpRfuncs::vecsplit(header,by='#',n=-1)
  if (strip_labels) { 
    header<-gsub("cosmological_parameters--","",header,ignore.case=TRUE)
    header<-gsub("nofz_shifts--","nz_",header,ignore.case=TRUE)
    header<-gsub("halo_model_parameters--","hm_",header,ignore.case=TRUE)
    header<-gsub("intrinsic_alignment_parameters--","ia_",header,ignore.case=TRUE)
  }
  colnames(cat)<-header
  return=cat
}
#/*fend*/}}}
