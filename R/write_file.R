
#All-in-one function for writing output files /*fold*/{{{ 
write.file<-function(file,cat,quote=FALSE,row.names=FALSE,col.names=TRUE,verbose=FALSE,...) { 
  #Determine the desired file type and output it
  if (grepl('\\.fits',file,ignore.case=TRUE)){
    #Write as FITS {{{
    #Check for Rfits {{{
    if (!"Rfits" %in% rownames(installed.packages())) { 
      stop("Rfits is not installed; cannot write to FITS")
    }
    #}}}
    #Check for factors /*fold*/ {{{
    if (any(unlist(lapply(cat,class))=='factor')) { 
      for (col in which(unlist(lapply(cat,class))=='factor')) { 
        cat[[col]]<-levels(cat[[col]])[cat[[col]]]
      }
    }
    #/*fend*/}}}
    #Write to output file {{{
    Rfits::Rfits_write_table(file=file,cat,verbose=verbose,...)
    #}}}
    #}}}
  } else if (grepl('\\.cat',file,ignore.case=TRUE)){
    #Write as LDAC {{{
    #Check for Rfits {{{
    if (!"Rfits" %in% rownames(installed.packages())) { 
      stop("Rfits is not installed; cannot write to LDAC-FITS")
    }
    #}}}
    #Check for factors /*fold*/ {{{
    if (any(unlist(lapply(cat,class))=='factor')) { 
      for (col in which(unlist(lapply(cat,class))=='factor')) { 
        cat[[col]]<-levels(cat[[col]])[cat[[col]]]
      }
    }
    #/*fend*/}}}
    #Check for 64-bit integers {{{
    classes<-unlist(lapply(cat,class))
    if (any(classes=="integer64")) { 
      cat("WARNING: catalogue contains integer64 columns, which are incompatible with the LDAC-FITS format:\n")
      #Convert to 32-bit 
      for (i in names(classes)[which(classes=='integer64')]) {
        cat(paste("-> Converting column",i,"from 64- to 32-bit integer\n"))
        cat[[i]]<-as.integer(cat[[i]])
      }
    }
    #}}}
    #Check for the SeqNr and FIELD_POS variables {{{
    #Add the field pos column{{{
    if (!any(colnames(cat)=="SeqNr")) {
      cat(paste("Adding SeqNr column\n"))
      cat$SeqNr<-as.integer(1:nrow(cat))
    }
    #}}}
    #Add the field pos column {{{
    if (!any(colnames(cat)=="FIELD_POS")) {
      cat(paste("Adding FIELD_POS column\n"))
      cat$FIELD_POS<-as.integer(1)
    }
    #}}}
    #}}}
    #Write the OBJECTS extension {{{
    #Get the column types 
    cattypes<-get_types(cat)
    #use short int for the FIELD_POS
    cattypes[which(colnames(cat)=='FIELD_POS')]<-'1I'
    #Write the OBJECTS 
    Rfits::Rfits_write_table(file=file,cat,tforms=cattypes,extname="OBJECTS",verbose=verbose)
    #}}}
    #Write the FIELDS extension {{{
    #Construct the FIELDS table 
    fields<-data.frame(OBJECT_POS=as.integer(1),OBJECT_COUNT=nrow(cat),CHANNEL_NR=as.integer(0),CHANNEL_NAME="I",SubNr=as.integer(1))
    #Construct the fields extension comments 
    fcomms<-list(TCOMM1  = 'Starting position of field', TCOMM2  = 'Number of objects in field', TCOMM3  = 'Channel numbers', TCOMM4  = 'Channel name', TCOMM5  = 'Subset Number')
    #Get the fields data types
    cattypes<-get_types(fields)
    cattypes[which(colnames(fields)=="CHANNEL_NAME")]<-"16A"
    #Get the existing extensions 
    extn<-Rfits::Rfits_extnames(filename=file)
    #Write the FIELDS table 
    Rfits::Rfits_write_table(file=file,fields,extname="FIELDS",ext=length(extn)+1,tforms=cattypes,tadd=fcomms,
                           overwrite_file=FALSE,create_file=FALSE,verbose=verbose,create_ext=TRUE)
    #}}}
    #}}}
  } else if (grepl('\\.txt',file,ignore.case=TRUE)){
    #Write to ascii {{{
    write.table(file=file,cat,quote=quote,row.names=row.names,col.names=col.names,...)
    #}}}
  } else if (grepl('\\.dat',file,ignore.case=TRUE)){
    #Write to ascii {{{
    write.table(file=file,cat,quote=quote,row.names=row.names,col.names=col.names,...)
    #}}}
  } else if (grepl('\\.asc',file,ignore.case=TRUE)){
    #Write to ascii {{{
    write.table(file=file,cat,quote=quote,row.names=row.names,col.names=col.names,...)
    #}}}
  } else if (grepl('\\.csv',file,ignore.case=TRUE)){
    #Write to CSV with data.table {{{
    #data.table::fwrite(file=file,cat,quote=quote,row.names=row.names,...)
    data.table::fwrite(file=file,cat,...)
    #}}}
  } else if (grepl('\\.Rdata',file,ignore.case=TRUE)){
    #Save as Rdata {{{
    save(file=file,cat,...)
    #}}}
  } else { 
    stop(paste0("Unknown extension (not fits/cat/asc/csv/Rdata):",file))
  }
  return=NULL
}
#/*fend*/}}}

#Get types function {{{
get_types<-function(table) { 
  ttypes = colnames(table)
  check.logical = sapply(table, is.logical)
  check.int = sapply(table, is.integer)
  check.integer64 = sapply(table, bit64::is.integer64)
  check.double = sapply(table, is.numeric) & (!check.int) & (!check.integer64)
  check.char = sapply(table, is.character)
  tforms = character(ncol(table))
  tforms[check.logical] = "1J"
  tforms[check.int] = "1J"
  tforms[check.integer64] = "1K"
  tforms[check.double] = "1D"
  if (data.table::is.data.table(table)) { 
    res=try(tforms[check.char] <- paste(sapply(table[, ..check.char,drop = FALSE], function(x) max(nchar(x)) + 1),"A", sep = ""))
    if (class(res)=='try-error') { 
      tforms[check.char] = paste(sapply(table[, check.char,drop = FALSE], function(x) max(nchar(x)) + 1),"A", sep = "")
    }
  } else { 
    tforms[check.char] = paste(sapply(table[, check.char,drop = FALSE], function(x) max(nchar(x)) + 1),"A", sep = "")
  } 
  return=tforms
}
#}}}
