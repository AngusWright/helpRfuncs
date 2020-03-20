#
# Function to open an arbitrary plot device
#
open_plot<-function(file,height,width,res=220,...) {

  if (missing(width)) { 
    width<-7
  }
  if (missing(height)) { 
    height<-7
  }
  #Check calling syntax {{{
  if (length(file)!=1) { 
    stop("File must be of length 1")
  }
  #}}}
  #Get the plot type from the extension
  if (grepl('.png',file,ignore.case=T)) { 
    png(file=file,res=res,height=height*res,width=width*res)
  } else if (grepl('.pdf',file,ignore.case=T)) {
    pdf(file=file,height=height,width=width)
  } else { 
    stop("Unknown file extension: ",file)
  }
  return=NULL
}
