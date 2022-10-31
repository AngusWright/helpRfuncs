colWeightedCounts<-function (x, w, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, 
    dim. = dim(x), ...) 
{
    if (is.matrix(x)) {
    }
    else if (is.vector(x)) {
    }
    else {
        stop("Argument 'x' must be a matrix or a vector: ", mode(x)[1L])
    }
    dim. <- as.integer(dim.)
    if (length(value) != 1L) {
        stop("Argument 'value' has to be a single value: ", length(value))
    }
    storage.mode(value) <- storage.mode(x)
    #if (is.numeric(x) || is.logical(x)) {
    #    na.rm <- as.logical(na.rm)
    #    has_nas <- TRUE
    #    counts <- .Call(C_colCounts, x, dim., rows, cols, value, 
    #        2L, na.rm, has_nas)
    #}
    #else {
        if (is.vector(x)) 
            dim(x) <- dim.
        if (!is.null(rows) && !is.null(cols)) 
            x <- x[rows, cols, drop = FALSE]
        else if (!is.null(rows)) 
            x <- x[rows, , drop = FALSE]
        else if (!is.null(cols)) 
            x <- x[, cols, drop = FALSE]
        dim. <- dim(x)
        if (is.na(value)) {
            counts <- apply(x, MARGIN = 2L, FUN = function(x) sum(is.na(x)))
        }
        else if (!missing(w)) { 
            #if (length(x)!=length(w)) { 
            #  print(str(x)) 
            #  print(str(w))
            #  #stop("weight and index arrays differ") 
            #} 
            counts <- apply(x, MARGIN = 2L, FUN = function(x) {
              if (length(x)!=length(w)) { 
                stop(paste("weight and index arrays differ:\n",length(x),"!=",length(w)))
              } else {
                sum(w[x == value], na.rm = na.rm)}} )
        }
        else {
            counts <- apply(x, MARGIN = 2L, FUN = function(x) sum(x == 
                value, na.rm = na.rm))
        }
    #}
    if (missing(w)) { 
      as.integer(counts)
    } else { 
      as.numeric(counts)
    }
}
colWeightedTabulates<-function (x, w, rows = NULL, cols = NULL, values = NULL, cores=1, na.rm, ...) 
{
    library(foreach)
    if (cores>1) { 
      #cat(paste("Running in parallel:",cores,"\n"))
      library(doParallel)
      registerDoParallel(cores=cores)
    }

    if (is.integer(x)) {
    }
    else if (is.logical(x)) {
    }
    else if (is.raw(x)) {
    }
    else {
        stop("Argument 'x' is not of type integer or raw: ", 
            class(x)[1])
    }
    if (!is.null(rows) && !is.null(cols)) 
        x <- x[rows, cols, drop = FALSE]
    else if (!is.null(rows)) 
        x <- x[rows, , drop = FALSE]
    else if (!is.null(cols)) 
        x <- x[, cols, drop = FALSE]
    if (is.null(values)) {
        values <- as.vector(x)
        values <- unique(values)
        if (is.raw(values)) {
            values <- as.integer(values)
            values <- sort(values)
            names <- sprintf("%x", values)
            names <- paste("0x", names, sep = "")
            values <- as.raw(values)
        }
        else {
            values <- sort(values, na.last = TRUE)
            names <- as.character(values)
        }
    }
    else {
        if (is.raw(values)) {
            names <- sprintf("%x", as.integer(values))
            names <- paste("0x", names, sep = "")
        }
        else {
            names <- as.character(values)
        }
    }
    transpose <- FALSE
    if (!transpose) {
        nbr_of_values <- length(values)
        counts <- matrix(0L, nrow = ncol(x), ncol = nbr_of_values)
        colnames(counts) <- names
        if (missing(na.rm)) {
          na.rm <- matrixStats::anyMissing(x)
        }
        if (!missing(w)) { 
          counts<-foreach(value=values,.combine='cbind',.export=c('x','w'))%dopar% {
              return=colWeightedCounts(x, w, value = value, na.rm = na.rm)
          }
        } else { 
          counts<-foreach(value=values,.combine='cbind',.export=c('x'))%dopar% {
              return=colWeightedCounts(x, value = value, na.rm = na.rm)
          }
        }
    }
    counts
}
