vgsub<-function (pattern, replacement, x, ignore.case = FALSE, perl = FALSE, 
    fixed = FALSE, useBytes = FALSE) 
{
    if (!is.character(x)) 
        x <- as.character(x)
    if (length(pattern)!=length(replacement)) {
        if (length(replacement)>=1)  
          warning("replacement is not length(pattern); recyling the first element!") 
        replacement<-rep(replacement[1],length(pattern))
    }
    for (i in 1:length(pattern)) {
        x<-.Internal(gsub(as.character(pattern[i]), as.character(replacement[i]), 
            x, ignore.case, perl, fixed, useBytes))
    }
    return(x)
}
