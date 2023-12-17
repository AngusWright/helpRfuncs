#
# Plotting & Calculation function for contours given scatter and 2d image data
#


contour<-function (x, y, z, h, doim = TRUE, docon = TRUE, dobar = TRUE, ngrid = 100, 
    add = FALSE, xlab = "", ylab = "", imcol = c(NA, rev(rainbow(1000, 
        start = 0, end = 2/3))), conlevels = c(0.5, pnorm(1) - 
        pnorm(-1), 0.95), barposition = "topright", barorient = "v", 
    bartitle = "Contained %", bartitleshift = 0, xlim = NULL, 
    ylim = NULL, weights = NULL, fill=FALSE, fill.col, col='black', returnLevels=TRUE, ...) 
{
  #Load relevant packages {{{
  library(magicaxis)
  library(sm)
  #}}}
  #Check for missing xy data and correct syntax {{{
  if (missing(y)) {
    #If y is missing, check if x in a Nx2 vector {{{
    if (!is.null(dim(x))) {
      #If so, use the xy values from there {{{
      if (dim(x)[2] >= 2) {
        y = x[, 2]
        x = x[, 1]
      }
      #}}}
    }
    #}}}
  }
  #}}}
  #If we are using fill, check that the length matches the contour levels {{{
  if (fill) { 
    if (length(fill.col)!=length(conlevels)) { 
      stop("length(conlevels) fill.col values must be specified with 'fill=TRUE'")
    }
  }
  #}}}
  #Define the valid x/y data {{{
  use = !is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x) & 
    !is.na(y) & !is.nan(y) & !is.null(y) & is.finite(y)
  #}}}
  #If provided, use only valid z data as well {{{
  if (!missing(z)) use = use & !is.na(z) & !is.nan(z) & !is.null(z) & is.finite(z)
  #}}}
  #If not provided, define the x/y limits {{{
  if (is.null(xlim)) {
    xlim = range(x[use], na.rm = TRUE)
  }
  if (is.null(ylim)) {
    ylim = range(y[use], na.rm = TRUE)
  }
  #}}}
  if (any(!is.finite(h))) { 
    htmp <- h.select(x = cbind(x,y), y = NA, weights = weights,nbins = 0)
  } else { 
    htmp <- h 
  }
  #Only use data within 5 sigma of the x and y limits {{{
  use = use & x >= (min(xlim) - 5*htmp[1]) & x <= (max(xlim) + 5*htmp[1]) & 
              y >= (min(ylim) - 5*htmp[2]) & y <= (max(ylim) + 5*htmp[2])
  #}}}
  #If provided, check the length of the weights {{{
  if (is.null(weights) == FALSE) {
    if (length(weights) == length(x)) {
      #Only use valid weights 
      weights = weights[use]
    } else {
      #Error 
      stop("weights must match lengths of x / y")
    }
  }
  #}}}
  #Only use valid x/y data {{{
  x = x[use]
  y = y[use]
  #}}}
  #If provided, only use valid z data {{{
  if (!missing(z)) { 
    z = z[use]
  }
  #}}}
  #Define the contour levels as integrating from peak down {{{
  conlevels = 1 - conlevels
  #}}}
  #Generate the image to use for contouring {{{
  if (missing(z)) { 
    #If no z-data provided, construct the (weighted-)count image {{{
    #If non-finite smoothing kernel sizes provided, define them new {{{
    if (any(!is.finite(h))) { 
      h <- h.select(x = cbind(x,y), y = NA, weights = weights,nbins = 0)
    }
    #}}}
    #Construct the contour map {{{
    tempcon = sm.density(cbind(x, y), h = h, weights = weights, 
                         display = "none", ngrid = ngrid, xlim = xlim + c(-diff(xlim), 
                                                                          diff(xlim)), ylim = ylim + c(-diff(ylim), diff(ylim)), 
                         verbose = FALSE,...)
    #}}}
    #Extract the x/y/z values {{{ 
    tempcon$x = tempcon$eval.points[, 1]
    tempcon$y = tempcon$eval.points[, 2]
    tempcon$z = tempcon$estimate
    #}}}
    #}}}
  } else { 
    #Otherwise, use the z-data as the image {{{
    tempcon = data.frame(x=x,y=y,z=z)
    #}}}  
  }
  #}}}
  #Construct the CDF of map values {{{ 
  #Sort the z-values {{{
  temp = sort(tempcon$z)
  #}}}
  #Cumulative distribution {{{
  tempsum = cumsum(temp)
  #}}}
  #Continuous function of CDF with linear interpolation {{{
  convfunc = approxfun(tempsum, temp)
  #}}}
  #}}}
  #Generate the level map {{{
  levelmap = approxfun(convfunc(seq(min(tempsum), max(tempsum), len = 1000)), 
                       seq(0, 1, len = 1000))
  #}}}
  #Convert the image to the level map {{{
  tempcon$z = matrix(levelmap(tempcon$z), nrow = ngrid)
  tempcon$z[is.na(tempcon$z)] = min(tempcon$z, na.rm = TRUE)
  #}}}
  #If not "add"-ing, generate the plot from scratch {{{
  if (add == FALSE) {
    plot.new()
    plot.window(xlim = xlim, ylim = ylim)
    usrlims = par()$usr
    rect(usrlims[1], usrlims[3], usrlims[2], usrlims[4], 
         col = imcol[1])
  }
  #}}}
  #If requested, plot the image {{{
  if (doim) {
    magimage(tempcon, col = imcol, axes = FALSE, add = TRUE, 
             xlim = xlim, ylim = ylim, magmap = FALSE)
  }
  #}}}
  #If requested, plot the contours {{{
  if (docon) { 
    #Generate the contour lines {{{
    con.res=contourLines(tempcon, levels = conlevels)
    #}}}
    #Plot each level {{{ 
    for (lev in order(conlevels,decreasing=FALSE)) { 
      if (fill) { 
        #Use filled polygons {{{
        polygon(con.res[[lev]],col=fill.col[lev],border=col)
        #}}}
      } else { 
        #Use lines {{{
        lines(con.res[[lev]],col=col,...)
        #}}}
      }
    }
    #}}}
  }
  #}}}
  #plot the bounding box if needed {{{
  if (!doim) {
    box()
  }
  #}}}
  #If not "add"-ing, draw the axes {{{
  if (add == FALSE) {
    magaxis(xlab = xlab, ylab = ylab)
  }
  #}}}
  #If requested, draw the colourbar {{{
  if (dobar) {
    helpRfuncs::magbar(position = barposition, range = c(0, 100), orient = barorient, 
           col = rev(imcol), title = bartitle, titleshift = bartitleshift)
  }
  #}}}
  #If requested, return the contour levels {{{
  if (returnLevels & docon) { 
    tempcon$contours=con.res
  } 
  #}}}
  #Return 
  return=tempcon
}
