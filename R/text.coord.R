text.coord<-function (loc, inset = 0) {
    auto <- match.arg(loc, c("bottomright", "bottom", "bottomleft", 
            "left", "topleft", "top", "topright", "right", "center"))
    if (is.na(auto)) {
            stop("invalid coordinate!")
    }
    usr <- par("usr")
    inset <- rep_len(inset, 2)
    insetx <- inset[1L] * (usr[2L] - usr[1L])
    left <- switch(auto, bottomright = , topright = , 
        right = usr[2L] - insetx, bottomleft = , 
        left = , topleft = usr[1L] + insetx, bottom = , 
        top = , center = (usr[1L] + usr[2L] )/2)
    insety <- inset[2L] * (usr[4L] - usr[3L])
    top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3L] + 
                  insety, topleft = , top = , topright = usr[4L] - insety, 
                  left = , right = , center = (usr[3L] + usr[4L])/2)
    return=cbind(x=left,y=top)
}
