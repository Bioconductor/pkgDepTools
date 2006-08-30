globals <- new.env(parent=emptyenv(), hash=TRUE)

.First.lib <- function(libname, pkgname) {
    globals$have_RCurl <- suppressWarnings(require("RCurl", quietly=TRUE))
    ## FIXME: temporary fix since RCurl segfaults 
    if (globals$have_RCurl) {
        cat("Disabling RCurl due to R-devel/RCurl segfault issue\n")
        globals$have_RCurl <- FALSE
    }
}
