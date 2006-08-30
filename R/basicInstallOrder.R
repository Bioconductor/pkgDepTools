basicInstallOrder <- function(pkg, depG) {
    ## Helper function to return the complete install order
    ## for the given package.
    allPkgs <- c(pkg, names(acc(depG, pkg)[[1]]))
    if (length(allPkgs) > 1) {
        pkgSub <- subGraph(allPkgs, depG)
        toInst <- tsort(pkgSub) 
        if (!is.character(toInst))
          stop("depG is not a DAG")
        rev(toInst)
    } else {
        allPkgs
    }
}

