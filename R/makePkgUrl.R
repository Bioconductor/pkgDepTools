"makePkgUrl" <-
function(pMat, type=getOption("pkgType")) {
    pkg <- paste(pMat[, "Package"], pMat[, "Version"], sep="_")
    ext <- switch(type,
                  source="tar.gz",
                  win.binary="zip",
                  mac.binary="tgz")
    pkg <- paste(pkg, ext, sep=".")
    paste(pMat[, "Repository"], pkg, sep="/")
}

