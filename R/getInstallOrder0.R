"getInstallOrder0" <-
function(pkg, depG, sugG=NULL, needed.only=FALSE) {
    deps <- basicInstallOrder(pkg, depG)
    if (!is.null(sugG)) {
        sugs <- edges(sugG)[[pkg]]
        sugs <- lapply(sugs, basicInstallOrder, depG)
        if (length(sugs))
          sugs <- unlist(sugs)
        else
          sugs <- character(0)
        toInst <- unique(c(deps, sugs, pkg))
    } else {
        toInst <- deps
    }
    if (needed.only)
      toInst <- setdiff(toInst, rownames(installed.packages()))
    if (length(toInst)) {
        sizes <- unlist(nodeData(depG, n=toInst, attr="size"))
        sizesC <- as.character(round(sizes, 2))
        sizesC[is.na(sizes)] <- "?"
        sizesC <- paste(sizesC, "MB", sep="")
        names(toInst) <- sizesC
        total <- sum(sizes[!is.na(sizes)])
        list(packages=toInst, total.size=total)
    } else {
        list(packages=character(0), total.size=numeric(0))
    }
}

