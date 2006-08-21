"getInstallOrder" <-
function(pkg, depG, needed.only=TRUE) {
    toInst <- basicInstallOrder(pkg, depG)
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

