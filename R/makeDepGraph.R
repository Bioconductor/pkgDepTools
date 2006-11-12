makeDepGraph <- function(repList, suggests.only=FALSE,
                         type=getOption("pkgType"), 
                         keep.builtin=FALSE, dosize=TRUE)
{
    ## Return a directed graphNEL instance.  Each package is a node,
    ## an edge goes from a package to each of its direct dependencies.
    ##
    ## By default, Suggests are ignored.  If suggests.only is TRUE,
    ## then _only_ suggests are considered for creating edges.  Note that
    ## whereas the dependencies should be a DAG, the suggests currently
    ## are not a DAG as there are a number of packages that suggest
    ## each other.
    pkgMatList <- lapply(repList, function(x) {
        available.packages(contrib.url(x, type=type))
    })
    if (!keep.builtin)
      baseOrRecPkgs <- rownames(installed.packages(priority="high"))
    allPkgs <- unlist(sapply(pkgMatList, function(x) rownames(x)))
    if (!length(allPkgs))
      stop("no packages in specified repositories")
    allPkgs <- unique(allPkgs)
    depG <- new("graphNEL", nodes=allPkgs, edgemode="directed")
    nodeDataDefaults(depG, attr="size") <- as.numeric(NA)
    for (pMat in pkgMatList) {
        for (p in rownames(pMat)) {
            if (!suggests.only) {
                deps <- cleanPkgField(pMat[p, "Depends"])
                deps <- c(deps, cleanPkgField(pMat[p, "Imports"]))
            } else {
                deps <- cleanPkgField(pMat[p, "Suggests"])
            }
            if (length(deps) && !keep.builtin)
              deps <- deps[!(deps %in% baseOrRecPkgs)]
            if (length(deps)) {
                notFound <- ! (deps %in% nodes(depG))
                if (any(notFound))
                  depG <- addNode(deps[notFound], depG)
                deps <- deps[!is.na(deps)]
                depG <- addEdge(from=p, to=deps, depG)
            }
        }
        if (dosize) {
            sizes <- getDownloadSizesBatched(makePkgUrl(pMat))
            nodeData(depG, n=rownames(pMat), attr="size") <- sizes
        }

    }
    depG
}

