\name{makeDepGraph}
\alias{makeDepGraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
makeDepGraph(repList, suggests.only = FALSE, type = getOption("pkgType"), keep.builtin = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repList}{ ~~Describe \code{repList} here~~ }
  \item{suggests.only}{ ~~Describe \code{suggests.only} here~~ }
  \item{type}{ ~~Describe \code{type} here~~ }
  \item{keep.builtin}{ ~~Describe \code{keep.builtin} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(repList, suggests.only=FALSE,
                         type=getOption("pkgType"), 
                         keep.builtin=FALSE)
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
              deps <- deps[!(deps \%in\% baseOrRecPkgs)]
            if (length(deps)) {
                notFound <- ! (deps \%in\% nodes(depG))
                if (any(notFound))
                  depG <- addNode(deps[notFound], depG)
                deps <- deps[!is.na(deps)]
                depG <- addEdge(from=p, to=deps, depG)
            }
            size <- getDownloadSize(makePkgUrl(pMat[p, , drop=FALSE]))
            nodeData(depG, n=p, attr="size") <- size
        }
    }
    depG
  }
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
