\name{getInstallOrder0}
\alias{getInstallOrder0}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getInstallOrder0(pkg, depG, sugG = NULL, needed.only = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pkg}{ ~~Describe \code{pkg} here~~ }
  \item{depG}{ ~~Describe \code{depG} here~~ }
  \item{sugG}{ ~~Describe \code{sugG} here~~ }
  \item{needed.only}{ ~~Describe \code{needed.only} here~~ }
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
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
