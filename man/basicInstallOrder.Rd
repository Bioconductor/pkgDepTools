\name{basicInstallOrder}
\alias{basicInstallOrder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
basicInstallOrder(pkg, depG)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pkg}{ ~~Describe \code{pkg} here~~ }
  \item{depG}{ ~~Describe \code{depG} here~~ }
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
function(pkg, depG) {
    ## Helper function to return the complete install order
    ## for the given package.
    allPkgs <- c(pkg, names(acc(depG, pkg)[[1]]))
    pkgSub <- subGraph(allPkgs, depG)
    toInst <- tsort(pkgSub) 
    if (!is.character(toInst))
      stop("depG is not a DAG")
    rev(toInst)
  }
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
