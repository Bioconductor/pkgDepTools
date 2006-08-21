\name{cleanPkgField}
\alias{cleanPkgField}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cleanPkgField(val)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{val}{ ~~Describe \code{val} here~~ }
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
function(val) {
    ## Given the value from a field like 'Depends' in a package's
    ## DESCRIPTION file, return a character vector of package names
    ## with the version restrictions stripped and \R~removed.
    ## FIXME: uses a private function from tools
    if (is.na(val))
      return(character(0))
    val <- names(tools:::.split_dependencies(val))
    if (is.null(val))
      return(character(0))
    val <- val[! val \%in\% "R"]
    if (length(val))
      return(val)
    return(character(0))
  }
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
