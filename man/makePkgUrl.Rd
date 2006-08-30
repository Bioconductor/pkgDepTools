\name{makePkgUrl}
\alias{makePkgUrl}

\title{Create a URL for a Package in a CRAN-Style Repository}
\description{
  Given a CRAN-style package repository meta data matrix as returned
  by \code{available.packages}, return a character vector of complete
  URLs for each package.
}
\usage{
makePkgUrl(pMat, type = getOption("pkgType"))
}

\arguments{
  \item{pMat}{matrix as returned by \code{available.packages}}
  \item{type}{character string, used to determine file extension.
  Should match the argument used to generate \code{pMat}.}
}
\details{
  Internal function.
}
\author{Seth Falcon}
\keyword{utilities}
