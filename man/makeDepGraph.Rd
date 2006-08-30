\name{makeDepGraph}
\alias{makeDepGraph}

\title{Create a graph representing R package dependencies}

\description{
  
  Given a list of CRAN-style repository URLs, this function creates a
  \code{graph} instance representing dependencies between packages in
  the repositories.

}

\usage{
makeDepGraph(repList, suggests.only = FALSE, type =
getOption("pkgType"), keep.builtin = FALSE, dosize = TRUE)
}

\arguments{
  \item{repList}{a character vector of URLs pointing to CRAN-style
      repositories.
    }

  \item{suggests.only}{logical value indicating whether only the
      \code{"Suggests"} field should be used.  The default
      (\code{FALSE}) means that \code{DESCRIPTION} fields
      \code{"Depends"} and \code{"Imports"} are used.  Note that
      \code{"Suggests"} is special in that its use is less regulated
      and the resulting graph is less likely to be a DAG (packages
      sometimes suggest each other).}

  \item{type}{a character vector specifying the type of packages to
      search for in the repositories.  Must be one of \code{"source"},
      \code{"win.binary"}, or \code{"mac.binary"}.}

  \item{keep.builtin}{logical value indicating whether or not packages
    that come with a default R installation should be included in the
    nodes of the dependency graph.  The default, \code{FALSE}, is to
    remove these packages from the return result.}

  \item{dosize}{logical value.  When \code{TRUE}, the function will
      attempt to estimate the download size of each package (requires
      the RCurl package).  The size of each package is added as a node
      attribute to the graph.}

}

\value{
  A \code{graphNEL-class} instance.  If the \code{dosize} argument was
  set to \code{TRUE}, then an estimate of the download size of each
  package is stored as a node attribute of the graph.  A missing value
  is used when the download size was not able to be determined as well
  as when the RCurl package is not available.
}

\author{Seth Falcon}

\examples{
  \dontrun{
    deps <- makeDepGraph("http://cran.fhcrc.org", type="source")
  }
}
\keyword{utilities}

