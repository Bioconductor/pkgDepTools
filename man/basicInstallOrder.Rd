\name{basicInstallOrder}
\alias{basicInstallOrder}

\title{Complete Installation Order of a Given Package}
\description{
  Helper function to return the complete install order of a given
  package.
}
\usage{
basicInstallOrder(pkg, depG)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pkg}{character string package name}
  \item{depG}{\code{graphNEL} instance as returned by \code{makeDepGraph}}
}
\details{
  Internal function.
}
\value{
  character vector of package names in a viable installation order.
}

\author{Seth Falcon}
\keyword{utilities}

