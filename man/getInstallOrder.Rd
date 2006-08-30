\name{getInstallOrder}
\alias{getInstallOrder}

\title{List package dependencies in install order}

\description{

  This function uses a dependency graph created with
  \code{makeDepGraph} to list all (recursive) dependencies of a
  given package in an order suitable for installation.

}

\usage{
getInstallOrder(pkg, depG, needed.only = TRUE)
}

\arguments{
  \item{pkg}{character string name of package}

  \item{depG}{\code{graphNEL} instance as returned from
      \code{makeDepGraph}.  
    }

  \item{needed.only}{logical value.  When \code{TRUE}, only those
      dependencies not currently installed are included in the list,
      this is the default.  When \code{FALSE} the complete list of
      dependencies is given regardless of the set of currently
      installed packages.}
}

\value{
  a list with components:

  \item{packages }{character vector of package names in an order
      suitable for installation.  The order is not unique.  The names
      of the character vector are the estimated download sizes of each
      package.}

  \item{total.size}{When available, the total size in
      megabytes of all listed dependencies.}
}


\author{Seth Falcon}

\keyword{utilities}
