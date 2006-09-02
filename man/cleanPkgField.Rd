\name{cleanPkgField}
\alias{cleanPkgField}

\title{Clean Packages Names from a DESCRIPTION File Field}
\description{

  Given the value from a field like 'Depends' in a package's
  DESCRIPTION file, return a character vector of package names with
  the version restrictions stripped and R removed.  

}
\usage{
cleanPkgField(val)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{val}{character string parsed from a package's DESCRIPTION file
    containing package names and possible version specifiers.}
}
\details{
  Internal function
}

\author{Seth Falcon}

\section{Warning}{
  FIXME: uses a private function from tools
}


\keyword{utilities}
