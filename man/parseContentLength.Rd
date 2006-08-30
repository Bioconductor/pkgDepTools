\name{parseContentLength}
\alias{parseContentLength}

\title{Parse HTTP Response Header for Content Length}
\description{
  Given a string containing an HTTP response header, extract the
  \code{"Content-Length"} header and convert it to a numeric value.
  If no such header is found, return \code{NA}.
}
\usage{
parseContentLength(h)
}

\arguments{
  \item{h}{string containing the HTTP response header}
}
\details{
  Internal function
}

\author{Seth Falcon}
\keyword{utilities}
