\name{getDownloadSize}
\alias{getDownloadSize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get Size in MB of a URL Using RCurl}
\description{
  Returns the size in MB of the specified URL.  Uses RCurl to
  parse the HTTP headers for Content-Length. 
}
\usage{
getDownloadSize(url)
}

\arguments{
  \item{url}{character vector giving the URL.  This is not vectorized.}
}

\author{Seth Falcon}
\note{ 
Internal function.
}

\keyword{utilities}
