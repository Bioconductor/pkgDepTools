getDownloadSize <- function(url) {
    if (globals$have_RCurl) {
        h <- basicTextGatherer()
        junk <- getURI(url, writeheader=h$update, header=TRUE, nobody=TRUE)
        h <- h$value()
        ans <- parseContentLength(h)
    } else {
        ans <- as.numeric(NA)
    }
    ans
}

