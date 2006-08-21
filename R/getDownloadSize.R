"getDownloadSize" <-
function(url) {
    h <- basicTextGatherer()
    junk <- getURI(url, writeheader=h$update, header=TRUE, nobody=TRUE)
    h <- h$value()
    parseContentLength(h)
}

