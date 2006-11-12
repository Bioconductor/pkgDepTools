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

getDownloadSizes <- function(urls) {
    ## Fetch HTTP headers for a vector of URLs
    ## This appears to be much faster than looping over getDownloadSize
    ## for individual URLs.
    if (globals$have_RCurl) {
        h <- multiTextGatherer(urls)
        junk <- getURIAsynchronous(urls, write=h, header=TRUE, nobody=TRUE)
        headerContents <- sapply(h, function(x) {
            parseContentLength(x$value())
        })
    } else {
        headerContents <- rep(as.numeric(NA), length(urls))
        names(headerContents) <- urls
    }
    headerContents
}


getDownloadSizesBatched <- function(urls) {
    if (globals$have_RCurl) {
        BATCH <- 20
        done <- FALSE
        start <- 1
        N <- length(urls)
        headerContents <- numeric(N)
        while(!done) {
            end <- start + BATCH
            if (end >= N) {
                end <- N
                done <- TRUE
            }
            batchIdx <- seq.int(start, end)
            h <- multiTextGatherer(urls[batchIdx])
            junk <- getURIAsynchronous(urls[batchIdx], write=h,
                                       header=TRUE, nobody=TRUE)
            headerContents[batchIdx] <- sapply(h, function(x) {
                parseContentLength(x$value())
            })
            start <- end + 1
        }
    } else {
        headerContents <- rep(as.numeric(NA), length(urls))
    }
    names(headerContents) <- urls
    headerContents
}
