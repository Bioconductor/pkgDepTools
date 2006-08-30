parseContentLength <- function(h) {
    fldName <- "Content-Length:"
    sizeRegex <- "^.*Content-Length: ([0-9]+)[\r\n]+.*"
    ## Return size in megabytes
    if (length(grep(fldName, h)))
      as.numeric(sub(sizeRegex, "\\1", h)) / 1024^2
    else
      as.numeric(NA)
}

