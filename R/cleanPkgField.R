"cleanPkgField" <-
function(val) {
    ## Given the value from a field like 'Depends' in a package's
    ## DESCRIPTION file, return a character vector of package names
    ## with the version restrictions stripped and \R~removed.
    ## FIXME: uses a private function from tools
    if (is.na(val))
      return(character(0))
    val <- names(tools:::.split_dependencies(val))
    if (is.null(val))
      return(character(0))
    val <- val[! val %in% "R"]
    if (length(val))
      return(val)
    return(character(0))
}

