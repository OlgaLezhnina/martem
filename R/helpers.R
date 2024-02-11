#' Format a string
#'
#' @param text
#'
#' @return text
#'
#'
#'
format_string  <- function(text) {
  return(stringr::str_replace_all(
    stringr::str_replace_all(tolower(text), "[ -]", "_"),
    "[^a-zA-Z0-9_]",
    ""
  ))
}

#' Title
#'
#' @param ... - one or more arguments to be concatenated
#' representing the route of the URL
#'
#' @return
#'
#'
#' @examples
with_host <- function(...) {
  return(paste(the$hostname, ..., sep = ""))
}


#' Title
#'
#' @return
#'
#'
#' @examples
generate_uid <- function() {
  i <- 0
  return(function() {
    i <<- i + 1
    return(i)
  })
}
