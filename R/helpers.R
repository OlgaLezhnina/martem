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
#' @param route
#'
#' @return
#'
#'
#' @examples
with_host <- function(route) {
  return(paste(the$hostname, route, sep = ""))
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
