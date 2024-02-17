#' Format a string
#'
#' @param text A character vector
#'
#' @return A character vector formatted in the required way
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

#' Generate a counting function
#' used for assigning unique identifiers
#'
#' @return The counting function
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
