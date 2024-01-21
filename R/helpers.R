#' Format a string
#'
#' @param text
#'
#' @return text
#' @export
#'
#' @examples format_string("Ab cd")
format_string = function(text) {
  return(stringr::str_replace_all(
    stringr::str_replace_all(tolower(text), "[ -]", "_"),
    "[^a-zA-Z0-9_]",
    ""
  ))
}
