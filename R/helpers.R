#' Format a string
#' @param text A character vector
#' @return A character vector formatted in the required way
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
#' @return The counting function
#'
generate_uid <- function() {
  i <- 0
  return(function() {
    i <<- i + 1
    return(i)
  })
}

#'Show the fields of a reference class for a template
#'which can be used to write an instance
#' @param ref_class A reference class based on a template
#' @return The string listing all fields for writing an instance
#' @export
#' @examples tp <- load_reference_classes("R937648")
#' show_fields(tp$measurement_scale)
#'
show_fields <- function(ref_class) {
  output <- list()
  all_fields <- ref_class$fields()
  written <- c("template_name",
               "template_class",
               "components")
  for (field in names(all_fields)) {
    if (!field %in% written) {
      output <- append(output, field)
    }
  }
  return(paste(output))
}
