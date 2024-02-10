loaded_templates <- new.env()

#' Set a reference class(es) for a template and its components
#'
#' @param json_string
#'
#' @return reference class(es)
#' @export
#'
#' @examples
set_class <- function(json_string) {
  the$templ_info <- jsonlite::fromJSON(txt = json_string)
  result <- list()
  for (t in 1:length(the$templ_info)) {
    templ_data <- the$templ_info[[t]][[1]]
    class_t <-
      paste(
        "methods::setRefClass('",
        format_string(templ_data[[1]]$template_name),
        "',
      fields = list(
      label = 'character',
      template_name = 'character',
      template_class = 'character',
      components = 'list'",
        paste(sprintf(
          ",\n%s = 'ANY'",
          format_string(templ_data[[2]]$predicate_label)
        ), collapse = ""),
        " ),
      methods = list(initialize = function(...,
      template_name = '",
        format_string(templ_data[[1]]$template_name),
        "',
      template_class = '",
        templ_data[[1]]$template_class,
        "'",
        paste(sprintf(
          ",\n%s = 'none'",
          format_string(templ_data[[2]]$predicate_label)
        ), collapse = ""),
        "){
      callSuper(...,
      label = label,
      template_name = template_name
      ",
        paste(
          sprintf(',\n%1$s = %1$s',
                  format_string(templ_data[[2]]$predicate_label)),
          collapse = ''
        ),
        ")}
      ), where = loaded_templates)",
        sep = ""
      )
    result[[format_string(templ_data[[1]]$template_name)]] <-
      eval(parse(text = class_t))
  }
  return(result)
}
