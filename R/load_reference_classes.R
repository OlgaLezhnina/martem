loaded_templates <- new.env()

#' Extractor function for the ORKG templates
#' @param template_id The ID of an ORKG template
#' @return An R object that contains information about the template
#' and its components for the internal use
#'
extractor_orkg <- function(template_id) {
  extract_all <- list()
  extractor_function <- function(template_id) {
    info <-
      request_orkg(paste("/api/templates/", template_id, sep = ""))
    template_name <- format_string(info$label)
    template_class <- info$target_class$id
    templ_df <- data.frame(template_name, template_class)
    i <- 0
    all_comps <- data.frame(
      predicate_id = character(),
      predicate_label = character(),
      value_class_id = character(),
      nested_template = logical(),
      stringsAsFactors = FALSE
    )
    for (component in info$properties) {
      predicate_id <- component$path$id
      predicate_label <- component$path$label
      if (is.null(component$class$id)) {
        value_class_id <- component$datatype$id
        nested_template <- FALSE
      } else {
        value_class_id <- component$class$id
        info_n <- request_orkg(paste(
          "/api/templates/?target_class=",
          value_class_id,
          sep = ""
        ))
        if (!length(info_n$content)) {
          nested_template <- FALSE
        } else {
          nested_template <- TRUE
          nested_id <- info_n$content[[1]]$id
          nested_name <- info_n$content[[1]]$label
          if (!nested_name %in% names(extract_all)) {
            extractor_function(nested_id)
          }
        }
      }
      comp_list <- list(predicate_id,
                        predicate_label,
                        value_class_id,
                        nested_template)
      i <- i + 1
      all_comps[i, ] <- comp_list
      all_comps <- all_comps[order(all_comps$predicate_id), ]
    }
    extracted <-
      list(templ_df, all_comps)
    extract_all[[template_name]] <<- list(extracted)
    return(extract_all)
  }
  extractor_function(template_id)
  return(extract_all)
}

#' Load reference classes for a template and its components
#' @param template_id The ID of an ORKG template
#' @return reference classes for the template and its components
#' @export
#' @examples tp <- load_reference_classes("R937648")
#'
load_reference_classes <- function(template_id) {
  the$templ_info <- extractor_orkg(template_id)
  result <- list()
  for (t in seq_along(the$templ_info)) {
    templ_data <- the$templ_info[[t]][[1]]
    reference_class_template <-
      paste(
        "methods::setRefClass('",
        paste(format_string(templ_data[[1]]$template_name), "_orkg", sep = ""),
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
          sprintf(",\n%1$s = %1$s",
                  format_string(templ_data[[2]]$predicate_label)),
          collapse = ""
        ),
        ")}
      ), where = loaded_templates)",
        sep = ""
      )
    result[[format_string(templ_data[[1]]$template_name)]] <-
      eval(parse(text = reference_class_template))
  }
  return(result)
}
