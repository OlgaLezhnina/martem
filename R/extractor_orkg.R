#' Extractor function for the ORKG templates
#'
#' @param template_id
#'
#' @return
#'
#' @examples
#'
extractor_orkg <- function(template_id) {
  extract_all <- list()
  extractor_function <- function(template_id) {
    info <-
      request_orkg(paste("/api/templates/", template_id, sep = ""))
    template_name <- format_string(info$label)
    template_class <- info$target_class
    templ_df <- data.frame(template_name, template_class)
    i <- 0
    all_comps <- data.frame(
      predicate_id = character(),
      predicate_label = character(),
      value_class_id = character(),
      stringsAsFactors = FALSE
    )
    for (component in info$properties) {
      predicate_id <- component$path$id
      predicate_label <- component$path$label
      if (is.null(component$class$id)) {
        value_class_id <- component$datatype$id
      } else {
        value_class_id <- component$class$id
        info_n <- request_orkg(paste(
          "/api/templates/?target_class=",
          value_class_id,
          sep = ""
        ))
        if (!length(info_n$content)) {
          next
        } else {
          nested_id <- info_n$content[[1]]$id
          nested_name <- info_n$content[[1]]$label
          if (!nested_name %in% names(extract_all)) {
            extractor_function(nested_id)
          }
        }
      }
      comp_list <- list(predicate_id,
                        predicate_label,
                        value_class_id)
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
