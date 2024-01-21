#' Extractor function for the ORKG templates
#'
#' @param template_id
#'
#' @return JSON string
#' @export
#'
#' @examples extractor_orkg("R937648")
extractor_orkg <- function(template_id) {
  orkg_str <- "https://incubating.orkg.org/"
  extract_all <- list()
  extractor_function <- function(template_id) {
    path <-
      paste(orkg_str, '/api/templates/', template_id, sep = '')
    req <- httr2::request(path)
    resp <- httr2::req_perform(req)
    info <- httr2::resp_body_json(resp)
    template_name <- format_string(info$label)
    template_class <- info$target_class
    templ_df <- data.frame(template_name, template_class)
    i <- 0
    all_comps <- data.frame(
      predicate_id = character(),
      predicate_label = character(),
      value_class_id = character(),
      stringsAsFactors = F
    )
    for (component in info$properties) {
      predicate_id <- component$path$id
      predicate_label <- component$path$label
      if (is.null(component$class$id)) {
        value_class_id <- component$datatype$id
      } else{
        value_class_id <- component$class$id
        if (startsWith(value_class_id, "C") == T) {
          path_n <-
            paste(orkg_str,
                  '/api/templates/?target_class=',
                  value_class_id,
                  sep = '')
          req_n <- httr2::request(path_n)
          resp_n <- httr2::req_perform(req_n)
          info_n <- httr2::resp_body_json(resp_n)
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
      all_comps[i,] <- comp_list
      all_comps <- all_comps[order(all_comps$predicate_id), ]
    }
    extracted <-
      list(templ_df, all_comps)
    extract_all[[template_name]] <<- list(extracted)
    return(extract_all)
  }
  extractor_function(template_id)
  json_extract_all <-
    jsonlite::toJSON(extract_all, pretty = T, auto_unbox = T)
  return(json_extract_all)
}
