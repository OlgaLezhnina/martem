#' Title
#'
#' @param df
#'
#' @return
#'
#'
#' @examples
df_structure <- function(df) {
  result <- list()
  result[["@type"]] <- list(with_host("class/Table"))
  result[["label"]] <- "Table"
  index <- list()
  result[["columns"]] <- list()
  for (i in seq_len(ncol(df))) {
    column <- list()
    column[["@type"]] <- list(with_host("class/Column"))
    column[["titles"]] <- colnames(df)[i]
    column[["number"]] <- i
    column[["@id"]] <- paste("_:n", the$uid(), sep = "")
    index <- append(index, column[["@id"]])
    result[["columns"]] <-
      append(result[["columns"]], list(column))
  }
  result[["rows"]] <- list()
  for (i in seq_len(nrow(df))) {
    row <- list()
    row[["@type"]] <- list(with_host("class/Row"))
    row[["number"]] <- i
    row[["titles"]] <- rownames(df)[i]
    row[["cells"]] <- list()
    for (y in seq_len(ncol(df))) {
      cell <- list()
      cell[["@type"]] <- list(with_host("class/Cell"))
      if (!is.null(df[[y]][[i]])) {
        cell[["value"]] <- as.character(df[[y]][[i]])
      } else {
        cell["value"] <- list(NULL)
      }
      cell[["column"]] <- index[[y]]
      row[["cells"]] <- append(row[["cells"]], list(cell))
    }
    result[["rows"]] <- append(result[["rows"]], list(row))
  }
  result[["@id"]] <- paste("_:n", the$uid(), sep = "")
  return(result)
}

#' Turn an instance of a reference class into ORKG-harvestable JSON-LD
#'
#' @param instance
#'
#' @return JSON string
#' @export
#'
#' @examples
turn_json_orkg <- function(instance) {
  the$uid <- generate_uid()
  context <- list()
  context[["label"]] <- "http://www.w3.org/2000/01/rdf-schema#label"
  context[["number"]] <- with_host("property/CSVW_Number")
  context[["rows"]] <- with_host("property/CSVW_Rows")
  context[["cells"]] <- with_host("property/CSVW_Cells")
  context[["value"]] <- with_host("property/CSVW_Value")
  context[["column"]] <- with_host("property/CSVW_Column")
  context[["columns"]] <- with_host("property/CSVW_Columns")
  context[["titles"]] <- with_host("property/CSVW_Titles")
  write_info <- function(instance) {
    templ_schema <-
      the$templ_info[[instance$template_name]][[1]]
    field_list <-
      stringr::str_split(format_string(templ_schema[[2]]$predicate_label), " ")
    result <- list()
    result[["@id"]] <- paste("_:n", the$uid(), sep = "")
    result[["label"]] <- instance$label
    template_class <- templ_schema[[1]]$template_class
    result[["@type"]] <-
      list(paste(the$hostname, "class/", template_class, sep = ""))
    for (j in field_list) {
      if (length(instance$field(j)) == 1 &&
            is.character(instance$field(j)) &&
            (instance$field(j) == "none")) {
        next
      }
      class_id <-
        templ_schema[[2]]$value_class_id[format_string(templ_schema[[2]]$predicate_label) == j]
      if (startsWith(class_id, "C") == TRUE) {
        pred_id <-
          templ_schema[[2]]$predicate_id[format_string(templ_schema[[2]]$predicate_label) == j]
        if (length(instance$field(j)) == 1) {
          result[[pred_id]] <- write_info(instance$field(j))
        } else {
          result[[pred_id]] <- lapply(instance$field(j), write_info)
        }
        context[[pred_id]] <<-
          paste(the$hostname, "property/", pred_id, sep = "")
      } else {
        if (j %in% names(instance$initFields())) {
          pred_id <-
            templ_schema[[2]]$predicate_id[format_string(templ_schema[[2]]$predicate_label) == j]
          if (is.data.frame(instance$field(j))) {
            result[[pred_id]] <- df_structure(instance$field(j))
          } else {
            result[[pred_id]] <- list(instance$field(j))
          }
          context[[pred_id]] <<-
            paste(the$hostname, "property/", pred_id, sep = "")
        }
      }
    }
    return(result)
  }
  result <- write_info(instance)
  result[["@context"]] <- context
  inst_json <-
    jsonlite::toJSON(result, pretty = TRUE, auto_unbox = TRUE)
  return(inst_json)
}
