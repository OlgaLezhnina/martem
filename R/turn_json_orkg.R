#' Writes dataframes and tuples into JSON-LD
#' for the internal use
#' @param df A dataframe or a tuple
#' @param label A character string
#' @return JSON-LD of the dataframe of a tuple
#'
df_structure <- function(df, label) {
  result <- list()
  result[["@type"]] <- list(with_host("class/Table"))
  result[["label"]] <- label
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

#' Recognizes the length of input to apply or lapply a function
#' @param input A single element or a list
#' @param func A function to be applied to the input
#' @return The result of the function applied in the differentiated way
#'
differ_length <- function(input, func) {
  if (length(input) == 1) {
    output <- func(input)
  } else {
    output <- lapply(input, func)
  }
  return(output)
}

#' Reprocess the input to be used in JSON-LD depending on its type
#' @param input A dataframe, a tuple, or another basic data type
#' @return The resulting R object to be used in JSON-LD
#'
differ_type <- function(input) {
  if (methods::is(input, "data.frame")) {
    output <- df_structure(df = input, label = "Table")
  } else if (methods::is(input, "tuple")) {
    output <- df_structure(df = input[[1]], label = input[[2]])
  } else {
    output <- list(input)
  }
  return(output)
}

#' Turn an instance of a reference class into ORKG-harvestable JSON-LD
#' @param instance An instance of a reference class
#' @return JSON string in JSON-LD format
#' @export
#' @examples tp <- load_reference_classes("R937648")
#' my_instance <- tp$measurement_scale(label = "my_scale")
#' my_json <- turn_json_orkg(my_instance)
#'
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
    result[["@type"]] <-
      list(with_host("class/", templ_schema[[1]]$template_class))
    for (field_name in field_list) {
      if (length(instance$field(field_name)) == 1 &&
            is.character(instance$field(field_name)) &&
            (instance$field(field_name) == "none")) {
        next
      }
      written_label <- format_string(templ_schema[[2]]$predicate_label)
      nested_templ <-
        templ_schema[[2]]$nested_template[written_label == field_name]
      if (nested_templ) {
        pred_id <-
          templ_schema[[2]]$predicate_id[written_label == field_name]
        result[[pred_id]] <-
          differ_length(instance$field(field_name), write_info)
        context[[pred_id]] <<-
          with_host("property/", pred_id)
      } else {
        if (field_name %in% names(instance$initFields())) {
          pred_id <-
            templ_schema[[2]]$predicate_id[written_label == field_name]
          result[[pred_id]] <-
            differ_type(instance$field(field_name))
          context[[pred_id]] <<-
            with_host("property/", pred_id)
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
