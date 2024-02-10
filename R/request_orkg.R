#' Title
#'
#' @param route
#'
#' @return
#'
#'
#' @examples
request_orkg <- function(route) {
  path <- paste(the$hostname, route, sep = '')
  req <- httr2::request(path)
  resp <- httr2::req_perform(req)
  info <- httr2::resp_body_json(resp)
  return(info)
}
