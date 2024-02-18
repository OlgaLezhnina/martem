#' Request the ORKG REST API
#' @param route The part of path for requesting the ORKG REST API
#'which follows the hostname
#' @return Requested information about an ORKG object
#'
request_orkg <- function(route) {
  path <- with_host(route)
  req <- httr2::request(path)
  resp <- httr2::req_perform(req)
  info <- httr2::resp_body_json(resp)
  return(info)
}
