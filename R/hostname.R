the$hostname <- "https://incubating.orkg.org/"
#' Title
#'
#' @return
#' @export
#'
#' @examples
show_hostname <- function() {
  return(the$hostname)
}
#' Title
#'
#' @param hostname
#'
#' @return
#' @export
#'
#' @examples
change_hostname <- function(hostname) {
  the$hostname <- hostname
  return(hostname)
}

#' Title
#'
#' @param ... - one or more arguments to be concatenated
#' representing the route of the URL
#'
#' @return
#'
#'
#' @examples
with_host <- function(...) {
  return(paste(the$hostname, ..., sep = ""))
}
