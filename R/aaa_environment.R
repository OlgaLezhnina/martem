the <- new.env(parent = emptyenv())
the$hostname <- "https://incubating.orkg.org/"
#' Title
#'
#' @return
#' @export
#'
#' @examples
get_hostname <- function() {
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
set_hostname <- function(hostname) {
  the$hostname <- hostname
  return(hostname)
}
