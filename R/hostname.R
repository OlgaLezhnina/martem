the$hostname <- "https://incubating.orkg.org/"

#' Get the hostname
#'@return The hostname
#' @export
#'@examples show_hostname()
#'
show_hostname <- function() {
  return(the$hostname)
}

#' Change the hostname
#'@param hostname A new hostname, a string starting with "https://"
#'@return The new hostname
#' @export
#'@examples change_hostname("https://incubating.orkg.org/")
#'
change_hostname <- function(hostname) {
  the$hostname <- hostname
  return(hostname)
}

#' Concatenates a hostname with other parameters
#' @param ... - one or more arguments to be concatenated
#' representing the route of the URL
#' @return A concatenated URL
#'
with_host <- function(...) {
  return(paste(the$hostname, ..., sep = ""))
}
