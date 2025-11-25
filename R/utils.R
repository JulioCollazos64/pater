#' Validate name
#'
#' @noRd
#' @keywords internal
#' @param name
isNameSafe <- function(name) {
  make.names(name) == name
}

#' Validate next token
#'
#' @param token token following a parameter token
#'
#' @noRd
#' @keywords internal
isNextNameSafe <- function(token) {
  regex_next <- regexOthers()
  if (token$type == "text") {
    letter <- substr(token$value, 1, 1)
    return(!grepl(regex_next, letter, perl = TRUE))
  }
  TRUE
}

#' Valid first character in R
#'
#' @noRd
#' @keywords internal
#' @seealso [make.names()] for official documentation.
#'
regexFirst <- function() {
  "[a-zA-Z\\p{L}.]"
}

#' Valid non-first characters in R
#'
#' @noRd
#' @keywords internal
#' @seealso [make.names()] for official documentation.
#'
regexOthers <- function() {
  "[a-zA-Z0-9\\p{L}._]"
}
