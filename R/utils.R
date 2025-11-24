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
  regex_next <- "[a-zA-Z0-9\\p{L}._]"
  if (token$type == "text") {
    letter <- substr(token$value, 1, 1)
    return(!grepl(regex_next, letter, perl = TRUE))
  }
  TRUE
}
