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
#' @param token An object of class `tokenData`.
#'
#' @noRd
#' @keywords internal
isNextNameSafe <- function(token) {
  regex_next <- regexOthers()
  token <- token[[1]]

  if (is.null(token)) {
    return(TRUE)
  }

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

#' Escape some regex keywords
#'
#' @noRd
#' @keywords internal
escapeText <- function(value) {
  gsub(
    pattern = "([{}()\\[\\]+?!:*])",
    replacement = "\\\\\\1",
    value,
    perl = TRUE
  )
}
