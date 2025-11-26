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
