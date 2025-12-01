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
  "[a-zA-Z0-9\\p{L}_]"
}

#' Escape some regex keywords
#'
#' @noRd
#' @keywords internal
escapeText <- function(value) {
  gsub(
    pattern = "([{}()\\[\\]+?!:*\\\\])",
    replacement = "\\\\\\1",
    value,
    perl = TRUE
  )
}

#' Escape text
#'
#' @noRd
#' @keywords internal
escape <- function(value) {
  gsub(
    pattern = "([.+*?^${}()\\[\\]|/\\\\])",
    replacement = "\\\\\\1",
    value,
    perl = TRUE
  )
}

#' Block backtracking on previous text and ignore delimiter string
#'
#' @noRd
#' @keywords internal
negate <- function(delimiter, backtrack) {
  if (nchar(backtrack) < 3) {
    if (nchar(delimiter) < 3) {
      return(paste0("[^", escape(paste0(delimiter, backtrack)), "]"))
    }
    return(paste0("(?:(?!", escape(delimiter), ")[^", escape(backtrack), "])"))
  }

  if (nchar(delimiter) < 3) {
    return(
      paste0("(?:(?!", escape(backtrack), ")[^", escape(delimiter), "])")
    )
  }
  paste0("(?:(?!", escape(backtrack), "|", escape(delimiter), ")[\\s\\S])")
}

#' @noRd
#' @keywords internal
extract <- function(string, regex) {
  regmatches(
    x = string,
    m = regexec(
      regex,
      string,
      perl = TRUE
    )
  )[[1]]
}
