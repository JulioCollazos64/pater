#' From `tokenData` object to a character vector
#'
#' The inverse of the [parse()] function.
#' @param tokens An object of class `tokenData`
#' @return A character vector of length 1.
#' @export
stringifyTokens <- function(tokens) {
  stopifnot(isTokenData(tokens))
  value <- ""
  i <- 1

  name <- function(value, nextToken) {
    isSafe <- isNameSafe(value) && isNextNameSafe(nextToken)
    if (isSafe) {
      return(value)
    }

    paste0("\"", value, "\"")
  }

  while (i <= length(tokens)) {
    token <- tokens[[i]]
    type <- token$type

    switch(
      type,
      "text" = {
        value <- paste0(value, escapeText(token$value))
        i <- i + 1
      },
      "group" = {
        value <- paste0(
          value,
          "{",
          stringifyTokens(token$tokens),
          "}"
        )
        i <- i + 1
      },
      "param" = {
        i <- i + 1
        value <- paste0(value, ":", name(token$name, nextToken = tokens[i]))
      },
      "wildcard" = {
        i <- i + 1
        value <- paste0(value, "*", name(token$name, nextToken = tokens[i]))
      },
      stop("Unknown token type", call. = FALSE)
    )
  }
  value
}

#' Validate name
#'
#' @noRd
#' @keywords internal
#' @param name A possible R name
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
