#' From `tokenData` object to a character vector
#'
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
          stringifyTokens(token),
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
      stop("Unknow token type", call. = FALSE)
    )
  }
  value
}
