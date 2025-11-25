#' @export
stringifyTokens <- function(tokens) {
  # browser()
  stopifnot(isTokenData(tokens))
  value <- ""
  i <- 1

  name <- function(value, nextToken) {
    isSafe <- isNameSafe(value) && isNextNameSafe(nextToken)
    if (isSafe) {
      return(value)
    }
  }

  while (i <= length(tokens)) {
    token <- tokens[[i]]
    type <- token$type
    i <- i + 1

    switch(
      type,
      "text" = {
        value <- paste0(value, escapeText(token$value))
        next
      },
      "group" = {
        value <- paste0(
          value,
          "{",
          stringifyTokens(token),
          "}"
        )
        next
      },
      "param" = {
        value <- paste0(value, ":", name(token$name, nextToken = tokens[[i]]))
        next
      },
      "wildcard" = {
        value <- paste0(value, "*", name(token$name, nextToken = tokens[[i]]))
        next
      },
      stop("Unknow token type", call. = FALSE)
    )
  }
  value
}
