#' Decompose a pathname
#'
#' This function decomposes a given pathname into meaningful tokens, see Details.
#'
#' @details
#'
#' The `parse` function returns a `tokenData` object which contains a list of tokens from the
#' path you provided. A token is a meaningful portion of a given pathname, it can contain
#' so called "parameters", which are placeholders that will be filled when a new HTTP
#' request comes in, these parameters may or not expand a whole path segment, meaning
#' that two parameters can share the same path segment, a token type can be any of
#' the following:
#'
#' \itemize{
#'
#'  \item{text: Represents a fixed path portion , e.g "/path/" or "/path/resource".}
#'  \item{param: Represent a dynamic path portion, e.g. "/path/:Id" or
#'  "/path/:from-:to". Must be named.}
#'  \item{wildcard: Similar to param but can "expand" itself into more than one path
#'  segment, e.g. "/public/*files". Must be named}
#'  \item{group: Represents an optional path portion, either an optional parameter
#'   or optional text, e.g. "/path\{/:optional\}" or "/user\{s\}".}
#' }
#'
#' `pater` uses this special syntax as some regex features are not available, e.g.
#' "/users?" doesn't work, this was done in the original implementation for security
#'  reasons. If you want to specify one of these in your path use double backlash,
#' "/users\\?"
#'
#'
#' @param path A character vector of length 1.
#' @param encodePath A function to encode characters, defaults to the `identity` function.
#' @returns An object of class `tokenData`.
#' @examples
#'
#' # A "fixed" path
#' path <- "/path/resource"
#' parse(path)
#'
#' # Parameters
#' path <- "/path/to/:resourceId"
#' parse(path)
#'
#' # Wildcard
#' path <- "/path/*files"
#' parse(path)
#'
#' # Group or "optional"
#' path <- "/path{s}"
#' parse(path)
#'
#' # Error because of regex feature not supported
#' \dontrun{
#'  path <- "/paths?"
#'  parse(path)
#' }
#'
#' # Escape it
#' path <- "/paths\\?"
#' parse(path)
#' @export
parse <- function(path, encodePath = identity) {
  chars <- strsplit(path, "")[[1]]
  stopifnot(
    "`path` must be of length 1" = length(path) == 1,
    "`encodePath` must be a function" = is.function(encodePath)
  )
  len <- nchar(path)
  tokens <- vector(mode = "list")

  name <- function(index, char, len) {
    value <- ""
    result <- ""
    quoted <- FALSE

    regex_start <- regexFirst()
    regex_next <- regexOthers()

    if (grepl(regex_start, char[index], perl = TRUE)) {
      start <- index
      while (grepl(regex_next, char[index], perl = TRUE)) {
        value <- paste0(value, char[index])
        index <- index + 1
      }

      result <- list(
        index = list(
          start = start,
          nextIter = index
        ),
        value = value
      )
    } else if (identical(char[index], '"')) {
      quoteStart <- index
      j <- quoteStart
      while (j <= len) {
        j <- j + 1
        if (identical(char[j], '"')) {
          quoteStart <- 0
          quoted <- TRUE
          break
        }
        if (identical(char[j], "\\")) {
          j <- j + 1
        }

        value <- paste0(value, char[j])
      }

      if (quoteStart) {
        stop("Unterminated quote at index ", quoteStart, call. = FALSE)
      }

      result <- list(
        index = list(
          start = index,
          nextIter = j + 1
        ),
        value = value
      )
    }

    if (identical(result, "") || !nzchar(result$value)) {
      stop("Missing parameter name at index ", index, call. = FALSE)
    }

    result
  }

  i <- 1 # index
  j <- 1 # pos

  while (i <= len) {
    value <- chars[i]
    type <- SIMPLE_TOKENS[value]

    if (!is.na(type)) {
      l <- list(
        type = type,
        index = i,
        value = value
      )
      tokens <- append(tokens, list(l))
      i <- i + 1
      next
    }

    switch(
      value,
      "\\" = {
        l <- list(
          type = "escape",
          index = i,
          value = chars[i + 1] # "catch" the next char
        )
        tokens <- append(tokens, list(l))
        i <- i + 2
      },
      ":" = {
        calc <- name(index = i + 1, chars, len)
        l <- list(
          type = "param",
          index = calc$index$start,
          value = calc$value
        )
        tokens <- append(tokens, list(l))
        i <- calc$index$nextIter
      },
      "*" = {
        calc <- name(index = i + 1, chars, len)
        l <- list(
          type = "wildcard",
          index = calc$index$start,
          value = calc$value
        )
        tokens <- append(tokens, list(l))
        i <- calc$index$nextIter
      },
      {
        l <- list(
          type = "char",
          index = i,
          value = value
        )
        tokens <- append(tokens, list(l))
        i <- i + 1
      }
    )
  }

  tokens <- append(
    tokens,
    list(
      list(type = "end", index = i, value = "")
    )
  )

  output <- list()
  consumeUntil <- function(endType, tokens, pos) {
    repeat {
      token <- tokens[[pos]]
      pos <- pos + 1

      if (token$type == endType) {
        break
      }

      if (token$type %in% c("char", "escape")) {
        path <- token$value
        cur <- tokens[[pos]]

        while (cur$type %in% c("char", "escape")) {
          path <- paste0(path, cur$value)
          pos <- pos + 1
          cur <- tokens[[pos]]
        }

        l <- list(
          type = "text",
          value = encodePath(path)
        )
        output <- append(output, list(l))

        next
      }

      if (token$type %in% c("param", "wildcard")) {
        # These ones were already preprocessed
        l <- list(
          type = token$type,
          name = token$value
        )

        output <- append(output, list(l))

        next
      }

      if (token$type == "{") {
        l <- consumeUntil(
          endType = "}",
          tokens = tokens,
          pos = pos
        )
        pos <- l[[2]]

        l <- list(
          type = "group",
          tokens = l[[1]]
        )

        output <- append(output, list(l))

        next
      }

      stop(
        "Unexpected ",
        token$type,
        " at index ",
        pos,
        " expected ",
        endType,
        call. = FALSE
      )
    }

    list(
      buildTokenData(output),
      pos
    )
  }
  result <- consumeUntil("end", tokens, j)[[1]]
  result
}

#' @noRd
#' @keywords internal
isTokenData <- function(x) {
  inherits(x, "tokenData")
}

#' Build tokenData object
#'
#' @noRd
#' @keywords internal
buildTokenData <- function(x) {
  structure(x, class = "tokenData")
}
