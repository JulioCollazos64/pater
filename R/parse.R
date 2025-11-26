#' Decompose a pathname
#'
#' @details
#'
#' This function decomposes a given pathname into tokens.
#'
#' @param path A character vector of length 1.
#' @param encodePath A function to encode characters, defaults to `URLencode`.
#' @returns An object of class `tokenData`.
#' @export
parse <- function(path, encodePath = utils::URLencode) {
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

    stopifnot("Not enough data!" = !is.na(char[index]))

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
