#' Decompose a path
#'
#' @param x A character path of length 1.
#' @returns A list with the path's components.
#' @export
parse <- function(x) {
  chars <- strsplit(x, "")[[1]]
  stopifnot(
    "`path` must be of length 1" = length(x) == 1
  )
  len <- nchar(x)
  tokens <- vector(mode = "list")

  name <- function(index, char, len) {
    value <- ""
    result <- ""

    stopifnot("Not enough data!" = !is.na(char[index]))

    if (!identical(char[index], '"')) {
      repeat {
        value <- paste0(value, char[index])
        index <- index + 1

        if (identical(char[index], ":") || is.na(char[index])) {
          break
        }
      }
      result <- list(
        index = list(
          start = index, # solve this later
          end = index
        ),
        value = value
      )
    } else if (identical(char[index], '"')) {
      quoteStart <- index

      for (j in (quoteStart + 1):len) {
        if (identical(char[j], '"')) {
          quoteStart <- 0
          break
        }
        value <- paste0(value, char[j])
      }

      if (quoteStart) {
        stop("Unterminated quote at index ", quoteStart, call. = FALSE)
      }

      result <- list(
        index = list(
          start = index,
          end = j + 1 # Must return
        ),
        value = value
      )
    }

    if (!nzchar(result$value)) {
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
    }

    switch(
      value,
      ":" = {
        calc <- name(index = i + 1, chars, len)
        l <- list(
          type = "param",
          index = calc$index$start,
          value = calc$value
        )
        tokens <- append(tokens, list(l))
        i <- calc$index$end
      },
      "*" = {
        calc <- name(index = i + 1, chars, len)
        l <- list(
          type = "wildcard",
          index = calc$index$start,
          value = calc$value
        )
        tokens <- append(tokens, list(l))
        i <- calc$index$end
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
      list(type = "end", index = i + 1, value = "")
    )
  )

  consumeUntil <- function(endType, tokens, pos) {
    output <- list()

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
          value = path
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
        # pos <- pos + 1
        output <- append(output, list(l))
      }
    }

    output
  }
  consumeUntil("end", tokens, j)
}
