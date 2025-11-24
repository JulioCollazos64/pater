#' Decompose a pathname
#'
#' @details
#'
#' This function decomposes a given pathname into tokens.
#'
#' @param x A character vector of length 1.
#' @returns A list with the pathname's tokens.
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
    quoted <- FALSE
    # see `make.names` docs
    regex_start <- "[a-zA-Z\\p{L}.]"
    regex_next <- "[a-zA-Z0-9\\p{L}._]"

    stopifnot("Not enough data!" = !is.na(char[index]))

    if (grepl(regex_start, char[index], perl = TRUE)) {
      repeat {
        value <- paste0(value, char[index])
        index <- index + 1

        if (!grepl(regex_next, char[index], perl = TRUE)) {
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
          end = j + 1 # Must return
        ),
        value = value
      )
    }

    if (!nzchar(result$value)) {
      stop("Missing parameter name at index ", index, call. = FALSE)
    }

    if (!is_name_safe(result$value) && !quoted) {
      stop(
        "The parameter name '",
        result$value,
        "' is not a valid one. ",
        "Did you forget to quote it?",
        call. = FALSE
      )
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
          value = chars[i]
        )
        tokens <- append(tokens, list(l))
        i <- i + 1
      },
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

  output <- list()
  consumeUntil <- function(endType, tokens, pos) {
    repeat {
      # browser()
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
      }
    }

    list(output, pos)
  }
  consumeUntil("end", tokens, j)[[1]]
}
