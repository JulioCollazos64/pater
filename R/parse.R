parse <- function(str, encodePath = NA) {
  browser()
  chars <- strsplit(str, "")[[1]]
  len <- length(chars)
  tokens <- list()
  name <- function() {
    browser()
    value <- ""
    while (i <= len) {
      if (chars[i] == '"') {
        quoteStart <- i
        i <<- i + 1
        while (i <= len) {
          if (chars[i] == '"') {
            i <<- i + 1
            quoteStart <- 0
            break
          }
          value <- paste0(value, chars[i])
          i <<- i + 1
        }
      }
      i <<- i + 1
    }
    if (identical(value, "")) {
      stop("Missing parameter name at index ", i, call. = FALSE)
    }
    value
  }
  i <- 1
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
        l <- list(
          type = "param",
          index = i,
          value = name()
        )
        tokens <- append(tokens, list(l))
        i <- i + 1
      },
      "*" = {
        l <- list(
          type = "wildcard",
          index = i,
          value = name()
        )
        tokens <- append(tokens, list(l))
        i <- i + 1
      },
      "\\" = {
        l <- list(
          type = "escape",
          index = i,
          value = value
        )
        tokens <- append(tokens, list(l))
        i <- i + 1
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
      type = "end",
      index = len + 1,
      value = ""
    )
  )
  tokens
}
# For the time being, let's just think we have one **segment**

x <- '/:"hola "'
# {
#   path: '/:"0"',
#   expected: new TokenData(
#     [
#       { type: "text", value: "/" },
#       { type: "param", name: "0" },
#     ],
#     '/:"0"',
#   ),
# }
x

parse <- function(x) {
  # browser()
  chars <- strsplit(x, "")[[1]]
  stopifnot(nchar(x) == length(chars))
  len <- nchar(x)
  tokens <- vector(mode = "list")

  name <- function(index, char, len) {
    # browser()
    index <- index + 1
    value <- ""
    result <- ""

    stopifnot("Not enough data!" = !is.na(char[index]))

    if (!identical(char[index], '"')) {
      for (j in index:len) {
        if (identical(char[j], ":")) {
          break
        }
        value <- paste0(value, char[j])
      }
      result <- list(
        index = list(
          start = index,
          end = j
        ),
        value = value
      )
    } else if (identical(char[index], '"')) {
      quoteStart <- index

      for (j in (quoteStart + 1):len) {
        if (identical(char[j], '"')) {
          break
        }
        value <- paste0(value, char[j])
      }

      result <- list(
        index = list(
          start = index,
          end = j
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
        calc <- name(index = i, chars, len)
        l <- list(
          type = "param",
          index = calc$index$start,
          value = calc$value
        )
        tokens <- append(tokens, list(l))
        i <- calc$index$end + 1
      },
      "*" = {
        calc <- name(index = i, chars, len)
        l <- list(
          type = "wildcard",
          index = calc$index$start,
          value = calc$value
        )
        tokens <- append(tokens, list(l))
        i <- calc$index$end + 1
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
        # Job done, there's no more char or escape character
        l <- list(
          type = "text",
          value = path
        )
        output <- append(output, l)

        next
      }

      if (token$type %in% c("param", "wildcard")) {
        # These ones were already preprocessed
        l <- list(
          type = token$type,
          name = token$value
        )
        # pos <- pos + 1
        output <- append(output, l)
      }
    }

    output
  }
}

# I've just tested on quotes
x <- '/:""'
x <- '/:"hola "' #
x <- '/:"test"stuff' # Work's correctly, the indexes are right.
x <- '/:"0"'

# Here' are the "usual" cases
x <- "/:test"
x <- "/:a:b"
x <- "/:foo"
# Corner cases
x <- ":"

x <- "/*path"
parse(x)
system.time(parse(x))
bench::mark(
  u = parse(x),
  iterations = 500,
  time_unit = "s"
)
