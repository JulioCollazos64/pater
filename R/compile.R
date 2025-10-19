#' Build a function that transform parameters into a valid path
#'
#' @param path A character vector of length 1.
#' @export
compile <- function(path, delimiter = "/", encode = URLencode) {
  data <- pater::parse(path)
  tokens <- data
  fn <- tokensToFunction(tokens)

  f <- function(params) {
    result <- fn(params)
    missing <- result[-1]
    if (length(missing)) {
      stop("Missing parameters: ", toString(missing))
    }
    result
  }
  invisible(f)
}

tokensToFunction <- function(tokens, delimiter, encode) {
  encoders <- Map(tokenToFunction, tokens)
  encoders

  function(data) {
    result <- ""

    for (encoder in encoders) {
      value <- encoder(data)
      missing <- value[-1]
      result[1] <- paste0(result[1], value[1])
      result <- c(result, missing)
    }
    result
  }
}

#' Convert a single token into a path building function
#'
#' @returns A function
tokenToFunction <- function(token, delimiter, encode) {
  type <- token$type
  data <- switch(
    type,
    "text" = function(...) {
      token$value
    },
    "group" = {
      fn <- tokensToFunction(
        tokens = token$tokens
      )

      function(data) {
        result <- fn(data)
        missing <- result[-1]
        value <- result[1]
        if (length(missing)) {
          return("")
        }
        value
      }
    },
    "wildcard" = {
      function(data) {
        value <- data[[token$name]]
        if (is.null(value)) {
          result <- c("", token$name)
          return(result)
        }

        if (is.list(value) && !length(value)) {
          stop(
            "Expected `",
            token$name,
            "` to be a non-empty list",
            call. = FALSE
          )
        }

        result <- vapply(value, URLencode, FUN.VALUE = character(1))
        result <- paste(result, collapse = "/")
        result
      }
    },
    "param" = function(data) {
      value <- data[[token$name]]

      if (is.null(value)) {
        result <- c("", token$name)
        return(result)
      }

      if (!is.character(value)) {
        stop("Expected ", token$name, " to be a string")
      }

      URLencode(value)
    }
  )
  data
}
