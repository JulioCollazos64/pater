#' Build a function for transforming parameters into a valid path
#'
#' The function will have one parameter in which you can specify the parameters
#' @export
compile <- function(path, delimiter = "/", encode) {
  if (missing(encode)) {
    encode <- function(url) {
      utils::URLencode(url, reserved = TRUE)
    }
  }

  if (isFALSE(encode)) {
    encode <- identity
  }

  data <- switch(
    class(path),
    "tokenData" = path,
    "character" = parse(path)
  )
  fn <- tokensToFunction(data, delimiter, encode)

  f <- function(params) {
    # browser()
    result <- fn(params)
    path <- result[1]
    missing <- result[-1]

    if (length(missing) > 0) {
      stop(
        "Missing parameters: ",
        toString(missing),
        call. = FALSE
      )
    }
    path
  }
}


#' @noRd
#' @keywords internal
tokensToFunction <- function(tokens, delimiter, encode) {
  encoders <- lapply(
    tokens,
    tokenToFunction,
    delimiter,
    encode
  )

  f <- function(data) {
    # browser()
    result <- ""

    for (encoder in encoders) {
      res <- encoder(data)
      value <- res[1]
      extras <- res[-1]
      result[1] <- paste0(result[1], value)
      result <- c(result, extras)
    }

    result
  }
  f
}

#' @noRd
#' @keywords internal
tokenToFunction <- function(token, delimiter, encode = identity) {
  if (token$type == "text") {
    f <- function(data = NULL) {
      token$value
    }
    return(f)
  }

  if (token$type == "group") {
    browser()
    fn <- tokensToFunction(token$tokens, delimiter, encode)

    f <- function(data) {
      res <- fn(data)
      value <- res[1]
      missing <- res[-1]
      if (!length(missing)) {
        return(value)
      }

      ""
    }

    return(f)
  }

  if (token$type == "wildcard") {
    f <- function(data) {
      # browser()
      value <- data[[token$name]]

      if (is.null(value)) {
        return(
          c("", token$name)
        )
      }

      if (!(is.character(value) && length(value) > 0)) {
        stop(
          "Expected ",
          token$name,
          " to be a non-empty character vector",
          call. = FALSE
        )
      }

      prev <- Map(
        f = function(val, index) {
          if (!(is.character(val) && length(val) == 1)) {
            stop(
              "Expected ",
              token$name,
              "/",
              index,
              " to be a character vector of length 1",
              call. = FALSE
            )
          }
          encode(val)
        },
        value,
        seq_along(value)
      )
      paste0(unlist(prev), collapse = delimiter)
    }
    return(f)
  }

  f <- function(data) {
    # browser()
    value <- data[[token$name]]

    if (is.null(value)) {
      return(
        c("", token$name)
      )
    }

    if (!(is.character(value) && length(value) == 1)) {
      stop(
        "Expected ",
        token$name,
        " to be a string of length 1",
        call. = FALSE
      )
    }
    encode(value)
  }

  return(f)
}
