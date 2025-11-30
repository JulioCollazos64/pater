#' Build a function for matching pathnames against a pathname specification
#'
#' @param path A character vector of length 1. A pathname specification.
#' @param decode A function for decoding a string or FALSE to disable it.
#' @param delimiter A character vector of length 1. Specifies the delimiter for the path segments.
#' @param ... Additional paramters for `pathToRegexp`.
#' @return A function.
#' @examples
#'
#' path <- "/users/:userId/books/:bookId/*public"
#' fn <- match(path)
#' p <- fn("/users/User1/books/Id1/2/3")
#' p
#'
#' @export
match <- function(path, decode, delimiter = "/", ...) {
  if (missing(decode)) {
    decode <- function(url) {
      utils::URLdecode(url)
    }
  }
  if (isFALSE(decode)) {
    decode <- identity
  }

  patter <- pathToRegexp(path, delimiter = delimiter, ...)
  regex <- patter$pattern
  keys <- patter$keys

  f <- function(key) {
    if (key$type == "param") {
      return(decode)
    }

    function(value) {
      Map(
        decode,
        strsplit(value, delimiter)[[1]]
      )
    }
  }
  decoders <- lapply(keys, f)
  function(input) {
    m <- extract(input, regex)
    if (!length(m)) {
      return(FALSE)
    }
    path <- m[1]
    params <- list()
    i <- 1
    while (i < length(m)) {
      if (identical(m[i], "")) {
        i <- i + 1
        next
      }

      key <- keys[[i]]
      decoder <- decoders[[i]]

      params[[key$name]] <- unlist(decoder(m[i + 1]), use.names = FALSE)
      i <- i + 1
    }
    list(
      path = path,
      params = params
    )
  }
}
