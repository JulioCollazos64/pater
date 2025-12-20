#' Build a function for matching pathnames against a pathname specification
#'
#' @param path A character vector of length 1. A pathname specification.
#' @param decode A function for decoding a string or FALSE to disable it.
#' Defaults to `utils::URLdecode`
#' @param delimiter A character vector of length 1. Specifies the delimiter for the path segments.
#' @param ... Additional parameters for `pathToRegexp` or `parse`.
#' @return A function, invisibly.
#' @examples
#'
#' path <- "/users/:userId/books/:bookId/*public"
#' fn <- match(path)
#' p <- fn("/users/User1/books/Id1/2/3")
#' p
#'
#' path <- "/path/resource"
#' fn <- match(path)
#' fn("/resource/path")
#'
#'
#' @export
match <- function(path, decode = utils::URLdecode, delimiter = "/", ...) {
  patter <- pathToRegexp(path, delimiter = delimiter, ...)
  regex <- patter$pattern
  keys <- patter$keys

  f <- function(key) {
    if (isFALSE(decode)) {
      decode <- identity
      return(decode)
    }

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

  invisible(
    function(input) {
      m <- extract(input, regex)
      if (!length(m)) {
        return(FALSE)
      }
      path <- m[1]
      params <- list()
      i <- 2
      while (i <= length(m)) {
        if (identical(m[i], "")) {
          i <- i + 1
          next
        }

        key <- keys[[i - 1]]
        decoder <- decoders[[i - 1]]

        params[[key$name]] <- unlist(decoder(m[i]), use.names = FALSE)
        i <- i + 1
      }
      list(
        path = path,
        params = params
      )
    }
  )
}
