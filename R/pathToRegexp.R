#' Build a regular expression for matching strings against paths.
#'
#' @param path A character vector, TokenData, or
#' an array of strings and TokenData objects.
#' @export
pathToRegexp <- function(
  path,
  end = TRUE,
  sensitive = FALSE,
  trailing = TRUE,
  delimiter = "/"
) {
  # --------------------------------------------------------
  keys <- character(0)
  flags <- if (sensitive) "" else "(?i)"
  sources <- list()

  # so that it doesn't grow each time this function is called!
  environment(pathsToList)$init <- NULL
  for (input in pathsToList(path)) {
    data <- if (isTokenData(input)) input else parse(input)
    flatten()
  }
}

#' Convert a character vector, tokenData or a combination of both into a flat list.
#'
#' @examples
#'
#' # Character vector
#' path <- list(
#'  c("/text/:param", "/text/:param2"),
#'    "/text/:param3"
#' )
#'
#' pathsToList(path)
#'
#' # tokenData
#' path <- buildTokenData(
#'   list(
#'     list(
#'       type = "text",
#'       value = "/text/"
#'     ),
#'     list(
#'       type = "param",
#'       name = "id"
#'     )
#'   )
#' )
#'
#' pathsToList(path)
#'
#' # Combination
#' path <- list(
#'   "/text/:param",
#'   buildTokenData(
#'     list(
#'       list(
#'         type = "text",
#'         value = "/value"
#'       )
#'     )
#'   ),
#'   "/text/:param2"
#' )
#'
#' pathsToList(path)
#'
#' @noRd
#' @keywords internal
pathsToList <- local({
  init <- list()
  function(path) {
    if (inherits(path, "list") || (is.character(path) && length(path) > 1)) {
      for (p in path) {
        pathsToList(p)
      }
    } else {
      init <<- append(init, list(path))
    }
    return(init)
  }
})

#' @noRd
#' @keywords internal
flatten <- function() {}
