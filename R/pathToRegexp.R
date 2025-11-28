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
  sources <- character(0)

  # so that it doesn't grow each time this function is called!
  environment(pathsToList)$init <- NULL
  for (input in pathsToList(path)) {
    data <- if (isTokenData(input)) input else parse(input)
    flatten()
  }
}

#' Convert a character vector, tokenData or a combination of
#' both into a flat list
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

#' Generate a flat list of sequence tokens from the given tokens
#'
#' @examples
#' # "normal" tokenData
#' builded <- buildTokenData(
#'   list(
#'     list(
#'       type = "text",
#'       value = "/users/"
#'     ),
#'     list(
#'       type = "param",
#'       name = "id"
#'     )
#'   )
#' )
#' flatten(builded)
#' # token of type group
#' builded <- buildTokenData(
#'  list(
#'    list(
#'      type = "text",
#'      value = "/users"
#'    ),
#'    list(
#'      type = "group",
#'      tokens = buildTokenData(
#'        list(
#'          list(
#'            type = "text",
#'            value = "/"
#'          ),
#'          list(
#'            type = "param",
#'            name = "id"
#'          )
#'        )
#'      )
#'    )
#'  )
#')
#' flatten(builded)
#'
#' @noRd
#' @keywords internal
#' @details
#' Original implementation use "generators".
#'
#'
flatten <- function(tokens, index = 1, init = list()) {
  if (index > length(tokens)) {
    return(init)
  }

  token <- tokens[[index]]

  if (token$type == "group") {
    result <- list()
    l <- flatten(token$tokens, init = init)
    init <- append(init, list(l))
  } else {
    init <- append(init, list(token))
  }
  flatten(tokens, index + 1, init)
}
