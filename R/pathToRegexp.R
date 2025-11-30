#' Build a regular expression for matching strings against pathnames
#'
#' @param path A character vector, TokenData, or
#' a list of strings and TokenData objects.
#' @param end A logical vector of length 1. Whether to add a construct to the
#' regular expression to check for a complete end of string match. Defaults to TRUE.
#' @param sensitive A logical vector of length 1. Whether resulting regex
#' will be case sensitive. Defaults to FALSE.
#' @param delimiter A character vector of length 1. Specifies the delimiter
#' for the path segments. Defaults to "/"
#' @param trailing A logical vector of length 1. Whether or not match
#' trailing path. Defaults to TRUE.
#'
#' @return A list with two elements: a regular expression and a list of keys.
#' @examples
#' path <- "/hello/world"
#' regex <- pathToRegexp(path)
#' grepl(regex,"/hello/world", perl = TRUE)
#' grepl(regex,"/hello/world/", perl = TRUE)
#'
#' path <- "/hello/:world"
#' regex <- pathToRegexp(path)
#' grepl(regex, "/hello/world", perl = TRUE)
#' grepl(regex, "/hello/path", perl = TRUE)
#'
#'
#' # Taken from https://expressjs.com/en/guide/routing.html
#' path <- "/flights/:from-:to"
#' regex <- pathToRegexp(path)
#' grepl(regex, "/flights/a-b", perl = TRUE)
#' grepl(regex, "/flights/a-b/", perl = TRUE)
#'
#' # Taken from https://expressjs.com/en/guide/routing.html
#' path <- "/users/:userId/books/:bookId"
#' regex <- pathToRegexp(path)
#' grepl(regex, "/users/1/books/2", perl = TRUE)
#' grepl(regex, "/users/1/books/2/", perl = TRUE)
#'
#' path <- "/plantae/:genus.:species"
#' regex <- pathToRegexp(path)
#' grepl(regex, "/plantae/a.b", perl = TRUE)
#' grepl(regex, "/plantae/a.b/", perl = TRUE)
#'
#' # Will match any route that starts with "/public/"
#' path <- "/public/*files"
#' regex <- pathToRegexp(path)
#' grepl(regex,"/public/format1", perl = TRUE)
#' grepl(regex,"/public/format2/format3", perl = TRUE)
#'
#' # trailing
#' path <- "/user/:userId"
#' regex <- pathToRegexp(path, trailing = FALSE)
#' grepl(regex, "/user/1", perl = TRUE) # TRUE
#' grepl(regex, "/users/1/", perl = TRUE) # FALSE
#'
#' # sensitive
#' path <- "/user"
#' regex <- pathToRegexp(path, sensitive = TRUE)
#' grepl(regex, "/user", perl = TRUE) # TRUE
#' grepl(regex, "/USER", perl = TRUE) # FALSE
#'
#' # end
#' path <- "/users"
#' regex1 <- pathToRegexp(path, trailing = FALSE, end = FALSE)
#' regex2 <- pathToRegexp(path, trailing = FALSE, end = TRUE)
#' if(require("stringr")){
#'   str_extract("/users////", regex1) # "/users"
#'   str_extract("/users////", regex2) # NA
#' }
#'
#' @export
pathToRegexp <- function(
  path,
  end = TRUE,
  sensitive = FALSE,
  trailing = TRUE,
  delimiter = "/"
) {
  # --------------------------------------------------------
  keys <- vector(mode = "list")
  flags <- if (sensitive) "" else "(?i)"
  sources <- character(0)

  # so that it doesn't grow each time this function is called!
  environment(pathsToList)$init <- NULL
  for (input in pathsToList(path)) {
    data <- if (isTokenData(input)) input else parse(input)
    for (tokens in flatten(data)) {
      browser()
      new <- toRegExpSource(tokens, delimiter, keys)
      sources <- c(sources, new$sources)
      keys <- new$keys
    }
  }
  pattern <- sprintf("^(?:%s)", paste0(sources, collapse = "|"))
  if (trailing) {
    pattern <- paste0(pattern, "(?:", escape(delimiter), "$)?")
  }

  finally <- if (end) "$" else paste0("(?=", escape(delimiter), "|$)")

  pattern <- paste0(flags, pattern, finally)
  list(pattern = pattern, keys = keys)
}


#' Transform a flat sequence of tokens into a regular expression
#'
#' @examples
#'
#' tokens <- parse("/users/")
#' toRegExpSource(tokens)
#' # parameter
#' tokens <- parse("/users/:id")
#' toRegExpSource(tokens)
#'
#' @noRd
#' @keywords internal
toRegExpSource <- function(
  tokens,
  delimiter,
  keys
  # , originalPath # (not implemented)
) {
  result <- ""
  backtrack <- ""
  isSafeSegmentParam <- TRUE
  for (token in tokens) {
    if (token$type == "text") {
      result <- paste0(result, escape(token$value))
      backtrack <- paste0(backtrack, token$value)
      if (isFALSE(isSafeSegmentParam)) {
        isSafeSegmentParam <- grepl(delimiter, token$value, perl = TRUE)
      }
      next
    }

    if (token$type %in% c("param", "wildcard")) {
      # TODO: if (!isSafeSegmentParam && !backtrack) {}

      if (token$type == "param") {
        result <- paste0(
          result,
          "(",
          negate(
            delimiter,
            if (isSafeSegmentParam) "" else backtrack
          ),
          "+)"
        )
      } else {
        result <- paste0(result, "([\\s\\S]+)")
      }

      keys <- append(keys, list(token))
      # keys <- c(keys, token)
      backtrack <- ""
      isSafeSegmentParam <- FALSE

      next
    }
  }

  result <- list(sources = result, keys = keys)
  result
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
#' path <- "/users/:id"
#' tokens <- parse(path)
#' flatten(tokens)
#'
#' path <- "/users{/:id}"
#' tokens <- parse(path)
#' flatten(tokens)
#'
#' @noRd
#' @keywords internal
#' @details
#' Original implementation use "generators".
flatten <- function(tokens, index = 1, init = list()) {
  if (index > length(tokens)) {
    return(list(init))
  }

  token <- tokens[[index]]

  if (token$type == "group") {
    results <- list()

    for (seq in flatten(token$tokens, 1, init)) {
      results <- c(results, flatten(tokens, index + 1, seq))
    }

    results <- c(results, flatten(tokens, index + 1, init))
    return(results)
  }

  newInit <- c(init, list(token))
  return(flatten(tokens, index + 1, newInit))
}
