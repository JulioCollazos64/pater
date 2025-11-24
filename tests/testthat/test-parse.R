test_that("Parse works", {
  path <- "/"
  parsed <- parse(path)

  expect_identical(
    parsed,
    list(
      list(
        type = "text",
        value = "/"
      )
    )
  )

  path <- "/:test"
  parsed <- parse(path)

  expect_identical(
    parsed,
    list(
      list(
        type = "text",
        value = "/"
      ),
      list(
        type = "param",
        name = "test"
      )
    )
  )

  path <- "/:a:b"
  parsed <- parse(path)

  expect_identical(
    parsed,
    list(
      list(
        type = "text",
        value = "/"
      ),
      list(
        type = "param",
        name = "a"
      ),
      list(
        type = "param",
        name = "b"
      )
    )
  )

  # Not valid parameter name in R, but quoted

  path <- '/:".1"'
  parsed <- parse(path)

  expect_identical(
    parsed,
    list(
      list(
        type = "text",
        value = "/"
      ),
      list(
        type = "param",
        name = ".1"
      )
    )
  )

  path <- "/:café"
  parsed <- parse(path)

  expect_identical(
    parsed,
    list(
      list(
        type = "text",
        value = "/"
      ),
      list(
        type = "param",
        name = "café"
      )
    )
  )

  path <- '/:"t123"'
  parsed <- parse(path)

  expect_identical(
    parsed,
    list(
      list(
        type = "text",
        value = "/"
      ),
      list(
        type = "param",
        name = "t123"
      )
    )
  )

  path <- "/*path"
  parsed <- parse(path)

  expect_identical(
    parsed,
    list(
      list(
        type = "text",
        value = "/"
      ),
      list(
        type = "wildcard",
        name = "path"
      )
    )
  )

  path <- '/:"test"stuff'
  parsed <- parse(path)

  expect_identical(
    parsed,
    list(
      list(
        type = "text",
        value = "/"
      ),
      list(
        type = "param",
        name = "test"
      ),
      list(
        type = "text",
        value = "stuff"
      )
    )
  )

  path <- '/:"1\\"2\\"3"'
  parsed <- parse(path)

  expect_identical(
    parsed,
    list(
      list(
        type = "text",
        value = "/"
      ),
      list(
        type = "param",
        name = '1"2"3'
      )
    )
  )

  # Groups
  path <- '{/:id}'
  parsed <- parse(path)

  expect_identical(
    parsed,
    list(
      list(
        type = "group",
        tokens = list(
          list(
            type = "text",
            value = "/"
          ),
          list(
            type = "param",
            name = "id"
          )
        )
      )
    )
  )

  # A consolidated URL

  path <- "/file/:filename{.:ext}"
  parsed <- parse(path)

  expect_identical(
    parsed,
    list(
      list(
        type = "text",
        value = "/file/"
      ),
      list(
        type = "param",
        name = "filename"
      ),
      list(
        type = "group",
        tokens = list(
          list(
            type = "text",
            value = "."
          ),
          list(
            type = "param",
            name = "ext"
          )
        )
      )
    )
  )

  # See: https://github.com/pillarjs/path-to-regexp/pull/390
  # path <- '\\\\:test'
  # parsed <- parse(path)

  # expect_identical(
  #   parsed,
  #   list(
  #     list(
  #       type = "text",
  #       value = "\\\\" # Two backlashes
  #     ),
  #     list(
  #       type = "param",
  #       name = "test"
  #     )
  #   )
  # )

  # Slug
  path <- "/book/holy-book"
  parsed <- parse(path)

  expect_identical(
    parsed,
    list(
      list(
        type = "text",
        value = "/book/holy-book"
      )
    )
  )
})

test_that("Parse errors", {
  # Unterminated quote

  path <- '/:"foo'

  expect_error(
    parse(path),
    regexp = "Unterminated quote"
  )

  # Missing parameter name

  path <- ':""test'

  expect_error(
    parse(path),
    regexp = "Missing parameter name"
  )

  # Invalid R name

  path <- "/:_"

  expect_error(
    parse(path)
  )
})
