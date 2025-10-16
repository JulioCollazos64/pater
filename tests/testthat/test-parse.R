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

  path <- '/:"0"'
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
        name = "0"
      )
    )
  )

  path <- "/:_"
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
        name = "_"
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

  path <- '/:"123"'
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
        name = "123"
      )
    )
  )

  # Need to implement this

  # path <- '/:"1\\"\\2\\"3"'
  # parsed <- parse(path)

  # expect_identical(
  #   parsed,
  #   list(
  #     list(
  #       type = "text",
  #       value = "/"
  #     ),
  #     list(
  #       type = "param",
  #       value = '1"2"3'
  #     )
  #   )
  # )

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
})
