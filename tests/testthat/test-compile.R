test_that("Compile works", {
  path <- "/"
  toPath <- compile(path)

  Map(
    f = function(input, expected) {
      path <- toPath(input)
      expect_identical(
        path,
        expected
      )
    },
    list(NULL, list(), list(id = "123")),
    list("/", "/", "/")
  )

  path <- "/test"
  toPath <- compile(path)

  Map(
    f = function(input, expected) {
      path <- toPath(input)
      expect_identical(
        path,
        expected
      )
    },
    list(NULL, list(), list(id = "123")),
    list("/test", "/test", "/test")
  )

  path <- "/test/"
  toPath <- compile(path)

  Map(
    f = function(input, expected) {
      path <- toPath(input)
      expect_identical(
        path,
        expected
      )
    },
    list(NULL, list(), list(id = "123")),
    list("/test/", "/test/", "/test/")
  )

  path <- '/:"0"'
  toPath <- compile(path)

  # not including any argument results in an error even in JS

  expect_identical(
    toPath(list("0" = "123")),
    "/123"
  )

  path <- "/:test"
  toPath <- compile(path)

  expect_identical(
    toPath(list(test = "123")),
    "/123"
  )

  expect_identical(
    toPath(list(test = "123/xyz")),
    "/123%2Fxyz"
  )

  path <- "/:test"
  toPath <- compile(path, encode = FALSE)

  expect_identical(
    toPath(list(test = "123")),
    "/123"
  )

  expect_identical(
    toPath(list(test = "123/xyz")),
    "/123/xyz"
  )

  path <- "/:test"
  toPath <- compile(path, encode = function(s) "static")

  expect_identical(
    toPath(list(test = "123")),
    "/static"
  )

  expect_identical(
    toPath(list(test = "123/xyz")),
    "/static"
  )

  path <- "{/:test}"
  toPath <- compile(path, encode = FALSE)

  expect_identical(
    toPath(list(test = NULL)),
    ""
  )

  expect_identical(
    toPath(list(test = "123")),
    "/123"
  )

  expect_identical(
    toPath(list(test = "123/xyz")),
    "/123/xyz"
  )

  path <- "/*test" # "one or more"
  toPath <- compile(path)

  expect_identical(
    toPath(list(test = "123")),
    "/123"
  )

  expect_identical(
    toPath(list(test = c("123", "xyz"))),
    "/123/xyz"
  )

  path <- "/*test"
  toPath <- compile(path, encode = FALSE)

  expect_identical(
    toPath(list(test = "123")),
    "/123"
  )

  expect_identical(
    toPath(list(test = "123/xyz")),
    "/123/xyz"
  )

  path <- buildTokenData(
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
  toPath <- compile(path)

  expect_identical(
    toPath(list(test = "123")),
    "/123"
  )
})
