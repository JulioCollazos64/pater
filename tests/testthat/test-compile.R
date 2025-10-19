test_that("Compile works", {
  path <- "/"
  toPath <- compile(path)

  expect_identical(
    toPath(),
    "/"
  )

  expect_identical(
    toPath(list(id = "123")),
    "/"
  )

  path <- "/test"
  toPath <- compile(path)

  expect_identical(
    toPath(),
    "/test"
  )

  expect_identical(
    toPath(list(id = "123")),
    "/test"
  )

  path <- "/test/"
  toPath <- compile(path)

  expect_identical(
    toPath(),
    "/test/"
  )

  expect_identical(
    toPath(list(id = "123")),
    "/test/"
  )

  path <- '/:"0"'
  toPath <- compile(path)

  expect_identical(
    toPath(list(`0` = "123")),
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
    "/123/xyz"
  )

  path <- "{/:test}" # "Optional parameter"
  toPath <- compile(path)

  expect_identical(
    toPath(list()),
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

  path <- "/users{/:id}/delete"
  toPath <- compile(path)

  expect_identical(
    toPath(list()),
    "/users/delete"
  )

  expect_identical(
    toPath(list(id = "123")),
    "/users/123/delete"
  )

  path <- "/*test"
  toPath <- compile(path)

  expect_identical(
    toPath(list(test = "123")),
    "/123"
  )

  # The wildcard is used to match one or more parameters, can't be defined with 0
  expect_error(
    toPath(list()),
    regexp = "Missing parameter"
  )

  expect_identical(
    toPath(list(test = list("123", "xyz"))),
    "/123/xyz"
  )

  path <- "/user/:id"
  toPath <- compile(path)
  expect_identical(
    toPath(list(id = "café")),
    "/user/caf%C3%A9"
  )

  path <- "/*segment"
  toPath <- compile(path)
  expect_identical(
    toPath(list(segment = list("a", "b", "c"))),
    "/a/b/c"
  )
})
