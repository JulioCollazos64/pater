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

  path <- "{/:test}"
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

  path <- "/*test"
  toPath <- compile(path)

  expect_identical(
    toPath(list(test = "123")),
    "/123"
  )

  # TODO
  # expect_identical(
  #   toPath(list()),
  #   NULL
  # )

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
})
