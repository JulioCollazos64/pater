test_that("Stringify works", {
  tokens <- buildTokenData(
    list(
      list(
        type = "text",
        value = "/users/"
      ),
      list(
        type = "param",
        name = "userId"
      ),
      list(
        type = "text",
        value = "/posts/"
      ),
      list(
        type = "param",
        name = "postId"
      )
    )
  )
  path <- stringifyTokens(tokens)

  expect_identical(
    path,
    "/users/:userId/posts/:postId"
  )

  # This is one case where the token next to the parameter
  # is not "safe".
  tokens <- buildTokenData(
    list(
      list(
        type = "text",
        value = "/users/"
      ),
      list(
        type = "param",
        name = "id"
      ),
      list(
        type = "text",
        value = "randomText"
      )
    )
  )
  path <- stringifyTokens(tokens)

  expect_identical(
    path,
    '/users/:"id"randomText'
  )

  tokens <- buildTokenData(
    list(
      list(
        type = "text",
        value = "/"
      ),
      list(
        type = "param",
        name = "foo"
      )
    )
  )
  path <- stringifyTokens(tokens)

  expect_identical(
    path,
    "/:foo"
  )
})

test_that("Stringify errors", {
  tokens <- buildTokenData(
    list(
      list(
        type = "non-existent",
        value = "/users/"
      )
    )
  )

  expect_error(
    stringifyTokens(tokens),
    regexp = "Unknown .* type"
  )
})
