test_that("Stringify works", {
  tokens <- buildTokenData(
    list(
      list(
        type = "text",
        value = "/"
      )
    )
  )
  path <- stringifyTokens(tokens)

  expect_identical(
    path,
    "/"
  )

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

  # group
  tokens <- buildTokenData(
    list(
      list(
        type = "text",
        value = "/users"
      ),
      list(
        type = "group",
        tokens = buildTokenData(
          list(
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
      ),
      list(
        type = "text",
        value = "/delete"
      )
    )
  )
  path <- stringifyTokens(tokens)

  expect_identical(
    path,
    "/users{/:id}/delete"
  )

  tokens <- buildTokenData(
    list(
      list(
        type = "text",
        value = "/:+?*"
      )
    )
  )
  path <- stringifyTokens(tokens)

  expect_identical(
    path,
    "/\\:\\+\\?\\*"
  )

  tokens <- buildTokenData(
    list(
      list(
        type = "text",
        value = "/"
      ),
      list(
        type = "wildcard",
        name = "0"
      )
    )
  )
  path <- stringifyTokens(tokens)

  expect_identical(
    path,
    '/*"0"'
  )

  tokens <- buildTokenData(
    list(
      list(
        type = "text",
        value = "/"
      ),
      list(
        type = "wildcard",
        name = "test"
      )
    )
  )
  path <- stringifyTokens(tokens)

  expect_identical(
    path,
    "/*test"
  )

  tokens <- buildTokenData(
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
  path <- stringifyTokens(tokens)

  expect_identical(
    path,
    '/:"0"'
  )

  tokens <- buildTokenData(
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
  path <- stringifyTokens(tokens)

  expect_identical(
    path,
    "/:café"
  )

  # see: https://github.com/pillarjs/path-to-regexp/pull/390/files

  tokens <- buildTokenData(
    list(
      list(
        type = "text",
        value = "\\"
      ),
      list(
        type = "param",
        name = "test"
      )
    )
  )
  path <- stringifyTokens(tokens)

  expect_identical(
    path,
    "\\\\:test"
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
