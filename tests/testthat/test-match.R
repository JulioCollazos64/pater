test_that("match works", {
  # Simple paths
  path <- "/"
  fn <- match(path)

  expect_identical(
    fn("/"),
    list(
      path = "/",
      params = list()
    )
  )

  expect_identical(
    fn("/route"),
    FALSE
  )

  path <- "/test"
  fn <- match(path)

  expect_identical(
    fn("/test"),
    list(
      path = "/test",
      params = list()
    )
  )

  expect_identical(
    fn("/route"),
    FALSE
  )

  expect_identical(
    fn("/test/route"),
    FALSE
  )

  expect_identical(
    fn("/test/"),
    list(
      path = "/test/",
      params = list()
    )
  )

  expect_identical(
    fn("/TEST/"),
    list(
      path = "/TEST/",
      params = list()
    )
  )

  path <- "/test/"
  fn <- match(path)

  expect_identical(
    fn("/test/"),
    list(
      path = "/test/",
      params = list()
    )
  )

  expect_identical(
    fn("/route"),
    FALSE
  )

  expect_identical(
    fn("/test"),
    FALSE
  )

  expect_identical(
    fn("/test//"),
    list(
      path = "/test//",
      params = list()
    )
  )

  path <- "/:test"
  fn <- match(path)

  expect_identical(
    fn("/route"),
    list(
      path = "/route",
      params = list(
        test = "route"
      )
    )
  )

  expect_identical(
    fn("/route/"),
    list(
      path = "/route/",
      params = list(
        test = "route"
      )
    )
  )

  expect_identical(
    fn("/route.json"),
    list(
      path = "/route.json",
      params = list(
        test = "route.json"
      )
    )
  )

  expect_identical(
    fn("/route.json/"),
    list(
      path = "/route.json/",
      params = list(
        test = "route.json"
      )
    )
  )

  expect_identical(
    fn("/route/test"),
    FALSE
  )

  expect_identical(
    fn("/caf%C3%A9"),
    list(
      path = "/caf%C3%A9",
      params = list(
        test = "café"
      )
    )
  )

  expect_identical(
    fn("/;,:@&=+$-_.!~*()"),
    list(
      path = "/;,:@&=+$-_.!~*()",
      params = list(
        test = ";,:@&=+$-_.!~*()"
      )
    )
  )

  expect_identical(
    fn("/param%2523"),
    list(
      path = "/param%2523",
      params = list(
        test = "param%23"
      )
    )
  )

  # Case-sensitive paths
  path <- "/test"
  fn <- match(path, sensitive = TRUE)

  expect_identical(
    fn("/test"),
    list(
      path = "/test",
      params = list()
    )
  )

  expect_identical(
    fn("/TEST"),
    FALSE
  )

  path <- "/TEST"
  fn <- match(path, sensitive = TRUE)

  expect_identical(
    fn("/test"),
    FALSE
  )

  expect_identical(
    fn("/TEST"),
    list(
      path = "/TEST",
      params = list()
    )
  )

  # non-ending mode
  path <- "/test"
  fn <- match(path, end = FALSE)

  expect_identical(
    fn("/test"),
    list(
      path = "/test",
      params = list()
    )
  )

  expect_identical(
    fn("/test/"),
    list(
      path = "/test/",
      params = list()
    )
  )

  expect_identical(
    fn("/test////"),
    list(
      path = "/test",
      params = list()
    )
  )

  expect_identical(
    fn("/route/test"),
    FALSE
  )

  expect_identical(
    fn("/test/route"),
    list(
      path = "/test",
      params = list()
    )
  )

  expect_identical(
    fn("/route"),
    FALSE
  )

  path <- "/test/"
  fn <- match(path, end = FALSE)

  expect_identical(
    fn("/test"),
    FALSE
  )

  expect_identical(
    fn("/test/"),
    list(
      path = "/test/",
      params = list()
    )
  )

  expect_identical(
    fn("/test//"),
    list(
      path = "/test//",
      params = list()
    )
  )

  expect_identical(
    fn("/test/route"),
    FALSE
  )

  expect_identical(
    fn("/route/test/deep"),
    FALSE
  )

  path <- "/:test"
  fn <- match(path, end = FALSE)

  expect_identical(
    fn("/route"),
    list(
      path = "/route",
      params = list(
        test = "route"
      )
    )
  )

  expect_identical(
    fn("/route/"),
    list(
      path = "/route/",
      params = list(
        test = "route"
      )
    )
  )

  expect_identical(
    fn("/route.json"),
    list(
      path = "/route.json",
      params = list(
        test = "route.json"
      )
    )
  )

  expect_identical(
    fn("/route.json/"),
    list(
      path = "/route.json/",
      params = list(
        test = "route.json"
      )
    )
  )

  expect_identical(
    fn("/route/test"),
    list(
      path = "/route",
      params = list(
        test = "route"
      )
    )
  )

  expect_identical(
    fn("/route.json/test"),
    list(
      path = "/route.json",
      params = list(
        test = "route.json"
      )
    )
  )

  expect_identical(
    fn("/caf%C3%A9"),
    list(
      path = "/caf%C3%A9",
      params = list(
        test = "café"
      )
    )
  )

  path <- "/:test/"
  fn <- match(path, end = FALSE)

  expect_identical(
    fn("/route"),
    FALSE
  )

  expect_identical(
    fn("/route/"),
    list(
      path = "/route/",
      params = list(
        test = "route"
      )
    )
  )

  expect_identical(
    fn("/route/test"),
    FALSE
  )

  expect_identical(
    fn("/route/test/"),
    FALSE
  )

  expect_identical(
    fn("/route//test"),
    list(
      path = "/route/",
      params = list(
        test = "route"
      )
    )
  )

  path <- ""
  fn <- match(path, end = FALSE)

  expect_identical(
    fn(""),
    list(
      path = "",
      params = list()
    )
  )

  expect_identical(
    fn("/"),
    list(
      path = "/",
      params = list()
    )
  )

  expect_identical(
    fn("route"),
    FALSE
  )

  expect_identical(
    fn("/route"),
    list(
      path = "",
      params = list()
    )
  )

  expect_identical(
    fn("/route/"),
    list(
      path = "",
      params = list()
    )
  )

  # Optional

  path <- "{/route}"
  fn <- match(path)

  expect_identical(
    fn(""),
    list(
      path = "",
      params = list()
    )
  )

  expect_identical(
    fn("/"),
    list(
      path = "/",
      params = list()
    )
  )

  expect_identical(
    fn("/foo"),
    FALSE
  )

  expect_identical(
    fn("/route"),
    list(
      path = "/route",
      params = list()
    )
  )

  path <- "{/:test}"
  fn <- match(path)

  expect_identical(
    fn("/route"),
    list(
      path = "/route",
      params = list(
        test = "route"
      )
    )
  )

  expect_identical(
    fn(""),
    list(
      path = "",
      params = list()
    )
  )

  expect_identical(
    fn("/"),
    list(
      path = "/",
      params = list()
    )
  )

  path <- "{/:test}/bar"
  fn <- match(path)

  expect_identical(
    fn("/bar"),
    list(
      path = "/bar",
      params = list()
    )
  )

  expect_identical(
    fn("/foo/bar"),
    list(
      path = "/foo/bar",
      params = list(
        test = "foo"
      )
    )
  )

  expect_identical(
    fn("/foo/bar/"),
    list(
      path = "/foo/bar/",
      params = list(
        test = "foo"
      )
    )
  )

  path <- "{/:test}-bar"
  fn <- match(path)

  expect_identical(
    fn("-bar"),
    list(
      path = "-bar",
      params = list()
    )
  )

  expect_identical(
    fn("/foo-bar"),
    list(
      path = "/foo-bar",
      params = list(
        test = "foo"
      )
    )
  )

  expect_identical(
    fn("/foo-bar/"),
    list(
      path = "/foo-bar/",
      params = list(
        test = "foo"
      )
    )
  )

  # No prefix characters

  path <- "test"
  fn <- match(path)

  expect_identical(
    fn("test"),
    list(
      path = "test",
      params = list()
    )
  )

  expect_identical(
    fn("/test"),
    FALSE
  )

  path <- ":test"
  fn <- match(path)

  expect_identical(
    fn("route"),
    list(
      path = "route",
      params = list(
        test = "route"
      )
    )
  )

  expect_identical(
    fn("/route"),
    FALSE
  )

  expect_identical(
    fn("route/"),
    list(
      path = "route/",
      params = list(
        test = "route"
      )
    )
  )

  path <- "{:test}"
  fn <- match(path)

  expect_identical(
    fn("test"),
    list(
      path = "test",
      params = list(
        test = "test"
      )
    )
  )

  expect_identical(
    fn(""),
    list(
      path = "",
      params = list()
    )
  )

  # Formats
  path <- "/test.json"
  fn <- match(path)

  expect_identical(
    fn("/test.json"),
    list(
      path = "/test.json",
      params = list()
    )
  )

  expect_identical(
    fn("/test"),
    FALSE
  )

  path <- "/:test.json"
  fn <- match(path)

  expect_identical(
    fn("/.json"),
    FALSE
  )

  expect_identical(
    fn("/test.json"),
    list(
      path = "/test.json",
      params = list(
        test = "test"
      )
    )
  )

  expect_identical(
    fn("/route.json"),
    list(
      path = "/route.json",
      params = list(
        test = "route"
      )
    )
  )

  expect_identical(
    fn("/route.json.json"),
    list(
      path = "/route.json.json",
      params = list(
        test = "route.json"
      )
    )
  )

  # format and path params

  path <- "/:test.:format"
  fn <- match(path)

  expect_identical(
    fn("/route.html"),
    list(
      path = "/route.html",
      params = list(
        test = "route",
        format = "html"
      )
    )
  )

  expect_identical(
    fn("/route"),
    FALSE
  )

  expect_identical(
    fn("/route.html.json"),
    list(
      path = "/route.html.json",
      params = list(
        test = "route.html",
        format = "json"
      )
    )
  )

  path <- "/:test{.:format}"
  fn <- match(path)

  expect_identical(
    fn("/route"),
    list(
      path = "/route",
      params = list(
        test = "route"
      )
    )
  )

  expect_identical(
    fn("/route.json"),
    list(
      path = "/route.json",
      params = list(
        test = "route",
        format = "json"
      )
    )
  )

  expect_identical(
    fn("/route.json.html"),
    list(
      path = "/route.json.html",
      params = list(
        test = "route.json",
        format = "html"
      )
    )
  )

  path <- "/:test.:format\\z"
  fn <- match(path)

  expect_identical(
    fn("/route.htmlz"),
    list(
      path = "/route.htmlz",
      params = list(
        test = "route",
        format = "html"
      )
    )
  )

  expect_identical(
    fn("/route.html"),
    FALSE
  )

  # escaped characters

  path <- "/\\(testing\\)"
  fn <- match(path)

  expect_identical(
    fn("/testing"),
    FALSE
  )

  expect_identical(
    fn("/(testing)"),
    list(
      path = "/(testing)",
      params = list()
    )
  )

  path <- "/.\\+\\*\\?\\{\\}=^\\!\\:$\\[\\]\\|"
  fn <- match(path)

  expect_identical(
    fn("/.+*?{}=^!:$[]|"),
    list(
      path = "/.+*?{}=^!:$[]|",
      params = list()
    )
  )

  # random examples

  path <- "/:foo/:bar"
  fn <- match(path)

  expect_identical(
    fn("/match/route"),
    list(
      path = "/match/route",
      params = list(
        foo = "match",
        bar = "route"
      )
    )
  )

  path <- "/:foo\\(test\\)/bar"
  fn <- match(path)

  expect_identical(
    fn("/foo(test)/bar"),
    list(
      path = "/foo(test)/bar",
      params = list(
        foo = "foo"
      )
    )
  )

  expect_identical(
    fn("/foo/bar"),
    FALSE
  )

  path <- "/:foo\\?"
  fn <- match(path)

  expect_identical(
    fn("/route?"),
    list(
      path = "/route?",
      params = list(
        foo = "route"
      )
    )
  )

  expect_identical(
    fn("/route"),
    FALSE
  )

  path <- "/{:pre}baz"
  fn <- match(path)

  expect_identical(
    fn("/foobaz"),
    list(
      path = "/foobaz",
      params = list(
        pre = "foo"
      )
    )
  )

  expect_identical(
    fn("/baz"),
    list(
      path = "/baz",
      params = list()
    )
  )
})
