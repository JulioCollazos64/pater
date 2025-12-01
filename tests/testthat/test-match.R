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

  path <- "/:foo\\(:bar\\)"
  fn <- match(path)

  expect_identical(
    fn("/hello(world)"),
    list(
      path = "/hello(world)",
      params = list(
        foo = "hello",
        bar = "world"
      )
    )
  )

  expect_identical(
    fn("/hello()"),
    FALSE
  )

  path <- "/:foo\\({:bar}\\)"
  fn <- match(path)

  expect_identical(
    fn("/hello(world)"),
    list(
      path = "/hello(world)",
      params = list(
        foo = "hello",
        bar = "world"
      )
    )
  )

  expect_identical(
    fn("/hello()"),
    list(
      path = "/hello()",
      params = list(
        foo = "hello"
      )
    )
  )

  path <- "{/:foo}{/:bar}-ext"
  fn <- match(path)

  expect_identical(
    fn("/-ext"),
    FALSE
  )

  expect_identical(
    fn("-ext"),
    list(
      path = "-ext",
      params = list()
    )
  )

  expect_identical(
    fn("/foo-ext"),
    list(
      path = "/foo-ext",
      params = list(
        foo = "foo"
      )
    )
  )

  expect_identical(
    fn("/foo/bar-ext"),
    list(
      path = "/foo/bar-ext",
      params = list(
        foo = "foo",
        bar = "bar"
      )
    )
  )

  expect_identical(
    fn("/foo/-ext"),
    FALSE
  )

  path <- "/:required{/:optional}-ext"
  fn <- match(path)

  expect_identical(
    fn("/foo-ext"),
    list(
      path = "/foo-ext",
      params = list(
        required = "foo"
      )
    )
  )

  expect_identical(
    fn("/foo/bar-ext"),
    list(
      path = "/foo/bar-ext",
      params = list(
        required = "foo",
        optional = "bar"
      )
    )
  )

  expect_identical(
    fn("/foo/-ext"),
    FALSE
  )

  # Unicode matches
  path <- "/:foo"
  fn <- match(path)

  expect_identical(
    fn("/café"),
    list(
      path = "/café",
      params = list(
        foo = "café"
      )
    )
  )

  path <- "/:foo"
  fn <- match(path, FALSE)

  expect_identical(
    fn("/caf%C3%A9"),
    list(
      path = "/caf%C3%A9",
      params = list(
        foo = "caf%C3%A9"
      )
    )
  )

  path <- "/café"
  fn <- match(path)

  expect_identical(
    fn("/café"),
    list(
      path = "/café",
      params = list()
    )
  )

  path <- "/café"

  fn <- match(path, encodePath = utils::URLencode)

  expect_identical(
    fn("/caf%C3%A9"),
    list(
      path = "/caf%C3%A9",
      params = list()
    )
  )

  # Hostnames

  path <- ":domain.com"
  fn <- match(path, delimiter = ".")

  expect_identical(
    fn("example.com"),
    list(
      path = "example.com",
      params = list(
        domain = "example"
      )
    )
  )

  expect_identical(
    fn("github.com"),
    list(
      path = "github.com",
      params = list(
        domain = "github"
      )
    )
  )

  path <- "mail.:domain.com"
  fn <- match(path, delimiter = ".")

  expect_identical(
    fn("mail.example.com"),
    list(
      path = "mail.example.com",
      params = list(
        domain = "example"
      )
    )
  )

  expect_identical(
    fn("mail.github.com"),
    list(
      path = "mail.github.com",
      params = list(
        domain = "github"
      )
    )
  )

  path <- "mail{.:domain}.com"
  fn <- match(path, delimiter = ".")

  expect_identical(
    fn("mail.com"),
    list(
      path = "mail.com",
      params = list()
    )
  )

  expect_identical(
    fn("mail.example.com"),
    list(
      path = "mail.example.com",
      params = list(
        domain = "example"
      )
    )
  )

  expect_identical(
    fn("mail.github.com"),
    list(
      path = "mail.github.com",
      params = list(
        domain = "github"
      )
    )
  )

  path <- "example.:ext"
  fn <- match(path, delimiter = ".")

  expect_identical(
    fn("example.com"),
    list(
      path = "example.com",
      params = list(
        ext = "com"
      )
    )
  )

  expect_identical(
    fn("example.org"),
    list(
      path = "example.org",
      params = list(
        ext = "org"
      )
    )
  )

  path <- "this is"
  fn <- match(path, delimiter = " ", end = FALSE)

  expect_identical(
    fn("this is a test"),
    list(
      path = "this is",
      params = list()
    )
  )

  expect_identical(
    fn("this isn't"),
    FALSE
  )

  # prefixes

  path <- "$:foo{$:bar}"
  fn <- match(path)

  expect_identical(
    fn("$x"),
    list(
      path = "$x",
      params = list(
        foo = "x"
      )
    )
  )

  expect_identical(
    fn("$x$y"),
    list(
      path = "$x$y",
      params = list(
        foo = "x",
        bar = "y"
      )
    )
  )

  path <- "name{/:attr1}{-:attr2}{-:attr3}"
  fn <- match(path)

  expect_identical(
    fn("name"),
    list(
      path = "name",
      params = list()
    )
  )

  expect_identical(
    fn("name/test"),
    list(
      path = "name/test",
      params = list(
        attr1 = "test"
      )
    )
  )

  expect_identical(
    fn("name/1"),
    list(
      path = "name/1",
      params = list(
        attr1 = "1"
      )
    )
  )

  expect_identical(
    fn("name/1-2"),
    list(
      path = "name/1-2",
      params = list(
        attr1 = "1",
        attr2 = "2"
      )
    )
  )

  expect_identical(
    fn("name/1-2-3"),
    list(
      path = "name/1-2-3",
      params = list(
        attr1 = "1",
        attr2 = "2",
        attr3 = "3"
      )
    )
  )

  expect_identical(
    fn("name/foo-bar/route"),
    FALSE
  )

  expect_identical(
    fn("name/test/route"),
    FALSE
  )

  # From upstream: https://github.com/pillarjs/path-to-regexp/issues/206

  path <- "/user{s}/:user"
  fn <- match(path)

  expect_identical(
    fn("/user/123"),
    list(
      path = "/user/123",
      params = list(
        user = "123"
      )
    )
  )

  expect_identical(
    fn("/users/123"),
    list(
      path = "/users/123",
      params = list(
        user = "123"
      )
    )
  )

  # Wildcard
  path <- "/*path"
  fn <- match(path)

  expect_identical(
    fn("/"),
    FALSE
  )

  expect_identical(
    fn("/route"),
    list(
      path = "/route",
      params = list(
        path = "route"
      )
    )
  )

  expect_identical(
    fn("/route/nested"),
    list(
      path = "/route/nested",
      params = list(
        path = c("route", "nested")
      )
    )
  )

  path <- "*path"
  fn <- match(path)

  expect_identical(
    fn("/"),
    list(
      path = "/",
      params = list(
        path = "" # TODO: it returns two "", but probably wont cause any problem
      )
    )
  )

  expect_identical(
    fn("/test"),
    list(
      path = "/test",
      params = list(
        path = c("", "test")
      )
    )
  )

  path <- "*path"
  fn <- match(path, decode = FALSE)

  expect_identical(
    fn("/"),
    list(
      path = "/",
      params = list(
        path = "/"
      )
    )
  )

  expect_identical(
    fn("/test"),
    list(
      path = "/test",
      params = list(
        path = "/test"
      )
    )
  )

  path <- "/*path.:ext"
  fn <- match(path)

  expect_identical(
    fn("/test.html"),
    list(
      path = "/test.html",
      params = list(
        path = "test",
        ext = "html"
      )
    )
  )

  expect_identical(
    fn("/test.html/nested"),
    FALSE
  )

  expect_identical(
    fn("/test.html/nested.json"),
    list(
      path = "/test.html/nested.json",
      params = list(
        path = c("test.html", "nested"),
        ext = "json"
      )
    )
  )

  path <- "/:path.*ext"
  fn <- match(path)

  expect_identical(
    fn("/test.html"),
    list(
      path = "/test.html",
      params = list(
        path = "test",
        ext = "html"
      )
    )
  )

  expect_identical(
    fn("/test.html/nested"),
    list(
      path = "/test.html/nested",
      params = list(
        path = "test",
        ext = c("html", "nested")
      )
    )
  )

  expect_identical(
    fn("/test.html/nested.json"),
    list(
      path = "/test.html/nested.json",
      params = list(
        path = "test",
        ext = c("html", "nested.json")
      )
    )
  )

  path <- "/*path{.:ext}"
  fn <- match(path)

  expect_identical(
    fn("/test.html"),
    list(
      path = "/test.html",
      params = list(
        path = "test",
        ext = "html"
      )
    )
  )

  expect_identical(
    fn("/test.html/nested"),
    list(
      path = "/test.html/nested",
      params = list(
        path = c("test.html", "nested")
      )
    )
  )

  path <- "/entity/:id/*path"
  fn <- match(path)

  expect_identical(
    fn("/entity/foo"),
    FALSE
  )

  expect_identical(
    fn("/entity/foo/path"),
    list(
      path = "/entity/foo/path",
      params = list(
        id = "foo",
        path = "path"
      )
    )
  )

  path <- "/*foo/:bar/*baz"
  fn <- match(path)

  expect_identical(
    fn("/x/y/z"),
    list(
      path = "/x/y/z",
      params = list(
        foo = "x",
        bar = "y",
        baz = "z"
      )
    )
  )

  expect_identical(
    fn("/1/2/3/4/5"),
    list(
      path = "/1/2/3/4/5",
      params = list(
        foo = c("1", "2", "3"),
        bar = "4",
        baz = "5"
      )
    )
  )

  # Longer prefix

  path <- "/:foo{/test/:bar}"
  fn <- match(path)

  expect_identical(
    fn("/route"),
    list(
      path = "/route",
      params = list(
        foo = "route"
      )
    )
  )

  expect_identical(
    fn("/route/test/again"),
    list(
      path = "/route/test/again",
      params = list(
        foo = "route",
        bar = "again"
      )
    )
  )

  # Backtracking test.

  path <- "{:foo/}{:bar.}"
  fn <- match(path)

  expect_identical(
    fn(""),
    list(
      path = "",
      params = list()
    )
  )

  expect_identical(
    fn("test/"),
    list(
      path = "test/",
      params = list(
        foo = "test"
      )
    )
  )

  expect_identical(
    fn("a/b."),
    list(
      path = "a/b.",
      params = list(
        foo = "a",
        bar = "b"
      )
    )
  )

  path <- "/abc{abc:foo}"
  fn <- match(path)

  expect_identical(
    fn("/abc"),
    list(
      path = "/abc",
      params = list()
    )
  )

  expect_identical(
    fn("/abcabc"),
    FALSE
  )

  expect_identical(
    fn("/abcabc123"),
    list(
      path = "/abcabc123",
      params = list(
        foo = "123"
      )
    )
  )

  expect_identical(
    fn("/abcabcabc"),
    list(
      path = "/abcabcabc",
      params = list(
        foo = "abc"
      )
    )
  )

  path <- "/:foo{abc:bar}"
  fn <- match(path)

  expect_identical(
    fn("/abc"),
    list(
      path = "/abc",
      params = list(
        foo = "abc"
      )
    )
  )

  expect_identical(
    fn("/abcabc"),
    list(
      path = "/abcabc",
      params = list(
        foo = "abcabc"
      )
    )
  )

  expect_identical(
    fn("/abcabc123"),
    list(
      path = "/abcabc123",
      params = list(
        foo = "abc",
        bar = "123"
      )
    )
  )

  expect_identical(
    fn("/acb"),
    list(
      path = "/acb",
      params = list(
        foo = "acb"
      )
    )
  )

  expect_identical(
    fn("/123"),
    list(
      path = "/123",
      params = list(
        foo = "123"
      )
    )
  )

  expect_identical(
    fn("/123abcabc"),
    list(
      path = "/123abcabc",
      params = list(
        foo = "123abcabc"
      )
    )
  )

  path <- "/:foo\\abc:bar"
  fn <- match(path)

  expect_identical(
    fn("/abc"),
    FALSE
  )

  expect_identical(
    fn("/abcabc"),
    FALSE
  )

  expect_identical(
    fn("/abcabc123"),
    list(
      path = "/abcabc123",
      params = list(
        foo = "abc",
        bar = "123"
      )
    )
  )

  expect_identical(
    fn("/123abcabc"),
    FALSE
  )

  path <- "/route|:param|"
  fn <- match(path)

  expect_identical(
    fn("/route|world|"),
    list(
      path = "/route|world|",
      params = list(
        param = "world"
      )
    )
  )

  expect_identical(
    fn("/route||"),
    FALSE
  )

  path <- "/:foo|:bar|"
  fn <- match(path)

  expect_identical(
    fn("/hello|world|"),
    list(
      path = "/hello|world|",
      params = list(
        foo = "hello",
        bar = "world"
      )
    )
  )

  expect_identical(
    fn("/hello||"),
    FALSE
  )

  path <- "/:foo{|:bar|}"
  fn <- match(path)

  expect_identical(
    fn("/hello|world|"),
    list(
      path = "/hello|world|",
      params = list(
        foo = "hello",
        bar = "world"
      )
    )
  )

  expect_identical(
    fn("/hello||"),
    list(
      path = "/hello||",
      params = list(
        foo = "hello||"
      )
    )
  )

  path <- ":foo\\@:bar"
  fn <- match(path)

  expect_identical(
    fn("x@y"),
    list(
      path = "x@y",
      params = list(
        foo = "x",
        bar = "y"
      )
    )
  )

  expect_identical(
    fn("x@"),
    FALSE
  )

  path <- "%25:foo{%25:bar}"
  fn <- match(path, delimiter = "%25")

  expect_identical(
    fn("%25hello"),
    list(
      path = "%25hello",
      params = list(
        foo = "hello"
      )
    )
  )

  expect_identical(
    fn("%25hello%25world"),
    list(
      path = "%25hello%25world",
      params = list(
        foo = "hello",
        bar = "world"
      )
    )
  )

  expect_identical(
    fn("%25555%25222"),
    list(
      path = "%25555%25222",
      params = list(
        foo = "555",
        bar = "222"
      )
    )
  )

  path <- "%25:foo..:bar"
  fn <- match(path, delimiter = "%25")

  expect_identical(
    fn("%25hello..world"),
    list(
      path = "%25hello..world",
      params = list(
        foo = "hello",
        bar = "world"
      )
    )
  )

  expect_identical(
    fn("%25555..222"),
    list(
      path = "%25555..222",
      params = list(
        foo = "555",
        bar = "222"
      )
    )
  )

  expect_identical(
    fn("%25555....222%25"),
    list(
      path = "%25555....222%25",
      params = list(
        foo = "555..",
        bar = "222"
      )
    )
  )

  # Longer vectors are normalized

  path <- c("/:foo/:bar", "/:foo/:baz")
  fn <- match(path)

  expect_identical(
    fn("/hello/world"),
    list(
      path = "/hello/world",
      params = list(
        foo = "hello",
        bar = "world"
      )
    )
  )

  # Token Data

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
  fn <- match(path)

  expect_identical(
    fn("/123"),
    list(
      path = "/123",
      params = list(
        test = "123"
      )
    )
  )
})
