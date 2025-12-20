<!-- README.md is generated from README.Rmd. Please edit that file -->

# pater

<!-- badges: start -->

<!-- badges: end -->

The goal of pater is to decompose a given pathname into meaningful tokens that will be later used to construct a regular expression useful when matching an HTTP request to a route handler.

Core to this are **path parameters** which are pieces of text you define in your route definitions and that will be filled when a new request comes in, they are useful because they act as identifiers for a given concept you care about.

Path parameters can appear *anywhere* you want inside a given path, they may share the same path segment or be next to plain text, given this the following are valid positions for path parameters:

-   /text/:parameter
-   /text/:parameter1-:parameter2
-   /text1/:parameter1/text2{.:parameter2}
-   /text1/\*parameter

`pater` borrows its syntax from the original [path-to-regexp](https://github.com/pillarjs/path-to-regexp) implementation that you can read, understand and experiment with `pater` in R as the R implementation tries to be as close as possible to the original.

## Installation

You can install the stable version of `pater` from CRAN with:

``` r
install.packages("pater")
```

You can install the development version of `pater` like so:

``` r
# install.packages("pak")
pak::pak("JulioCollazos64/pater")
```

# Common patterns

-   Parameters

``` r
library(pater)
#> 
#> Attaching package: 'pater'
#> The following objects are masked from 'package:base':
#> 
#>     match, parse
fn <- match("/path/:foo")
fn("/path/bar")
#> $path
#> [1] "/path/bar"
#> 
#> $params
#> $params$foo
#> [1] "bar"

fn <- match("/path/:foo-:bar")
fn("/path/foo-bar")
#> $path
#> [1] "/path/foo-bar"
#> 
#> $params
#> $params$foo
#> [1] "foo"
#> 
#> $params$bar
#> [1] "bar"
```

-   Optional parameters

``` r
library(pater)
fn <- match("/foo{/:bar}")
fn("/foo")
#> $path
#> [1] "/foo"
#> 
#> $params
#> list()
fn("/foo/bar")
#> $path
#> [1] "/foo/bar"
#> 
#> $params
#> $params$bar
#> [1] "bar"
```

-   Wildcard

``` r
library(pater)
fn <- match("/*foo")
fn("/foo/bar")
#> $path
#> [1] "/foo/bar"
#> 
#> $params
#> $params$foo
#> [1] "foo" "bar"
```