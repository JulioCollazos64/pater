
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pater

<!-- badges: start -->

<!-- badges: end -->

The goal of pater is to decompose a given pathname into meaningful
tokens that will be later used to construct a regular expression useful
when matching an HTTP request to a route handler.

Core to this are **path parameters** which are pieces of text you define
in your route definitions and that will be filled when a new request
comes in, they are useful because they act as identifiers for a given
concept you care about.

Path parameters can appear *anywhere* you want inside a given path, they
may share the same path segment or be next to plain text, given this the
following are valid positions for path parameters:

- /text/:parameter
- /text/:parameter1-:parameter2
- /text1/:parameter1/text2{.:parameter2}
- /text1/\*parameter

`pater` borrows its syntax from the original
[path-to-regexp](https://github.com/pillarjs/path-to-regexp)
implementation that you can read, understand and experiment with `pater`
in R as the R implementation tries to be as close as possible to the
original.

## Installation

You can install the stable version of `pater` from CRAN with:

``` r
install.packages("pater")
#> Installing package into '/tmp/RtmpeiTkU0/temp_libpatha14e5a624158'
#> (as 'lib' is unspecified)
```

You can install the development version of `pater` like so:

``` r
# install.packages("pak")
pak::pak("JulioCollazos64/pater")
```
