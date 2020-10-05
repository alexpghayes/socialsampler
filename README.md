
<!-- README.md is generated from README.Rmd. Please edit that file -->

# socialsampler

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/socialsampler)](https://CRAN.R-project.org/package=socialsampler)
[![Codecov test
coverage](https://codecov.io/gh/alexpghayes/socialsampler/branch/master/graph/badge.svg)](https://codecov.io/gh/alexpghayes/socialsampler?branch=master)
[![R build
status](https://github.com/alexpghayes/socialsampler/workflows/R-CMD-check/badge.svg)](https://github.com/alexpghayes/socialsampler/actions)
<!-- badges: end -->

`socialsampler` extends [`rtweet`](https://rtweet.info/) to gracefully
handle multiple tokens, to automatically use bearer tokens for increased
sampling capacity, and to automatically retry failed API requests.

## Installation

You can install the development version of `socialsampler` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("alexpghayes/socialsampler")
```

## Sampling Twitter data with `socialsampler`

`socialsampler` provides (essentially) drop-in replacements for several
`rtweet` functions. These drop-in functions follow the same naming
conventions as `rtweet` functions, but with a `safe_*` prefix. For
example:

``` r
library(socialsampler)

safe_get_friends("alexpghayes")
#> # A tibble: 1,244 x 2
#>    from        to                 
#>    <chr>       <chr>              
#>  1 alexpghayes 294710260          
#>  2 alexpghayes 24096463           
#>  3 alexpghayes 14636374           
#>  4 alexpghayes 1096256122137137153
#>  5 alexpghayes 1308811981         
#>  6 alexpghayes 1897939710         
#>  7 alexpghayes 780934633          
#>  8 alexpghayes 20406724           
#>  9 alexpghayes 1152655010871820288
#> 10 alexpghayes 302667955          
#> # ... with 1,234 more rows
```

If you run into Twitter API rate limits, `socialsampler` will just wait
for the rate limit to reset.

## Registering and managing your tokens

The major functionality of `socialsampler` is its ability to use
multiple tokens for data collection, automatically using tokens with
remaining API calls. Additionally, `socialsampler` uses both user tokens
and bearer tokens. Again, this is automatic, and should at least double
sampling capacity relative to `rtweet` for most API requests.

Registering a token is straightforward:

``` r
register_token(
  consumer_key = "FILL_THIS_IN",
  consumer_secret = "FILL_THIS_IN",
  access_token = "FILL_THIS_IN",
  access_secret = "FILL_THIS_IN"
)
```

You only ever need to register a token once, and `socialsampler` won’t
accidentally add any duplicate tokens to the token database. To see how
many tokens you have, you can use:

``` r
inspect_token_db()
#> Your socialsampler token database has
#>   - 10 registered token(s)
```

You can import `rtweet` tokens into the database via
`import_tokens_from_rtweet()`, and clear all tokens from the token
database with `unregister_all_tokens()`.

If no tokens have been registered with `socialsampler`, API calls will
look for a token registered with `rtweet` and use that token instead,
again automatically respecting rate limits and leveraging the
corresponding bearer token. If you have not registered tokens either
`socialsampler` or `rtweet` and you try to make an API request, you’ll
get an error.
