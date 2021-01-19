library(tibble)

empty_token_cache <- tibble(
  api_key = character(0),
  api_secret = character(0),
  access_token = character(0),
  access_secret = character(0)
)

usethis::use_data(
  empty_token_cache,
  internal = TRUE,
  overwrite = TRUE
)

