#' Find a token from token cache that can perform an API call
#'
#' Mostly meant for internal `socialsampler` use. Loads all
#' tokens from the token database, or the token registered
#' with `rtweet` if not tokens have been registered with
#' `socialsampler`, creates the corresponding bearer tokens
#' (see [rtweet::bearer_token()]), and returns the token
#' that can serve meet the requests. If no tokens meet
#' these requirements, waits until the minimum amount
#' until the API rate limits reset for a token in the token
#' database.
#'
#' @param query The API endpoint you want to query. See
#'   <https://developer.twitter.com/en/docs/basics/rate-limits>.
#'
#' @param requests How many requests you need to make.
#'
#' @return A token that make the API request while respecting
#'   Twitter API rate limits.
#'
#' @export
#'
#' @importFrom dplyr filter arrange
find_token <- function(query, requests) {

  tokens <- get_all_tokens()

  possible_tokens <- safe_rate_limit(tokens) %>%
    filter(query == !!query, requests <= limit)

  if (nrow(possible_tokens) == 0)
    stop(
      "None of the registered tokens can make this many requests.",
      call. = FALSE
    )

  available_tokens <- possible_tokens %>%
    filter(requests <= remaining) %>%
    arrange(remaining)

  any_available_token <- nrow(available_tokens) > 0

  # wait the shortest amount of time for the query to reset
  if (!any_available_token) {
    min_until_reset <- min(possible_tokens$reset)

    message(
      "API calls exhausted. Waiting ",
      round(min_until_reset * 60),
      " seconds."
    )

    Sys.sleep(min_until_reset * 60)
    return(find_token(query, requests))
  }

  # return the least flexible token that meets has sufficient
  # remaining requests
  index <- available_tokens$id[1]
  tokens[[index]]
}

# issue: rtweet has to make an API for *each token* separately
# when checking rate limits. this will be inefficient for many tokens.
#
# eventually we may want to fix this, probably with some global
# variable that tracks rate limits and only intermittently updates
#

#' @importFrom dplyr mutate select bind_rows
#' @importFrom purrr keep
#' @importFrom rtweet rate_limit
safe_rate_limit <- function(tokens) {

  # assumes a very specific token order to match "id" / index
  # with actual rate limit

  user_tokens <- keep(tokens, ~inherits(.x, "Token1.0"))
  bearer_tokens <- keep(tokens, ~inherits(.x, "bearer"))

  num_user_tokens <- length(user_tokens)
  num_bearer_tokens <- length(bearer_tokens)
  num_tokens <- length(tokens)

  stopifnot(num_user_tokens + num_bearer_tokens == num_tokens)

  user_ids <- 1:num_user_tokens
  bearer_ids <- (num_user_tokens + 1):(num_user_tokens + num_bearer_tokens)

  results_per_user_token <- 166
  results_per_bearer_token <- 85

  user_rate_limits <- rate_limit(user_tokens) %>%
    mutate(id = rep(user_ids, each = results_per_user_token))

  bearer_rate_limits <- rate_limit(bearer_tokens) %>%
    mutate(id = rep(bearer_ids, each = results_per_bearer_token))

  user_rate_limits %>%
    select(-app) %>%
    bind_rows(bearer_rate_limits)
}

