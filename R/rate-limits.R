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

  log_debug(
    glue("Called find_token(query = \"{query}\", requests = {requests})")
  )

  possible_tokens <- get_rate_table() %>%
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

    secs_until_reset <- as.numeric(
      min(possible_tokens$reset_at - Sys.time()),
      units = "secs"
    ) + 5

    if (secs_until_reset >= 0) {
      message(
        Sys.time(),
        " API calls exhausted. Waiting ",
        prettyunits::pretty_sec(secs_until_reset)
      )

      Sys.sleep(secs_until_reset)
    } else {

      wait_until <- ..last_update.. + lubridate::minutes(15)

      time_to_wait <- as.numeric(
        wait_until - Sys.time(),
        units = "secs"
      )

      message(
        Sys.time(),
        " Need to synchronize with Twitter. Waiting ",
        prettyunits::pretty_sec(time_to_wait)
      )

      if (time_to_wait > 0)
        Sys.sleep(time_to_wait)
      else
        warning("Negative time to wait LOL WHY")

    }


    return(find_token(query, requests))
  }

  tokens <- get_all_tokens()

  # return the least flexible token that meets has sufficient
  # remaining requests
  token_id <- available_tokens$id[1]
  list(token_id = token_id, token = tokens[[token_id]])
}

get_rate_table <- function() {

  log_debug("Getting rate table")

  update <- FALSE

  if (!exists("..rate_limit_table..")) {
    update <- TRUE
  } else {

    time_since_last_update <- as.numeric(
      Sys.time() - ..last_update..,
      units = "mins"
    )

    if (time_since_last_update > 15)
      update <- TRUE
  }

  if (update) {
    log_debug("Refreshing rate table with Twitter")
    rate_table <- safe_rate_limit(get_all_tokens())
    assign("..rate_limit_table..", rate_table, envir = globalenv())
    assign("..last_update..", Sys.time(), envir = globalenv())
  }

  ..rate_limit_table..
}

update_rate_table <- function(query, token_id, used) {
  ..rate_limit_table..[
    ..rate_limit_table..$query == query &
      ..rate_limit_table..$id == token_id,
    "remaining"
  ] <<- ..rate_limit_table..[
    ..rate_limit_table..$query == query &
      ..rate_limit_table..$id == token_id,
  ]$remaining - used
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

  log_debug("Called safe_rate_limit()")

  # TODO: this function requires that rtweet::rate_limit()
  # doesn't exceed rate limits

  # assumes a very specific token order to match "id" / index
  # with actual rate limit

  user_tokens <- keep(tokens, ~inherits(.x, "Token1.0"))
  bearer_tokens <- keep(tokens, ~inherits(.x, "bearer"))

  num_user_tokens <- length(user_tokens)
  num_bearer_tokens <- length(bearer_tokens)
  num_tokens <- length(tokens)

  stopifnot(num_user_tokens + num_bearer_tokens == num_tokens)

  # sanity check for duplicate tokens
  stopifnot(length(unique(user_tokens)) == num_user_tokens)
  stopifnot(length(unique(bearer_tokens)) == num_bearer_tokens)

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
    bind_rows(bearer_rate_limits) %>%
    select(-reset)
}

