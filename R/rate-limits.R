
# potentially inefficient for large numbers of nodes
find_token <- function(query, requests) {

  tokens <- get_all_tokens()

  limits <- safe_rate_limit(tokens) %>%
    dplyr::filter(query == !!query, remaining >= requests) %>%
    arrange(remaining)

  calls_remaining <- nrow(limits) > 0

  # wait the shortest amount of time for the query to reset
  if (!calls_remaining) {
    min_until_reset <- min(limits$reset)

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
  index <- limits$token[1]
  tokens[[index]]
}

#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate_at vars
safe_rate_limit <- function(tokens) {
  limits <- map_dfr(tokens, single_safe_rate_limit, .id = "token")
  mutate_at(limits, vars(token), as.integer)
}

#' @importFrom dplyr select
#' @importFrom rtweet rate_limit
single_safe_rate_limit <- function(token) {
  select(
    rate_limit(token),
    query, limit, remaining, reset, reset_at, timestamp
  )
}
