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

  break_time <- 2

  tokens <- get_all_tokens()
  num_tokens <- length(tokens)

  while (TRUE) {

    # random search order, in terms of token indices
    for (i in sample(num_tokens)) {

      remaining <- tryCatch({
        rtweet::rate_limit(tokens[[i]], query = query)$remaining
      }, error = function(cond) {
        message("tryCatch ", cond)
        0L
      })

      # TODO: not sure what !length() checks for?
      if (is.null(remaining) || !length(remaining))
        remaining <- 0L

      if (remaining >= requests) {
        log_debug("Using token ", i)
        return(tokens[[i]])
      }

    }

    message(
      "All tokens currently exhausted. Waiting ", break_time,
      " minutes."
    )

    Sys.sleep(60 * break_time)
  }
}
