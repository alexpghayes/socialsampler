# API RATE LIMIT INFORMATION INFORMATION // 2020-02-25
#
# 15 minute windows
#
# endpoint                   | requests (user auth) / requests (app auth)
# GET followers/ids	         | 15 /	15
# GET friends/ids            | 15	/ 15
# GET users/lookup	         | 900 / 300
# GET statuses/user_timeline | 900 / 1500


#' Safely get friend list of Twitter users
#'
#' `safe_get_friends()` is a drop-in replacement for
#' [rtweet::get_friends()] that automatically respects
#' Twitter API rate limits and coordinates use of
#' multiple Twitter access tokens that have been
#' registered with [register_token()]. At the moment
#' you can only request the friends list of 15 or
#' fewer users in a single call.
#'
#' If no tokens have been registered with `socialsampler`,
#' `safe_get_friends()` will look for a token registered
#' with `rtweet` and use that token instead, again
#' automatically respecting rate limits. If no tokens have
#' been registered with either `socialsampler` or `rtweet`,
#' you'll get an error.
#'
#' @inheritParams rtweet::get_friends
#'
#' @param attempts How many times should we attempt to access the Twitter
#'   API before giving up? Defaults to 5.
#'
#' @return An edgelist with columns:
#'
#'   - `from`: A character vector of node ids. This will match the
#'     input format of `users`. That is, if `users` consists of
#'     screen names, `from` will consist of screen names. If
#'     `users` consists of user IDs, `from` will as well.
#'
#'   - `to`: A character vector of node ids.
#'
#' @export
#'
#' @importFrom rtweet get_friends
#'
safe_get_friends <- function(users, n = 5000, page = -1, verbose = FALSE,
                             attempts = 5) {

  # split big request into chunks of size at most 15 users

  chunked_users <- split(users, ceiling(seq_along(users) / 15))

  handle_single_chunk <- function(chunk) {

    num_requests <- length(chunk)

    if (num_requests > 15)
      stop(
        "Must request friends of <=15 users at a time.",
        call. = FALSE
      )

    for (i in 1:attempts) {

      token <- find_token("friends/ids", num_requests)

      friends <- tryCatch({
        rtweet::get_friends(chunk, n = n, page = page,
                            token = token, verbose = verbose)
      }, error = function(cond) {
        warning(cond)
        NULL
      }, warning = function(cond) {
        warning(cond)
        NULL
      })

      if (!is.null(friends))
        break
    }

    if (is.null(friends) || nrow(friends) == 0)
      return(empty_edgelist())

    colnames(friends) <- c("from", "to")
    friends
  }

  purrr::map_dfr(chunked_users, handle_single_chunk)
}

#' Safely get follower list of Twitter users
#'
#' `safe_get_followers()` is a drop-in replacement for
#' [rtweet::get_followers()] that automatically respects
#' Twitter API rate limits and coordinates use of
#' multiple Twitter access tokens that have been
#' registered with [register_token()]. At the moment
#' you can only request the friends list of 15 or
#' fewer users in a single call.
#'
#' If no tokens have been registered with `socialsampler`,
#' `safe_get_followers()` will look for a token registered
#' with `rtweet` and use that token instead, again
#' automatically respecting rate limits. If no tokens have
#' been registered with either `socialsampler` or `rtweet`,
#' you'll get an error.
#'
#' @inheritParams rtweet::get_followers
#'
#' @param attempts How many times should we attempt to access the Twitter
#'   API before giving up? Defaults to 5.
#'
#' @return An edgelist with columns:
#'
#'   - `from`: A character vector of node ids. This will match the
#'     input format of `users`. That is, if `users` consists of
#'     screen names, `from` will consist of screen names. If
#'     `users` consists of user IDs, `from` will as well.
#'
#'   - `to`: A character vector of node ids.
#'
#' @export
#'
#' @importFrom rtweet get_followers
#'
safe_get_followers <- function(users, n = 5000, page = -1, verbose = FALSE,
                               attempts = 5) {

  handle_single_chunk <- function(chunk) {

    num_requests <- length(chunk)

    if (num_requests > 1)
      stop(
        "Must request followers of 1 user at a time.",
        call. = FALSE
      )

    for (i in 1:attempts) {

      token <- find_token("followers/ids", num_requests)

      followers <- tryCatch({
        rtweet::get_followers(chunk, page = page,
                              token = token, verbose = verbose)
      }, error = function(cond) {
        warning(cond)
        NULL
      }, warning = function(cond) {
        warning(cond)
      NULL
    })

      if (!is.null(followers))
        break
    }

    if (all(is.na(followers$user_id)) || nrow(followers) == 0)
      return(empty_edgelist())

    colnames(followers) <- "from"
    followers$to <- chunk

    followers
  }

  purrr::map_dfr(users, handle_single_chunk)
}

#' Safely lookup Twitter users
#'
#' `safe_lookup_users()` is a drop-in replacement for
#' [rtweet::lookup_users()] that automatically respects
#' Twitter API rate limits and coordinates use of
#' multiple Twitter access tokens that have been
#' registered with [register_token()]. At the moment
#' you can only request the friends list of 15 or
#' fewer users in a single call.
#'
#' If no tokens have been registered with `socialsampler`,
#' `safe_lookup_users()` will look for a token registered
#' with `rtweet` and use that token instead, again
#' automatically respecting rate limits. If no tokens have
#' been registered with either `socialsampler` or `rtweet`,
#' you'll get an error.
#'
#' @inheritParams rtweet::lookup_users
#'
#' @param attempts How many times should we attempt to access the Twitter
#'   API before giving up? Defaults to 5.
#'
#' @inherit rtweet::lookup_users return
#'
#' @export
#'
#' @importFrom rtweet lookup_users
#'
safe_lookup_users <- function(users, attempts = 5) {

  chunked_users <- split(users, ceiling(seq_along(users) / 90000))

  handle_single_chunk <- function(chunk) {

    num_requests <- ceiling(length(chunk) / 100)

    if (num_requests > 900)
      stop(
        "Must request user data on <=90000 users at a time.",
        call. = FALSE
      )

    for (i in 1:attempts) {

      token <- find_token("users/lookup", num_requests)

      user_data <- tryCatch({
        rtweet::lookup_users(users = chunk, token = token)
      }, error = function(cond) {
        warning(cond)
        NULL
      }, warning = function(cond) {
        warning(cond)
        NULL
      })

      if (!is.null(user_data))
        break
    }

    user_data
  }

  purrr::map_dfr(chunked_users, handle_single_chunk)
}


#' Safely sample Twitter user timelines
#'
#' `safe_get_timelines()` is a drop-in replacement for
#' [rtweet::get_timelines()] that automatically respects
#' Twitter API rate limits and coordinates use of
#' multiple Twitter access tokens that have been
#' registered with [register_token()]. At the moment
#' you can only request the friends list of 15 or
#' fewer users in a single call.
#'
#' If no tokens have been registered with `socialsampler`,
#' `safe_get_timelines()` will look for a token registered
#' with `rtweet` and use that token instead, again
#' automatically respecting rate limits. If no tokens have
#' been registered with either `socialsampler` or `rtweet`,
#' you'll get an error.
#'
#' @inheritParams rtweet::get_timelines
#'
#' @param attempts How many times should we attempt to access the Twitter
#'   API before giving up? Defaults to 5.
#'
#' @inherit rtweet::get_timelines return
#'
#' @export
#'
#' @importFrom rtweet get_timelines
#'
safe_get_timelines <- function(user, n = 100, max_id = NULL, home = FALSE,
                               attempts = 5) {

  chunked_users <- split(user, ceiling(seq_along(user) / 900))

  handle_single_chunk <- function(chunk) {

    num_requests <- length(chunk)

    if (num_requests > 900)
      stop(
        "Must request timelines of <=900 users at a time.",
        call. = FALSE
      )

    if (home)
      stop("`home = TRUE` is not yet supported.", call. = FALSE)

    for (i in 1:attempts) {

      log_debug(glue("safe_get_timelines(): Attempt {i}"))

      token <- find_token("statuses/user_timeline", num_requests)

      timeline_data <- tryCatch({
        rtweet::get_timeline(
          user = chunk,
          max_id = max_id,
          home = home,
          n = n,
          token = token
        )
      }, error = function(cond) {
        warning(cond)
        NULL
      }, warning = function(cond) {
        warning(cond)
        NULL
      })

      if (!is.null(timeline_data))
        break
    }

    timeline_data
  }

  purrr::map_dfr(chunked_users, handle_single_chunk)
}
