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

  num_requests <- length(users)

  if (num_requests > 15)
    stop(
      "Must request friends of <=15 users at a time.",
      call. = FALSE
    )

  for (i in 1:attempts) {

    token <- find_token("friends/ids", num_requests)

    friends <- tryCatch({
      rtweet::get_friends(users, n = n, token = token, verbose = verbose)
    }, error = function(cond) {
      NULL
    }, warning = function(cond) {
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
safe_get_followers <- function(user, n = 5000, page = -1, verbose = FALSE,
                               attempts = 5) {

  num_requests <- length(user)

  if (num_requests > 15)
    stop(
      "Must request followers of <=15 users at a time.",
      call. = FALSE
    )

  for (i in 1:attempts) {

    token <- find_token("followers/ids", num_requests)

    followers <- tryCatch({
      rtweet::get_followers(users, token = token, verbose = verbose)
    }, error = function(cond) {
      NULL
    }, warning = function(cond) {
      NULL
    })

    if (!is.null(friends))
      break
  }

  if (all(is.na(followers$user_id)) || nrow(followers) == 0)
    return(empty_edgelist())

  colnames(followers) <- "from"
  followers$to <- node

  followers
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

  num_requests <- length(users)

  if (num_requests > 90000)
    stop(
      "Must request user data of <=90000 users at a time.",
      call. = FALSE
    )

  for (i in 1:attempts) {

    token <- find_token("users/lookup", num_requests)

    user_data <- tryCatch({
      rtweet::lookup_users(users = user_id, token = token)
    }, error = function(cond) {
      NULL
    }, warning = function(cond) {
      NULL
    })

    if (!is.null(user_data))
      break
  }

  user_data
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

  num_requests <- length(user)

  if (num_requests > 1500)
    stop(
      "Must request timelines of <=1500 users at a time.",
      call. = FALSE
    )

  for (i in 1:attempts) {

    token <- find_token("statuses/user_timeline", num_requests)

    timeline_data <- tryCatch({
      rtweet::get_timeline(
        users = users,
        max_id = max_id,
        home = home,
        token = token
      )
    }, error = function(cond) {
      NULL
    }, warning = function(cond) {
      NULL
    })

    if (!is.null(timeline_data))
      break
  }

  timeline_data
}

