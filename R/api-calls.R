#' Title
#'
#' # document why no arguments for token, parse, retryonratelimit, or n
#'
#' @param users
#' @param attempts
#' @param page
#' @param verbose
#'
#' @return
#' @export
#'
#' @importFrom rtweet get_friends
limited_get_friends <- function(users, attempts, page, verbose) {

  num_requests <- length(users)

  if (num_requests > 15)
    stop(
      "Must request friends of <=15 users at a time.",
      call. = FALSE
    )

  for (i in 1:attempts) {

    token <- find_token("friends/ids", num_requests)

    friends <- tryCatch({
      rtweet::get_friends(users, token = token, verbose = verbose)
    }, error = function(cond) {
      NULL
    }, warning = function(cond) {
      NULL
    })

    if (!is.null(friends))
      break
  }

  if (nrow(friends) == 0)
    return(empty_edgelist())

  colnames(friends) <- c("from", "to")
  friends
}

#' Title
#'
#' @param users
#' @param attempts
#' @param page
#' @param verbose
#'
#' @return
#' @export
#'
#' @importFrom rtweet get_followers
#'
limited_get_followers <- function(users, attempts, page, verbose) {

  num_requests <- length(users)

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

limited_lookup_users <- function(users, attempts) {

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

limited_get_timelines <- function(
  user, n = 100, max_id = NULL, home = FALSE, ...) {

  num_requests <- length(user)

  if (num_requests > 900)
    stop(
      "Must request timelines of <=900 users at a time.",
      call. = FALSE
    )

  for (i in 1:attempts) {

    token <- find_token("statuses/user_timeline", num_requests)

    user_data <- tryCatch({
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

    if (!is.null(user_data))
      break
  }

  user_data

}

