# probably should give these like 5 attempts or something like
# that just in case

# TODO: fancy sampling
safe_get_friends <- function(node, token = NULL, ...) {

  stopifnot(length(node) == 1)

  friends <- rtweet::get_friends(node, token = token, verbose = FALSE, ...)

  if (nrow(friends) == 0)
    return(empty_edge_data)

  colnames(friends) <- c("from", "to")
  friends
}

safe_get_followers <- function(node, token = NULL, ...) {

  stopifnot(length(node) == 1)

  followers <- rtweet::get_followers(node, token = token, verbose = FALSE, ...)

  if (all(is.na(followers$user_id)))
    return(empty_edge_data)

  colnames(followers) <- "from"
  followers$to <- node

  followers
}

safe_lookup_users <- function(user_id, attempts) {

  if (length(user_id) > 90000)
    stop("TODO request less data")

  for (i in 1:attempts) {

    token <- find_token("users/lookup")

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
safe_get_friends <- function(users, attempts, page, verbose) {

  for (i in 1:attempts) {

    token <- find_token("friends/ids")

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
    stop("TODO") # return(empty_edgelist())

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
safe_get_followers <- function(users, attempts, page, verbose) {

  for (i in 1:attempts) {

    token <- find_token("followers/ids")

    friends <- tryCatch({
      rtweet::get_followers(users, token = token, verbose = verbose)
    }, error = function(cond) {
      NULL
    }, warning = function(cond) {
      NULL
    })

    if (!is.null(friends))
      break
  }

  followers <- rtweet::get_followers(node, token = token, verbose = FALSE, ...)

  if (all(is.na(followers$user_id)) || nrow(followers) == 0)
    stop("TODO") # return(empty_edgelist())

  colnames(followers) <- "from"
  followers$to <- node

  followers
}

safe_lookup_users <- function(user_id, attempts) {

  token <- find_token("users/lookup")

  for (i in 1:attempts) {

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
