get_token_db_path <- function() {
  sys_path <- Sys.getenv("SOCIALSAMPLER_PATH")

  if (sys_path == "")
    return(path.expand("~/.socialsampler_token_db.rds"))

  sys_path
}

token_db_exists <- function() {
  file.exists(get_token_db_path())
}

create_token_db_if_needed <- function() {
  if (!token_db_exists()) {
    db_path <- get_token_db_path()
    write_rds(empty_token_cache, db_path)
  }
}

#' TODO
#'
#' @return
#' @export
#'
print_token_db <- function() {

  if (!token_db_exists())
    stop(
      "No tokens have been registered with socialsampler.", call. = FALSE
    )

  num_tokens <- nrow(read_rds(get_token_db_path()))

  glue::glue(
    "Your socialsampler token database has\n",
    "  - {num_tokens} registered node(s)",
    .trim = FALSE
  )
}

#' Remove all sampled users from the cache
#'
#' Will ask you for confirmation before proceeding because
#' of the how time consuming it is to sample users.
#'
#' @export
#' @family cache management
#'
unregister_all_tokens <- function() {

  if (!token_db_exists())
    stop("No token database detected.", call. = FALSE)

  msg <- "I want to unregister all tokens from socialsampler"

  if (usethis::ui_yeah(msg))
    invisible(file.remove(get_token_db_path()))
}

#' Register a Twitter authentication token with socialsampler
#'
#' If you try to register a token already in the cache,
#' you'll get a warning. That is, you are protected from
#' creating duplicate entries in the token cache.
#'
#' For more details on how to get tokens please read
#' `vignette("auth", package = "rtweet")`.
#'
#' @param token A Twitter auth token token created by
#'   [rtweet::create_token()].
#'
#' @export
#'
#' @family managing tokens
#'
#' @importFrom dplyr distinct bind_rows
#' @importFrom readr read_rds write_rds
#'
register_token <- function(api_key, api_secret, access_token, access_secret) {

  create_token_db_if_needed()
  db_path <- get_token_db_path()
  token_db <- read_rds(db_path)

  new_token <- tibble::tibble(
    api_key = api_key,
    api_secret = api_secret,
    access_token = access_token,
    access_secret = access_secret
  )

  updated_db <- distinct(bind_rows(token_db, new_token))

  if (nrow(updated_db) == nrow(token_db))
    stop(
      "Token has already been registered with socialsampler.",
      call. = FALSE
    )

  write_rds(updated_db, db_path)
  invisible()
}

import_tokens_from_rtweet <- function() {


  token_paths <- list.files(
    "~",
    pattern = "rtweet_token",
    all.files = TRUE,
    full.names = TRUE
  )

  path <- token_paths[19]

  token <- readr::read_rds(path)

  token$app


}


#' Title
#'
#' @return
#' @export
#' @importFrom rtweet get_token create_token bearer_token
get_all_tokens <- function() {

  if (!token_db_exists() && Sys.getenv("TWITTER_PAT") == "")
    stop(
      "No tokens in socialsampler database and no token ",
      "registered with rtweet.",
      call. = FALSE
    )

  if (!token_db_exists() && Sys.getenv("TWITTER_PAT") != "") {
    rtweet_token <- get_token()
    return(list(token, list(bearer_token(rtweet_token))))
  }

  token_db <- read_rds(get_token_db_path())

  tokens <- list()

  for (index in 1:nrow(token_db)) {

    user_token <- create_token(
      app = "socialsampler",
      token_db$api_key[index],
      token_db$api_secret[index],
      token_db$access_token[index],
      token_db$access_secret[index],
      set_renv = FALSE
    )

    tokens <- append(tokens, user_token)
    tokens <- append(tokens, list(bearer_token(user_token)))
  }

  tokens
}

