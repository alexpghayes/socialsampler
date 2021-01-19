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

#' Inspect the socialsampler token database
#'
#' Prints information on the number of tokens registered with
#' `socialsampler`. If you haven't registered an tokens,
#' sampling function will use the default `rtweet` token,
#' and barring this, will give an error that tokens have
#' been registered with neither `rtweet` or `socialsampler`.
#'
#' @export
#' @family tokens
#'
#' @seealso [rtweet::get_token()], [rtweet::create_token()],
#'   [rtweet::bearer_token()]
#'
inspect_token_db <- function() {

  if (!token_db_exists())
    stop(
      "No tokens have been registered with socialsampler.", call. = FALSE
    )

  num_tokens <- nrow(read_rds(get_token_db_path()))

  print(
    glue(
      "Your socialsampler token database has\n",
      "  - {num_tokens} registered token(s)",
      .trim = FALSE
    )
  )
}

#' Remove all tokens for the token database
#'
#' Will ask you for confirmation before proceeding.
#'
#' @export
#' @family tokens
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
#' @param api_key Twitter application API key (character).
#' @param api_secret Twitter application API secret (character).
#' @param access_token Twitter access token (character).
#' @param access_secret Twitter accces secret (character).
#'
#' @export
#'
#' @family tokens
#' @seealso [rtweet::create_token()]
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
    warning(
      "Token has already been registered with socialsampler.",
      call. = FALSE
    )

  write_rds(updated_db, db_path)
  invisible()
}

#' Import tokens registered with rtweet into the socialsampler token database
#'
#' @export
#' @family tokens
#'
import_tokens_from_rtweet <- function() {

  token_paths <- list.files(
    "~",
    pattern = "rtweet_token",
    all.files = TRUE,
    full.names = TRUE
  )

  for (path in token_paths) {

    token <- readr::read_rds(path)

    register_token(
      api_key = token$app$key,
      api_secret = token$app$secret,
      access_token = token$credentials$oauth_token,
      access_secret = token$credentials$oauth_token_secret
    )
  }
}


#' Get all tokens from the social sampler database
#'
#' Mainly for internal `socialsampler` use.
#'
#' @return A list of all tokens in the `socialsampler` database
#'   as well as the accompanying bearer tokens. Note that user
#'   tokens and bearer tokens have different classes and behave
#'   slightly differently.
#'
#' @export
#' @family tokens
#' @importFrom rtweet get_token create_token bearer_token
#'
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

  user_tokens <- list()

  # TODO: why is there only a single bearer token??

  for (index in 1:nrow(token_db)) {

    suppressWarnings(
      user_token <- create_token(
        app = paste0("socialsampler", index),
        token_db$api_key[index],
        token_db$api_secret[index],
        token_db$access_token[index],
        token_db$access_secret[index],
        set_renv = FALSE
      )
    )

    user_tokens[[index]] <- user_token
  }

  c(user_tokens, list(bearer_token(user_token)))
}

