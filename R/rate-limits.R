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
