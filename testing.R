
local_friends_rate_table <- function() {
  get_rate_table() %>%
    filter(query == "friends/ids") %>%
    arrange(desc(remaining))
}

twitter_friends_rate_table <- function() {
  get_all_tokens() %>%
    safe_rate_limit() %>%
    filter(query == "friends/ids") %>%
    arrange(desc(remaining))
}



for (i in 1:340) {
  message(
    glue::glue(
      "Making query {i}"
    )
  )

  # safe_get_friends("alexpghayes")
  safe_get_followers("alexpghayes")
}

# 10, 50, 90

for (i in 1:5) {
  message(
    glue::glue(
      "Making query {i}"
    )
  )

  safe_get_friends("alexpghayes")
}

View(local_friends_rate_table())
View(twitter_friends_rate_table())

# token uniqueness. can i generate more bearer tokens?

tokens <- get_all_tokens()

token1 <- tokens[[1]]
token2 <- tokens[[2]]

bearer1 <- bearer_token(token1)
bearer2 <- bearer_token(token2)

identical(token1, token2)

identical(bearer1, bearer2)


rtweet::bearer_token()

length(unique(tokens))


lo

safe_rate_limit(get_all)


get_all_tokens() %>%
  safe_rate_limit() %>%
  filter(query == "followers/ids") %>%
  arrange(desc(remaining))

# socialsampler perception

get_rate_table() %>%
  filter(query == "followers/ids") %>%
  arrange(desc(remaining))

