library(rtweet)
library(lubridate)
library(tidyverse)
library(tibble)

get_tweets_pls_apelido <- function(words_df) {
  tweets <- purrr::map_df(words_df$apelido, ~ .get_tweets(.x))
}

.get_tweets <- function(word) {
  print(paste0("Baixando tweets com termo: '", word, "'..."))
  tweets <- search_tweets(word, n = 18000, include_rts = FALSE)
  pressao <- as_tibble()
  if (nrow(tweets) != 0) {
    tweets <- tweets %>% 
      mutate(termo = word) %>% 
      mutate(week = epiweek(created_at))
    pressao <- tweets %>%
      group_by(termo, week) %>%
      summarise(tweets = n(),
                retweets = sum(retweet_count),
                favs = sum(favorite_count))  
  } else {
    warning(paste0("Nao possui tweets relacionados ao termo: '", word, "'."))
  }
  return(pressao)
}
