library(rvest)
library(tidyverse)
library(here)
# Maybe we can be smarter about this later
valid_int <- 1:29

rated_album_ids <- sapply(valid_int, function(x) {
  print(x)
  str_c("https://www.albumoftheyear.org/ratings/6-highest-rated/2018/", x) %>%
    read_html() %>%
    html_nodes('.albumListTitle a') %>%
    html_attrs() %>%
    unlist() %>%
    str_extract('\\d+')
  }) %>%
  as.vector() %>%
  unlist()

ranked_album_ids <- read_rds(here("rankings.rds")) %>%
  pull(album_id) %>%
  unique()

all_album_ids <- c(ranked_album_ids, rated_album_ids) %>%
  unique()

write_rds(all_album_ids, here("all_album_ids.rds"))
