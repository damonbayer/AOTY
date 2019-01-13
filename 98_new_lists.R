library(tidyverse)
library(lubridate)
library(here)
library(rvest)
source(here("aoty_functions.R"))


rankings_original <- read_rds(here("rankings.rds"))

raw_html <- read_html("https://www.albumoftheyear.org/list/summary/2018/")

pub_names <- raw_html %>% 
  html_nodes('.noSingleColumn a') %>% 
  html_text()

new_lists <- which(!(pub_names %in% unique(rankings_original$pub_name)))

pub_links <- raw_html %>% 
  html_nodes('.noSingleColumn a') %>% 
  html_attrs() %>% 
  unlist() %>% 
  unname() %>% 
  paste0("https://www.albumoftheyear.org", .)

pub_key <- tibble(pub_name = pub_names[new_lists],
                  pub_link = pub_links[new_lists])

pub_rankings <- apply(X = pub_key, MARGIN = 1, FUN = function(x) {
  aoty_rank(url = x[2], pub_name = x[1])
}
) %>% 
  bind_rows() %>% 
  mutate(is_ranked = T)

rankings <- bind_rows(rankings_original, pub_rankings)

write_rds(rankings, here("rankings.rds"))

################################################################################

meta_original <- read_rds(here("meta.rds"))
scores_original <- read_rds(here("scores.rds"))

new_albums <- unique(pub_rankings$album_id)[!(unique(pub_rankings$album_id) %in% meta_original$album_id)]

dat <- sapply(new_albums, aoty_score_meta) %>% 
  t() %>% 
  as_tibble()

scores <- bind_rows(dat$dat_score)
meta <- bind_rows(dat$dat_meta)

meta <- meta %>% 
  mutate(genres = na_if(genres, "-")) %>% 
  mutate(genres = str_split(genres, ", ")) %>% 
  mutate(label = str_split(label, ", "))

meta <- bind_rows(meta_original, meta)
scores <- bind_rows(scores_original, scores)

write_rds(meta, here("meta.rds"))
write_rds(scores, here("scores.rds"))
