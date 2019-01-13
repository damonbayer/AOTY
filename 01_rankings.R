library(tidyverse)
library(rvest)
library(here)
source(here("aoty_functions.R"))


raw_html <- read_html("https://www.albumoftheyear.org/list/summary/2018/")

# Artist
artist <- raw_html %>%
  html_nodes('.artistTitle a') %>%
  rvest::html_text()

# Title
title <- raw_html %>%
  html_nodes('.albumTitle a') %>%
  rvest::html_text()

# Points
points <- raw_html %>%
  html_nodes('.summaryPoints a') %>%
  rvest::html_text() %>% 
  str_match_all("[0-9]+") %>% 
  as.integer()

# Album ID (for matching)
album_id <- raw_html %>% 
  html_nodes('.albumTitle a') %>% 
  html_attrs() %>% 
  unlist() %>% 
  str_extract('\\d+')

base <- tibble(artist = artist,
               title = title,
               points = points,
               album_id = album_id)

hon_mention <- raw_html %>%
  html_node('.alignTop div:nth-child(55)') %>% 
  html_text() %>% 
  str_remove('Honorable Mention:') %>% 
  str_split('(?<=\\)), ') %>% 
  unlist() %>% 
  str_split(' - | \\(', simplify = T) %>% 
  as_tibble() %>% 
  rename(artist = V1,
         title = V2,
         points = V3) %>% 
  mutate(points = as.integer(str_remove(points, '\\)')),
         album_id = raw_html %>%
           html_node('.alignTop div:nth-child(55)') %>% 
           html_nodes('a') %>% 
           html_attrs() %>% 
           unlist() %>% 
           str_extract('\\d+'))

# Error on Against All Logic
hon_mention %>% filter(V4 != "")
hon_mention[hon_mention$album_id == "102788","points"] <- as.integer(str_extract(hon_mention[hon_mention$album_id == "102788","V5"], pattern = "\\d+"))


aoty_ranking <- bind_rows(base, hon_mention) %>% 
  mutate(rank = dense_rank(desc(points)),
         pub_name = "aoty") %>% 
  arrange(rank) %>% 
  select(album_id, pub_name, rank)

################################################################################
# Pub names
pub_names <- raw_html %>% 
  html_nodes('.noSingleColumn a') %>% 
  html_text()

# Pub links
pub_links <- raw_html %>% 
  html_nodes('.noSingleColumn a') %>% 
  html_attrs() %>% 
  unlist() %>% 
  unname() %>% 
  paste0("https://www.albumoftheyear.org", .)

pub_key <- tibble(pub_name = pub_names,
                  pub_link = pub_links)

pub_rankings <- apply(X = pub_key, MARGIN = 1, FUN = function(x) {
  aoty_rank(url = x[2], pub_name = x[1])
  }
  ) %>% 
  bind_rows()

rankings <- bind_rows(aoty_ranking, pub_rankings)

rankings <- rankings %>% 
  mutate(is_ranked = T)

write_rds(rankings, path = here("rankings.rds"))