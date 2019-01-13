library(tidyverse)
library(rvest)
library(stringr)
library(lubridate)
library(here)
source(here("aoty_functions.R"))

all_album_ids <- read_rds(here("all_album_ids.rds"))

dat <- sapply(all_album_ids, aoty_score_meta) %>% 
  t() %>% 
  as_tibble()

scores <- bind_rows(dat$dat_score)
meta <- bind_rows(dat$dat_meta)

################################################################################
# label fails on a few entries
meta %>% group_by(album_id) %>% filter(n() > 1)

meta[which(meta$album_id == "99028")[1], "label"] <- "Father/Daughter"
meta[which(meta$album_id == "123072")[1], "label"] <- "Snakepit / Roadrunner"
meta[which(meta$album_id == "115877")[1], "label"] <- "Dovetown/KROD records"
meta[which(meta$album_id == "103641")[1], "label"] <- "Father/Daughter"
meta[which(meta$album_id == "118663")[1], "label"] <- "Virgin/EMI"

meta <- meta[-c(which(meta$album_id == "99028")[2],
                which(meta$album_id == "123072")[2],
                which(meta$album_id == "115877")[2],
                which(meta$album_id == "103641")[2],
                which(meta$album_id == "118663")[2]),]
meta <- meta %>% 
  select(-v3, -daughter, - roadrunner, -`krod records`, - emi)

################################################################################

meta <- meta %>% 
  mutate(genres = na_if(genres, "-")) %>% 
  mutate(genres = str_split(genres, ", ")) %>% 
  mutate(label = str_split(label, ", "))

write_rds(scores, path = here("scores.rds"))
write_rds(meta, path = here("meta.rds"))
