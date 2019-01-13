meta <- read_rds(here("meta.rds"))
scores <- read_rds(here("scores.rds"))
missing_ids <- all_album_ids[!(all_album_ids %in% meta$album_id)]
missing_ids

missing_meta <- sapply(missing_ids, aoty_score_meta)

dat <- missing_meta %>% t() %>% 
  as_tibble()

missing_meta_scores <- bind_rows(dat$dat_score)
missing_meta_meta <- bind_rows(dat$dat_meta)

################################################################################
missing_meta_meta %>% group_by(album_id) %>% filter(n() > 1)

missing_meta_meta[which(missing_meta_meta$album_id == "115877")[1], "label"] <- "Dovetown/KROD records"
missing_meta_meta[which(missing_meta_meta$album_id == "103641")[1], "label"] <- "Father/Daughter"
missing_meta_meta[which(missing_meta_meta$album_id == "118663")[1], "label"] <- "Virgin/EMI"

missing_meta_meta <- missing_meta_meta[-c(which(missing_meta_meta$album_id == "115877")[2],
                                          which(missing_meta_meta$album_id == "103641")[2],
                                          which(missing_meta_meta$album_id == "118663")[2]),]

missing_meta_meta <- missing_meta_meta %>% 
  select(-v3, -daughter, -`krod records`, - emi)
################################################################################

missing_meta_meta <- missing_meta_meta %>% 
  mutate(genres = na_if(genres, "-")) %>% 
  mutate(genres = str_split(genres, ", ")) %>% 
  mutate(label = str_split(label, ", "))

meta_merge <- bind_rows(meta, missing_meta_meta)
scores_merge <- bind_rows(scores, missing_meta_scores)

write_rds(meta_merge, here("meta.rds"))
write_rds(scores_merge, here("scores.rds"))
