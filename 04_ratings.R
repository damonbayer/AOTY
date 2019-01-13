library(tidyverse)
library(here)

rankings <- read_rds(here("rankings.rds"))
scores <- read_rds(here("scores.rds"))

ratings <- full_join(rankings, scores) %>% 
  mutate(is_ranked = !is.na(is_ranked))

write_rds(ratings, here("ratings.rds"))

meta <- read_rds(here("meta.rds"))

aoty <- full_join(ratings, meta)

write_rds(aoty, here("aoty.rds"))
