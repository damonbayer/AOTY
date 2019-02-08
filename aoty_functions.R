aoty_rank <- function(url, pub_name = NA) {
  print(paste('Starting', pub_name))
  raw_html <- read_html(url)
  
  # Album ID
  album_id <- raw_html %>% 
    html_nodes('.albumListTitle a') %>% 
    html_attrs() %>% 
    unlist() %>% 
    str_extract('\\d+')
  
  # Rank
  rank <- raw_html %>% 
    html_nodes(".albumListRank") %>% 
    html_text() %>% 
    as.integer()
  
  if(length(rank) == 0){
    rank <- NA
  }
  
  dat <- tibble(album_id = album_id,
                pub_name = pub_name,
                rank = rank)
  dat
}

aoty_score_meta <- function(album_id) {
  raw_html <- str_c("https://www.albumoftheyear.org/album/", album_id) %>% read_html()
  
  # Metadata
  # Artist
  artist <- raw_html %>% html_node('.artist span') %>% html_text()
  
  # Title
  title <- raw_html %>% html_node('.albumTitle span') %>% html_text()
  
  # Score
  score <- raw_html %>% html_nodes('.albumReviewRating span') %>% html_text() %>% as.integer()
  
  # Publication Name
  pub_name <- raw_html %>% html_nodes('.albumReviewHeader span') %>% html_text()
  
  # Critic Score
  aoty_score <- raw_html %>% html_node(".albumCriticScore a") %>% html_text() %>% as.integer()
  
  dat_score <- tibble(album_id = album_id,
                      pub_name = c("aoty", pub_name),
                      score = c(aoty_score, score))
  
  dat_meta <- raw_html %>% 
    html_nodes(".detailRow") %>%
    html_text() %>%
    stringr::str_split('/', simplify = T) %>% 
    as_tibble() %>% 
    rename(key = V2,
           value = V1) %>% 
    mutate_all(str_trim) %>% 
    mutate(key = na_if(key, "")) %>% 
    na.omit() %>% 
    spread(key = key, value = value) %>% 
    rename_all(str_to_lower)
  
  if (!has_name(dat_meta, "release date")) {
    dat_meta <- mutate(dat_meta, `release date` = NA)
  }
    
    dat_meta <- dat_meta %>% 
    rename(date = `release date`) %>% 
    mutate(date = str_replace_all(date, "\\s+", " "),
           date = mdy(date)) %>% 
    mutate(album_id = album_id,
           artist = artist,
           title = title) %>% 
    select(album_id, artist, title, everything())
 
  print(str_c("Finished ", artist, " - ", title))
   
  list(dat_score = dat_score,
       dat_meta = dat_meta)
}

aoty_most_similar <- function(cor = aoty_cor, n = Inf){
  cor %>%
    as.data.frame() %>% 
    rownames_to_column() %>%
    as_tibble() %>%
    gather(key = "pub2", value = 'value', -rowname) %>%
    rename(pub1 = rowname) %>%
    filter(pub1 != pub2) %>% 
    mutate(key = paste0(pmin(pub1, pub2), pmax(pub1, pub2), sep = "")) %>% 
    distinct(key, .keep_all = T) %>% 
    select(-key) %>% 
    arrange(desc(value)) %>% 
    rename("Publication 1" = pub1,
           "Publication 2" = pub2,
           "Similarity" = value) %>% 
    head(n)
}

aoty_most_similar_to <- function(cor = aoty_cor, n = Inf, pub){
  cor %>%
    as.data.frame() %>% 
    rownames_to_column() %>%
    filter(rowname == pub) %>% 
    as_tibble() %>% 
    select(-rowname) %>% 
    gather("Publication", "Similarity") %>% 
    filter(Publication != pub) %>% 
    arrange(desc(Similarity))
}

## Unused function to call Python rbo function
# source_python("/Users/damon/Downloads/rbo-master/rbo.py")
# 
# aoty_rbo <- function(pub_1, pub_2, p = 0.5, max = Inf) {
#   py$p <- p
#   
#   py$list_1 <- ratings %>% 
#     filter(pub_name == pub_1, is_ranked) %>% 
#     select(x = rank, nm = album_id) %>%
#     pmap(set_names) %>% 
#     unlist() %>% 
#     tail(max) %>% 
#     as.list() %>% 
#     dict()
#   
#   py$list_2 <- ratings %>% 
#     filter(pub_name == pub_2, is_ranked) %>% 
#     select(x = rank, nm = album_id) %>%
#     pmap(set_names) %>% 
#     unlist() %>% 
#     tail(max) %>%
#     as.list() %>% 
#     dict()
#   
#   py_run_string("result = rbo_dict(list_1, list_2, p = p)")
#   bind_rows(unlist(py$result))
# }