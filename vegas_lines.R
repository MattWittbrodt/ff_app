# Getting Vegas Lines

vegas_lines <- function() {
  
  library(rvest)
  library(tidyverse)
  
  # Using rvest to get the data in
  url <- "https://www.sportsline.com/nfl/odds/"
  page <- read_html(url)
  table <- html_table(page, fill = T)
  df <- table[[1]]
  
  # Cleaning Data to retrieve home, away, favorite, and total
  df2 <- select(df, game, consensus) %>% 
         subset(str_detect(game, "GMT") == T & str_detect(consensus, "ML") == F) %>%
         group_by(game) %>% 
         summarize(consensus = paste(consensus, collapse = '')) %>%
         mutate(away_team = str_extract(game, "(?<=\\w{3}\\s)(\\w{2,3})"),
                home_team = str_extract_all(game, "(?<=[@]\\s)(\\w{2,3})"),
                total = str_extract(consensus,"(?<=[+])(\\d+\\.\\d|\\d+)"),
                line = str_extract(consensus, "(?<=\\w{2,3}\\s[:punct:])(\\d+\\.\\d|\\d+)"),
                favorite = str_extract(consensus, "\\w{2,3}")) %>%
         select(-consensus,-game)
  
  # Getting into usable format
  df3 <- gather(df2, key = "home_or_away", value = "team", -total, -favorite, -line) %>%
         .[c("team", "home_or_away", "line", "total", "favorite")] %>%
         mutate(home_or_away = str_remove(home_or_away, "_team"),
                favorite = ifelse(team == favorite, 1, 0),
                line = ifelse(favorite == 1, as.numeric(line)*-1, as.numeric(line)),
                implied_total = (as.numeric(total)/2)+((line*-1)/2))
  
  return(df3)
  
}

