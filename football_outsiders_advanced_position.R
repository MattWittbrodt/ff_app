
# Getting Advanced Football Outsiders Metrics for each player
library(tidyverse)
library(rvest)

fo_qb <- function() {
  
  ## Read in data from regular and rushing ----
  df <- "https://www.footballoutsiders.com/stats/nfl/qb/2020" %>%
         read_html() %>%
         html_nodes(xpath = '//*[@id="node-76122"]/div/div/table[1]') %>%
         html_table() %>%
         .[[1]] %>%
         .[,-c(4,6,8)] %>%
         select(-DPI)
  colnames(df) <- str_to_lower(paste('pass',colnames(df), sep = "_")) %>%
                  str_replace("%","_per")
  
  df_rush <- "https://www.footballoutsiders.com/stats/nfl/qb/2020" %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="node-76122"]/div/div/table[2]') %>%
    html_table() %>%
    .[[1]] %>%
    .[,-c(4,6)]
  colnames(df_rush) <- str_to_lower(paste('rush',colnames(df_rush), sep = "_"))
  
  # Merge ----
  df_all <- left_join(df, df_rush, by = c("pass_player" = "rush_player", "pass_team" = "rush_team"))
  
  # Removing % from DVOA
  df_all[,c(3:ncol(df_all))] <- apply(df_all[,c(3:ncol(df_all))], 2, function(x) {as.numeric(str_remove_all(as.character(x), "%"))})
  
  return(df_all)
}

fo_rb <- function() {
  
  
  ## Read in data from rushing and receiving ----
  df <- "https://www.footballoutsiders.com/stats/nfl/rb/2020" %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="node-76123"]/div/div/table[1]') %>%
    html_table() %>%
    .[[1]] %>%
    .[,-c(4,6,13)] %>%
    select(-Runs, -Yards, -TD, -FUM)
  
  colnames(df) <- str_to_lower(paste('rush',colnames(df), sep = "_")) %>%
                  str_remove_all("\t") %>% str_replace("\n", "_")
  
  df_rec <- "https://www.footballoutsiders.com/stats/nfl/rb/2020" %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="node-76123"]/div/div/table[3]') %>%
    html_table() %>%
    .[[1]] %>%
    .[,-c(4,6)] %>%
    select(-Passes, -Yards, -TD, -FUM)
  colnames(df_rec) <- str_to_lower(paste('rec', colnames(df_rec), sep = "_")) %>%
                      str_remove_all("\t") %>% str_replace("\n", "_")
  
  # Merge ----
  df_all <- left_join(df, df_rec, by = c("rush_player" = "rec_player", "rush_team" = "rec_team"))
  
  # Removing % from DVOA
  df_all[,c(3:ncol(df_all))] <- apply(df_all[,c(3:ncol(df_all))], 2, function(x) {as.numeric(str_remove_all(as.character(x), "%"))})
  
  return(df_all)
}
  
fo_pass_catchers <- function(position) {
  
  # Getting node number for position
  num <- switch(position, 
                "wr" = 76125,
                "te" = 76124)
  
  ## Read in data for receiving ----
  df <- paste('https://www.footballoutsiders.com/stats/nfl/',
              position,
              '/2020', sep = "") %>%
    read_html() %>%
    html_nodes(xpath = paste('//*[@id="node-', as.character(num), '"]/div/div/table[1]',sep = "")) %>%
    html_table() %>%
    .[[1]] %>%
    .[,-c(4,6)] %>%
    select(-Passes, -Yards, -TD, -FUM, -DPI)
  
  colnames(df) <- str_to_lower(paste('rec',colnames(df), sep = "_")) %>%
    str_remove_all("\t") %>% str_replace("\n", "_")
  
  # Removing % from DVOA
  df[,c(3:ncol(df))] <- apply(df[,c(3:ncol(df))], 2, function(x) {as.numeric(str_remove_all(as.character(x), "%"))})
  
  return(df)
  
}