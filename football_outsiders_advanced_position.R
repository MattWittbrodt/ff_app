
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
  # This is going to get some of the lower used QBs but they may still be relevant
  df_low_use <- "https://www.footballoutsiders.com/stats/nfl/qb/2020" %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="node-76122"]/div/div/table[2]') %>%
    html_table() %>%
    .[[1]] %>%
    .[,-c(4,6,8)] %>%
    select(-DPI) %>%
    mutate(Yards = as.character(Yards), # the main df reads in 1,344 as char, so fixing for now before join
           EYds = as.character(EYds))

  # Joining
  df <- full_join(df, df_low_use, by = colnames(df_low_use))

  colnames(df) <- str_to_lower(paste('pass',colnames(df), sep = "_")) %>%
                  str_replace("%","_per")

  df_rush <- "https://www.footballoutsiders.com/stats/nfl/qb/2020" %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="node-76122"]/div/div/table[3]') %>%
    html_table() %>%
    .[[1]] %>%
    .[,-c(4,6)]
  colnames(df_rush) <- str_to_lower(paste('rush',colnames(df_rush), sep = "_"))

  # Merge ----
  df_all <- left_join(df, df_rush, by = c("pass_player" = "rush_player", "pass_team" = "rush_team"))

  # Removing % from DVOA
  df_all[,c(3:ncol(df_all))] <- apply(df_all[,c(3:ncol(df_all))], 2, function(x) {as.numeric(str_remove_all(as.character(x), ",|%"))})

  return(df_all)
}

fo_rb <- function() {


  ## Read in data from rushing and receiving ----
  df <- "https://www.footballoutsiders.com/stats/nfl/rb/2020" %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="node-76123"]/div/div/table[1]') %>%
    html_table() %>%
    .[[1]] %>%
    .[,-c(4,6,8,16)] %>%
    select(-Runs, -TD, -FUM)

  colnames(df) <- str_to_lower(paste('rush',colnames(df), sep = "_")) %>%
                  str_remove_all("\t") %>% str_replace("\n", "_")

  # Receiving Yards
  df_rec <- "https://www.footballoutsiders.com/stats/nfl/rb/2020" %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="node-76123"]/div/div/table[3]') %>%
    html_table() %>%
    .[[1]] %>%
    .[,-c(4,6,8)] %>%
    select(-Passes, -TD, -FUM)
  colnames(df_rec) <- str_to_lower(paste('rec', colnames(df_rec), sep = "_")) %>%
                      str_remove_all("\t") %>% str_replace("\n", "_")

  # Merge ----
  df_all <- left_join(df, df_rec, by = c("rush_player" = "rec_player", "rush_team" = "rec_team"))

  # Removing % from DVOA
  df_all[,c(3:ncol(df_all))] <- apply(df_all[,c(3:ncol(df_all))], 2, function(x) {as.numeric(str_remove_all(as.character(x), ",|%"))})

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
        .[,-c(4,6,8)]

  df2 <- paste('https://www.footballoutsiders.com/stats/nfl/',
               position,
               '/2020', sep = "") %>%
        read_html() %>%
        html_nodes(xpath = paste('//*[@id="node-', as.character(num), '"]/div/div/table[2]',sep = "")) %>%
        html_table() %>%
        .[[1]]

  # Adding in second table
  df3 <- full_join(df, df2, by = colnames(df2)) %>% select(-Passes, -TD, -FUM, -DPI) %>% mutate(Player = str_replace(Player, "Jo\\.", "J."))

  colnames(df3) <- str_to_lower(paste('rec',colnames(df3), sep = "_")) %>%
                   str_remove_all("\t") %>% str_replace("\n", "_")

  # Removing % from DVOA
  df3[,c(3:ncol(df3))] <- apply(df3[,c(3:ncol(df3))], 2, function(x) {as.numeric(str_remove_all(as.character(x), ",|%"))})

  return(df3)

}

# Getting all data
# qb_df <- fo_qb()
# rb_df <- fo_rb()
# wr_df <- fo_pass_catchers("wr")
# te_df <- fo_pass_catchers("te")

# Merging into 1 DF
# df_all <- full_join(qb_df, rb_df, by = c("pass_player" = "rush_player")) %>%
#           full_join(wr_df, by = c("pass_player" = "rec_player")) %>%
#           full_join(te_df, by = c("pass_player" = "rec_player")) %>%
#           mutate(pass_player = ifelse(is.na(str_extract(pass_player, "^[:upper:]\\.[:upper:]\\.")) == T,
#                             pass_player,
#                             str_remove(pass_player, "\\.")),
#           pass_player = str_replace(pass_player, "[:punct:]", " "),
#           pass_player = str_remove_all(pass_player, "-"))
# df_all[["pass_team"]] <- sapply(df_all[["pass_team"]], function(x) find_names(x, "fff_abbreviation"))
#
# # Merging with main file
# #Read in DF
# d <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_3_2020.xlsx") %>%
#   mutate(proj_player_new = str_remove_all(proj_player, "(?<=[:upper:])[:alpha:]{1,}(?=[:space:])"))
#
#
# # Combining
# combined_df <- left_join(d, df_all, by = c("proj_player_new" = "pass_player", "proj_tm" = "pass_team")) %>%
#   select(-proj_player_new)
#
# writexl::write_xlsx(combined_df, "C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_3_2020_advanced_FO.xlsx")



