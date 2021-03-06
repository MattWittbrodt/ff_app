
# Getting Advanced Football Outsiders Metrics for each player
library(tidyverse)
library(rvest)

#
# QB Football Outsider stats
#

fo_qb <- function(login) {

  ## Read in data from regular and rushing ----
  df <- login %>%
        jump_to("https://www.footballoutsiders.com/stats/nfl/qb/2020") %>%
        read_html()

  # Fixing issue with the content being hidden behind a comment. Returns a list of 3
  df <- gsub("!--","",df)
  df_list <- read_html(df) %>% html_table(fill = T)

  ## First passing stats
  df <- df_list[[1]]
  colnames(df) <- fo_col_to_rank(colnames(df)) # adding Rk to columns

  df <- df %>% .[,-c(3,5,7)] %>% select(-DPI) %>%
    mutate(DYAR = as.numeric(str_remove_all(DYAR, ",")),
           Yards = as.numeric(str_remove_all(Yards, ",")),
           EYds = as.numeric(str_remove_all(EYds, ",")))

  # This is going to get some of the lower used QBs but they may still be relevant
  df_low_use <- df_list[[2]]
  colnames(df_low_use) <- fo_col_to_rank(colnames(df_low_use)) # adding Rk to columns

  df_low_use <- df_low_use %>% select(-DPI) %>% mutate(Yards = as.numeric(str_remove_all(Yards, ",")),
                                                       EYds = as.numeric(str_remove_all(EYds, ",")),
                                                       DYAR = as.numeric(str_remove_all(DYAR, ",")))

  # Joining passing stats
  df <- full_join(df, df_low_use, by = colnames(df_low_use))

  colnames(df) <- str_to_lower(paste('pass',colnames(df), sep = "_")) %>%
                  str_replace("%","_per")

  # Adding in rushing stats
  df_rush <- df_list[[3]]
  colnames(df_rush) <- fo_col_to_rank(colnames(df_rush)) # adding Rk to columns

  df_rush <- df_rush[,-c(3,5)]

  colnames(df_rush) <- str_to_lower(paste('rush',colnames(df_rush), sep = "_"))

  # Merge ----
  df_all <- left_join(df, df_rush, by = c("pass_player" = "rush_player", "pass_team" = "rush_team")) %>%
            select(-ends_with("_rk")) %>%
            mutate(pass_player = str_remove_all(pass_player, "[:digit:]|-"))

  # Removing % from DVOA
  df_all[,c(3:ncol(df_all))] <- apply(df_all[,c(3:ncol(df_all))], 2, function(x) {as.numeric(str_remove_all(as.character(x), ",|%"))})

  return(df_all)
}

#
# RB Football Outsider Stats
#

fo_rb <- function(login) {

  ## Read in data from rushing and receiving ----
  df <- login %>% jump_to("https://www.footballoutsiders.com/stats/nfl/rb/2020") %>% read_html()

  # Fixing issue with the content being hidden behind a comment. Returns a list of 3
  df <- gsub("!--","",df)
  df_list <- read_html(df) %>% html_table(fill = T)

  # Main rushing table, loading in and making duplicate names as rk
  df <- df_list[[1]]
  colnames(df) <- fo_col_to_rank(colnames(df))

  df <- df %>% .[,-c(3,5,7,17)] %>%
        select(-Runs, -TD, -FUM) %>%
        mutate(Yards = as.numeric(as.character(str_remove_all(Yards,","))),
               EYds = as.numeric(as.character(str_remove_all(EYds, ","))))

  colnames(df) <- str_to_lower(paste('rush',colnames(df), sep = "_")) %>%
                  str_remove_all("\t") %>% str_replace("[:space:]", "_")

  # Receiving Yards - main table (#3)
  df_rec <- df_list[[3]]
  colnames(df_rec) <- fo_col_to_rank(colnames(df_rec))

  df_rec <- df_rec %>% .[,-c(3,5,7)] %>%
            select(-Passes, -TD, -FUM) %>%
            mutate(Yards = as.numeric(as.character(str_remove_all(Yards,","))),
                   EYds = as.numeric(as.character(str_remove_all(EYds, ","))))

  colnames(df_rec) <- str_to_lower(paste('rec', colnames(df_rec), sep = "_")) %>%
                      str_remove_all("\t") %>% str_replace_all("[:space:]", "_")

  # Merge ----
  df_all <- left_join(df, df_rec, by = c("rush_player" = "rec_player", "rush_team" = "rec_team")) %>%
            select(-ends_with("_rk")) %>%
            mutate(rush_player = str_remove_all(rush_player, "^[[:digit:]|-]{1,4}"),
                   rush_player = str_replace(rush_player, "(?<=[:upper:])\\.(?=[:upper:])", "\\. "))

  # Removing % from DVOA
  df_all[,c(3:ncol(df_all))] <- apply(df_all[,c(3:ncol(df_all))], 2, function(x) {as.numeric(str_remove_all(as.character(x), ",|%"))})

  return(df_all)
}

#
# WR Football Outsider Stats
#

fo_pass_catchers <- function(position, login) {

  ## Read in data for receiving ----
  df <- login %>% jump_to(paste0("https://www.footballoutsiders.com/stats/nfl/",position,"/2020")) %>% read_html()

  # Fixing issue with the content being hidden behind a comment. Returns a list of 3
  df <- gsub("!--","",df)
  df_list <- read_html(df) %>% html_table(fill = T)

  # Main receiving table
  df <- df_list[[1]]
  colnames(df) <- fo_col_to_rank(colnames(df))
  df <- df[,-c(3,5,7)]

  # Getting second table
  df2 <- df_list[[2]]
  colnames(df2) <- fo_col_to_rank(colnames(df2))
  df2 <- df2[,-c(3,5,7)]

  # Adding in second table
  df3 <- full_join(df, df2, by = colnames(df2)) %>%
         select(-Passes, -TD, -FUM, -DPI) %>%
         mutate(Player = str_replace(Player, "Jo\\.", "J."),
                Player = str_remove_all(Player, "[:digit:]|-"),
                Yards = as.numeric(as.character(str_remove_all(Yards,","))),
                EYds = as.numeric(as.character(str_remove_all(EYds, ","))))

  colnames(df3) <- str_to_lower(paste('rec',colnames(df3), sep = "_")) %>%
                   str_remove_all("\t") %>% str_replace_all("[:space:]", "_")

  # Removing % from DVOA
  df3[,c(3:ncol(df3))] <- apply(df3[,c(3:ncol(df3))], 2, function(x) {as.numeric(str_remove_all(as.character(x), ",|%"))})

  df3 <- select(df3, -ends_with("_rk"))

  return(df3)

}

