# Function for extracting performance data

#wk_num = 2
#position = "QB"

position_stats <- function(position, wk_num, data, tm_names) {
  
  library(tidyverse)
  library(stringr)
  library(rvest)
  
  # Reading in team name chart
  #tm_names <- readxl::read_xlsx("data/team_names.xlsx")
  source("~/ff_shiny_app/ff_app/find_names.R", local = T)
  
# Previous Week Data ------------------------------------------------------

  wk_data <- lapply(data$wk_data, function(x) {
  
              # Getting actual column names
              wkdf <- x
              wk_names <- paste(colnames(wkdf), wkdf[1,]) %>%
                          str_to_lower() %>%
                          str_remove_all("^\\s+") %>%
                          str_remove_all("[.]\\d+") %>%
                          str_remove_all("^\\s+") %>%
                          str_replace("receiving", "rec") %>%
                          str_remove("ing") %>%
                          str_replace("y/r","yds_per_rec") %>%
                          str_replace("y/a","yds_per_att") %>%
                          str_replace("y/tgt","yds_per_tgt") %>%
                          str_replace_all("cmp","comp") %>%
                          str_replace_all("%","_per") %>%
                          str_replace_all("\\s+","_")
              wk_names[8] <- 'field'
              
              colnames(wkdf) <- paste("prev_wk", wk_names, sep = "_")
              
              return(wkdf)
              })
  
  # Columns to include based on position, remove first column, then remove the 'Player' column
  wk_data2 <- switch(position,
                    QB = wk_data[[1]][,c(2,3,7:9,14:17,28:33)],
                    RB = wk_data[[2]][,c(2,3,7:9,14:24,28)],
                    WR = wk_data[[3]][,c(2,3,7:9,18:24,28)],
                    TE = wk_data[[4]][,c(2,3,7:9,18:24,28)]) %>%
              .[-1,] %>% 
              subset(prev_wk_player != 'Player') %>%
              mutate(prev_wk_field = ifelse(prev_wk_field == "@",2,1),
                     prev_wk_player = str_remove_all(prev_wk_player, "[:punct:]"))
  
  # Removing the '%' from columns
  if(position != "QB") {
     wk_data2[["prev_wk_rec_ctch_per"]] <- as.numeric(sapply(wk_data2[["prev_wk_rec_ctch_per"]],
                                                    function(x) str_remove(x, '%')))}
  # converting into numeric
  wk_data2[,c(6:length(wk_data2))] <- apply(wk_data2[,c(6:length(wk_data2))], 
                              2, 
                              function(x) as.numeric(as.character(x)))
  
  # Doing some subsetting to remove players without any data
  if(position == "QB") {
    wk_data2 <- filter(wk_data2, prev_wk_pass_att > 5)
  } else {
    if(position == "RB") {
    wk_data2 <- filter(wk_data2, prev_wk_rush_att > 4)
    } else {
      wk_data2 <- filter(wk_data2, prev_wk_rec_tgt > 1)
    }
  }

# Red Zone Stats ----------------------------------------------------------
  
  rz_df <- data$rz_df
  
  # Creating the appropriate output for each position
  rz_data <- switch(position,
                    QB = as.data.frame(rz_df[[1]]),
                    WR = as.data.frame(rz_df[[3]]),
                    TE = as.data.frame(rz_df[[3]]),
                    RB = left_join(as.data.frame(rz_df[[2]]), 
                                   as.data.frame(rz_df[[3]]),
                                   by = c("rushing_player" = "receiving_player"))) %>%
             select(-contains("tm"))
  
  # Subsetting for relevant information
  if(position == "RB") {
    rz_data <- select(rz_data, -contains("_ctch_per")) %>%
               mutate(rushing_twenty_per_rush = as.numeric(sapply(rushing_twenty_per_rush,
                                                     function(x) str_remove(x, '%'))),
                      rushing_ten_per_rush = as.numeric(sapply(rushing_ten_per_rush,
                                              function(x) str_remove(x, '%'))),
                      rushing_five_per_rush = as.numeric(sapply(rushing_five_per_rush,
                                                function(x) str_remove(x, '%'))),
                      receiving_twenty_per_tgt = as.numeric(sapply(receiving_twenty_per_tgt,
                                                function(x) str_remove(x, '%'))),
                      receiving_ten_per_tgt = as.numeric(sapply(receiving_ten_per_tgt,
                                                  function(x) str_remove(x, '%'))))
  } else {
      if(position == "WR" | position == "TE") {
        rz_data <- select(rz_data, -contains("_ctch_per")) %>%
          mutate(receiving_twenty_per_tgt = as.numeric(sapply(receiving_twenty_per_tgt,
                                                              function(x) str_remove(x, '%'))),
                 receiving_ten_per_tgt = as.numeric(sapply(receiving_ten_per_tgt,
                                                           function(x) str_remove(x, '%'))),
                 receiving_player = str_remove_all(receiving_player, "[:punct:]"))
      } 
  }
  
  # Making columns numeric
  rz_data[,c(2:length(rz_data))] <- apply(rz_data[,c(2:length(rz_data))], 
                                          2, 
                                          function(x) as.numeric(as.character(x)))

# Team Defense Integration ------------------------------------------------
  
  # Sub tables are commented out; need to read in and then eliminate comments
  d_data <- "https://www.pro-football-reference.com/years/2019/opp.htm" %>%
             read_html()
  
  d_data <-  gsub("!--","", d_data)
  
  d_data <-  read_html(d_data) %>% html_table(fill = T)
  
  #
  # Team Defense
  #
  team_d <- d_data[[1]] %>% .[,-1]
  
  # Hanlding Column Names
  team_d_names <- paste(colnames(team_d), team_d[1,]) %>%
    str_to_lower() %>%
    str_remove_all("^\\s+") %>%
    str_remove_all("[.][\\d]") %>%
    str_replace("^\\s+","tot_") %>%
    str_remove_all("& to") %>%
    str_replace_all("tot\\s+yds","tot") %>%
    str_replace("y/p","yds_per_play") %>%
    str_replace("y/a","yds_per_att") %>%
    str_replace_all("\\s+","_") %>%
    str_replace_all("cmp","comp") %>%
    str_replace_all("(1std|1stpy)","first_dn") %>%
    str_remove("ing") %>%
    str_replace_all("%","_per")
  
  colnames(team_d) <- team_d_names
  
  # Removing first row with column name information
  team_d <- team_d[-1,] %>%
            subset(tm != "Avg Team" & tm != "League Total" & tm != "Avg Tm/G") %>%
            select(-starts_with("rush"),
                   -starts_with("pass"),
                   -tot_fl,
                   -tot_ply,
                   -penalties_first_dn,
                   -penalties_yds)
  
  # Making columns numeric
  team_d[,c(2:length(team_d))] <- apply(team_d[,c(2:length(team_d))], 
                                          2, 
                                          function(x) as.numeric(as.character(x)))

  # team_d <- team_d %>% 
  #           mutate(pass_td = pass_td / tot_g,
  #                  pass_att = pass_att / tot_g,
  #                  pass_yds = pass_yds / tot_g,
  #                  pass_comp_per = pass_comp / pass_att) %>%
  #           select(-tot_g)  
  
  #
  # Conversions Against
  #
  
  conversion_d <- d_data[[8]] %>% .[,-1]
    
  # Column names
  conversion_d_names <- paste(colnames(conversion_d), conversion_d[1,]) %>%
    str_to_lower() %>%
    str_remove_all("^\\s+") %>%
    str_remove_all("[.][\\d]") %>%
    str_remove("^\\s+") %>%
    str_remove_all("downs\\s+") %>%
    str_replace_all("tot\\s+yds","tot") %>%
    str_replace("3","third_") %>%
    str_replace("4","fourth_") %>%
    str_replace_all("%","_per") %>%
    str_replace("rz","red_zone_") %>%
    str_remove("red zone\\s+")
  
  colnames(conversion_d) <- conversion_d_names
  
  # Removing first row with column name information
  conversion_d <- conversion_d[-1,] %>%
            subset(tm != "Avg Team" & tm != "League Total" & tm != "Avg Tm/G") %>%
            select(-g,-contains("datt"),-contains("dconv"),-red_zone_att) %>%
            mutate(third_d_per = as.numeric(sapply(third_d_per, function(x) str_remove(x, '%'))),
                   fourth_d_per = as.numeric(sapply(fourth_d_per, function(x) str_remove(x, '%'))),
                   red_zone_pct = as.numeric(sapply(red_zone_pct, function(x) str_remove(x, '%'))),
                   red_zone_td = as.numeric(red_zone_td))
  
  #
  # Rushing D
  #
  
  rush_d <- d_data[[3]] %>% .[,-c(1,3)]
  
  rush_d_names <- colnames(rush_d) %>%
                  str_to_lower() %>%
                  str_replace("y/a", "yds_per_att") %>%
                  str_replace("y/g", "yds_per_gm")
  
  colnames(rush_d) <- paste("rush", rush_d_names, sep = "_")
  
  rush_d <- rush_d %>% 
            subset(rush_tm != "Avg Team" & rush_tm != "League Total" & rush_tm != "Avg Tm/G") %>%
            select(-rush_att, -rush_yds)
  
  #
  # Passing D
  #
  
  pass_d <- d_data[[3]] %>% .[,-c(1,3)]

  pass_d_names <- colnames(pass_d) %>%
                  str_to_lower() %>%
                  str_replace("y/a", "yds_per_att") %>%
                  str_replace("y/g", "yds_per_gm") %>%
                  str_replace("y/c", "yds_per_catch") %>%
                  str_replace("ay/a", "avg_yds_per_att") %>%
                  str_replace_all("cmp","comp") %>%
                  str_replace_all("%","_per") %>%
                  str_replace("pd","defended") %>%
                  str_replace("rate","qb_rating_allowed") %>%
                  str_replace("sk","sacks") %>%
                  str_replace("anyds","adj_net_yds") %>%
                  str_replace("nyds","net_yds") %>%
                  str_replace("yds.1","sack_yds")

  colnames(pass_d) <- paste("pass", pass_d_names, sep = "_")

  pass_d <- pass_d %>%
            subset(pass_tm != "Avg Team" & pass_tm != "League Total" & pass_tm != "Avg Tm/G") %>%
            select(-pass_comp,
                   -pass_att,
                   -pass_qbhits,
                   -pass_tfl,
                   -pass_sack_yds,
                   -pass_sacks_per)
  
  
  ## Merging into one dataframe
  all_d_data <- inner_join(team_d, conversion_d, by = "tm") %>%
                inner_join(rush_d, by = c("tm" = "rush_tm")) %>%
                inner_join(pass_d, by = c("tm" = "pass_tm")) 
  
  # Making abbreviations
  all_d_data[["tm"]] <- sapply(all_d_data[["tm"]], function(x) find_names(x, "full_name"))
  
  # Adding defense for merging with other datasets
  colnames(all_d_data) <- paste("def", colnames(all_d_data), sep = "_")

# Year to Date Stats ------------------------------------------------------
  
  ytd_df <- data$ytd_df
  
  # Picking data which is relevant for the player - similar to red zone
  ytd_data <- switch(position,
                    QB = as.data.frame(ytd_df[[1]]),
                    WR = as.data.frame(ytd_df[[3]]),
                    TE = as.data.frame(ytd_df[[3]]),
                    RB = left_join(as.data.frame(ytd_df[[2]]), 
                                   as.data.frame(ytd_df[[3]]),
                                   by = c("ytd_rush_player" = "ytd_rec_player")) %>% 
                         mutate(ytd_rush_pos = ifelse(ytd_rush_pos != "WR" & ytd_rush_pos != "QB" & ytd_rush_pos != "TE", "RB", ytd_rush_pos)))
  
# Current Week Projections ------------------------------------------------
  
  proj_data <- read.csv(paste("~/ff_shiny_app/ff_app/data/4for4_W",wk_num,"_projections.csv", sep = "")) %>% 
    subset(Pos == position) %>%
    select(-PID,-Season,-XP,-Fum,-FG,-Grade,-Pa1D,-Ru1D,-Rec1D) %>%
    mutate(Player = str_remove_all(as.character(Player), "[:punct:]"),
           Player = str_replace(as.character(Player), "Mitch", "Mitchell"),
           Pos = as.character(Pos),
           Team = as.character(Team),
           field = ifelse(str_detect(Opp, "@") == T, 2, 1),
           Opp = str_remove(Opp, "@")) %>%
  filter(Opp != "BYE")
  
  # Column Processing
  proj_names <- colnames(proj_data) %>%
    str_to_lower() %>%
    str_replace("[.]","_") %>%
    str_replace("team", "tm")
  
  colnames(proj_data) <- paste("proj", proj_names, sep = "_") 
  
  # Making abbreviations
  proj_data[["proj_tm"]] <- as.character(sapply(proj_data[["proj_tm"]], function(x) find_names(x, "fff_abbreviation")))
  proj_data[["proj_opp"]] <- as.character(sapply(proj_data[["proj_opp"]], function(x) find_names(x, "fff_abbreviation")))
  

# Fantasy Points Against Data ---------------------------------------------
  source("~/ff_shiny_app/ff_app/pts_against_function.R")
  pts_vs <- pts_against(position)
  
# Merging all data --------------------------------------------------------
  
  if(position == "QB") {
    
    all_data <- full_join(proj_data, wk_data2, by = c("proj_player" = "prev_wk_player",
                                                       "proj_tm" = "prev_wk_tm",
                                                       "proj_pos" = "prev_wk_pos")) %>%
                full_join(ytd_data, by = c("proj_player"= "ytd_pass_player", 
                                                     "proj_tm" = "ytd_pass_tm",
                                                     "proj_pos" = "ytd_pass_pos")) %>%
                left_join(rz_data, by = c("proj_player" = "passing_player")) %>% # look at inner join later - issues with RZ information currently
                inner_join(all_d_data, by = c("proj_opp" = "def_tm")) %>%
                left_join(pts_vs, by = c("proj_opp" = "pts_vs_tm")) %>%
                select(-ytd_pass_qbrec,
                       -ytd_pass_g,
                       -ytd_pass_comp,
                       -ytd_pass_att,
                       -ytd_pass_yds,
                       -ytd_pass_sk,
                       -ytd_pass_yds.1,
                       -ytd_pass_sk_per,
                       -ytd_pass_4qc,
                       -ytd_pass_gwd) %>%
                filter(proj_ffpts > 0)
    
  } else {
    if(position == "RB") {
      
      all_data <- full_join(proj_data, wk_data2, by = c("proj_player" = "prev_wk_player",
                                                         "proj_tm" = "prev_wk_tm",
                                                         "proj_pos" = "prev_wk_pos")) %>%
                  full_join(ytd_data, by = c("proj_player" = "ytd_rush_player",
                                                       "proj_tm" = "ytd_rush_tm",
                                                       "proj_pos" = "ytd_rush_pos")) %>%
                  left_join(rz_data, by = c("proj_player" = "rushing_player")) %>%
                  left_join(all_d_data, by = c("proj_opp" = "def_tm")) %>%
                  left_join(pts_vs, by = c("proj_opp" = "pts_vs_tm")) %>%
                  filter(proj_ffpts > 0)
      
    } else {
       
      all_data <- full_join(proj_data, wk_data2, by = c("proj_player" = "prev_wk_player",
                                                         "proj_tm" = "prev_wk_tm",
                                                         "proj_pos" = "prev_wk_pos")) %>%
                  full_join(ytd_data, by = c("proj_player"= "ytd_rec_player", 
                                                       "proj_tm" = "ytd_rec_tm",
                                                       "proj_pos" = "ytd_rec_pos")) %>%
                  left_join(rz_data, by = c("proj_player" = "receiving_player")) %>%
                  left_join(all_d_data, by = c("proj_opp" = "def_tm")) %>% 
                  left_join(pts_vs, by = c("proj_opp" = "pts_vs_tm")) %>%
                  filter(proj_ffpts > 0) #, proj_pos != "RB", proj_pos != "TE", proj_pos != "HB")
      }
  }


  

# Return ------------------------------------------------------------------

  return(all_data)  
}
