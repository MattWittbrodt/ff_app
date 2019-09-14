# Function for extracting performance data

#wk_num = 2
#position = "QB"

position_stats <- function(position, wk_num) {
  
  library(tidyverse)
  library(stringr)
  library(rvest)
  
  # Reading in team name chart
  tm_names <- readxl::read_xlsx("~/ff_shiny_app/ff_app/data/team_names.xlsx")
  source("~/ff_shiny_app/ff_app/find_names.R")
  
# Previous Week Data ------------------------------------------------------

  wk_data <-  paste("https://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=2019&year_max=2019&season_start=1&season_end=-1&pos%5B%5D=",
                   position,
                   "&is_starter=E&game_type=R&career_game_num_min=0&career_game_num_max=499&qb_start_num_min=1&qb_start_num_max=400&game_num_min=0&game_num_max=99&week_num_min=",
                   as.character(wk_num - 1),
                   "&week_num_max=",
                   as.character(wk_num -1),
                   "&c1stat=rush_att&c1comp=gt&c1val=0&c2stat=fanduel_points&c2comp=gt&c3stat=rush_att&c3comp=gt&c4stat=targets&c4comp=gt&c4val=0&c5val=1.0&order_by=pass_rating",
                   sep = "") %>%                
             read_html() %>%
             html_table(fill = T) %>%
            .[[1]]
  
  # Getting actual column names
  wk_names <- paste(colnames(wk_data), wk_data[1,]) %>%
              str_to_lower() %>%
              str_remove_all("^\\s+") %>%
              str_remove_all("[.]\\d+") %>%
              str_remove_all("^\\s+") %>%
              str_remove("ing") %>%
              str_replace("y/r","yds_per_rec") %>%
              str_replace("y/a","yds_per_att") %>%
              str_replace("y/tgt","yds_per_tgt") %>%
              str_replace_all("cmp","comp") %>%
              str_replace_all("%","_per") %>%
              str_replace_all("\\s+","_")
  wk_names[8] <- 'field'
  
  colnames(wk_data) <- wk_names
  
  # Columns to include based on position
  columns <- switch(position,
                    QB = c(2,3,7:9,14:28,39),
                    RB = c(2,3,7:9,25:35,39),
                    WR = c(2,3,7:9,29:35,39),
                    TE = c(2,3,7:9,29:35,39))
  
  # Selecting relevant columns
  wk_data <- wk_data[-1,columns] %>%
             subset(player != 'Player')
  
  # Making Home field = 1, visitor = 2
  wk_data[['field']] <- ifelse(wk_data[['field']] == "@",2,1)
  

# Red Zone Stats ----------------------------------------------------------
  
  rz_types <- list("passing","rushing","receiving")
  
  rz_df <- lapply(rz_types,
                    function(x) {
                      
                      # Reading in Data
                      d <- paste("http://www.pro-football-reference.com/years/2019/redzone-",
                                 x,
                                 ".htm",
                                 sep = "") %>%
                            read_html() %>%
                            html_table(fill = T) %>%
                            .[[1]]
                      
                      # Ammending names
                      rz_names <- paste(colnames(d), d[1,]) %>%
                        str_to_lower() %>%
                        str_remove_all("inside") %>%
                        str_remove_all("^\\s+") %>%
                        str_replace_all("20","twenty") %>%
                        str_replace_all("10","ten") %>%
                        str_replace_all("5","five") %>%
                        str_replace_all("(?<=\\w)%","_per") %>%
                        str_replace_all("%(?=\\w)","per_") %>%
                        str_replace_all("\\s+","_") %>%
                        str_replace_all("cmp","comp")
                      
                      
                      # Adding into new columns
                      colnames(d) <- paste(x,rz_names,sep = "_")
                      
                      # Final few cleaning steps
                      d <- d %>%
                        subset(.[[paste(x,"player",sep = "_")]] != "Player") %>%
                        select(-contains("link"))
                      
                    })
  
  # Creating the appropriate output for each position
  rz_data <- switch(position,
                    QB = as.data.frame(rz_df[[1]]),
                    WR = as.data.frame(rz_df[[3]]),
                    TE = as.data.frame(rz_df[[3]]),
                    RB = left_join(as.data.frame(rz_df[[2]]), 
                                   as.data.frame(rz_df[[3]]),
                                   by = c("rushing_player" = "receiving_player")))
  

# Team Defense Integration ------------------------------------------------
  
  # Sub tables are commented out; need to read in and then eliminate comments
  d_data <- "https://www.pro-football-reference.com/years/2019/opp.htm" %>%
             read_html()
  
  d_data <-  gsub("!--","", d_data)
  
  d_data <-  read_html(d_data) %>% html_table(fill = T)
  
  #
  # Team Defense
  #
  team_d <- d_data[[1]] %>%
            .[,-c(1,3)]
  
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
    str_remove("ing")
  
  colnames(team_d) <- team_d_names
  
  # Removing first row with column name information
  team_d <- team_d[-1,] %>%
            subset(tm != "Avg Team" & tm != "League Total" & tm != "Avg Tm/G") %>%
            select(-starts_with("rush"),-starts_with("pass"))
  
  #
  # Conversions Against
  #
  
  conversion_d <- d_data[[7]] %>% .[,-1]
    
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
            select(-g)
    
  #
  # Rushing D
  #
  
  rush_d <- d_data[[3]] %>% .[,-c(1,3)]
  
  rush_d_names <- colnames(rush_d) %>%
                  str_to_lower() %>%
                  str_replace("y/a", "yds_per_att") %>%
                  str_replace("y/g", "yds_per_gm")
  
  colnames(rush_d) <- paste("rush", rush_d_names, sep = "_")
  
  rush_d <- rush_d %>% subset(rush_tm != "Avg Team" & rush_tm != "League Total" & rush_tm != "Avg Tm/G")
  
  #
  # Passing D
  #
  
  pass_d <- d_data[[2]] %>% .[,-c(1,3)]
  
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
  
  pass_d <- pass_d %>% subset(pass_tm != "Avg Team" & pass_tm != "League Total" & pass_tm != "Avg Tm/G")
  
  
  ## Merging into one dataframe
  all_d_data <- inner_join(team_d, conversion_d, by = "tm") %>%
                inner_join(rush_d, by = c("tm" = "rush_tm")) %>%
                inner_join(pass_d, by = c("tm" = "pass_tm")) 
  
  # Making abbreviations
  all_d_data[["tm"]] <- sapply(all_d_data[["tm"]], function(x) find_names(x, "full_name"))
  
  # Adding defense for merging with other datasets
  colnames(all_d_data) <- paste("def", colnames(all_d_data), sep = "_")

# Year to Date Stats ------------------------------------------------------
  
  ytd_types <- list("passing","rushing","receiving")
  
  ytd_df <- lapply(ytd_types, function(x) {
                                     
                                     # Reading in Data
                                     d <- paste("http://www.pro-football-reference.com/years/2019/",
                                                x,
                                                ".htm",
                                                sep = "") %>%
                                       read_html() %>%
                                       html_table(fill = T) %>%
                                       .[[1]] %>%
                                       .[,-1]
                                     
                                      if(x == "passing" | x == "receiving") {
                                        d <- d  %>%
                                             subset(Pos != "Pos") %>%
                                             select(-Age, -GS, -Lng)
                                      } else {
                                        
                                        # Rush is divided into two rows
                                        colnames(d) <- d[1,]
                                        d <- d[-1,] %>%
                                             subset(Pos != "Pos" & as.numeric(Att) > 4) %>%
                                             select(-Age, -GS, -Lng)
                                        
                                      }
                                     
                                     # Ammending names
                                     d_names <- colnames(d) %>%
                                       str_to_lower() %>%
                                       str_replace_all("%","_per") %>%
                                       str_replace_all("cmp","comp") %>%
                                       str_replace("any/a", "avg_net_yds_per_att") %>%
                                       str_replace("ay/a", "avg_yds_per_att") %>%
                                       str_replace("ny/a", "net_yds_per_att") %>%
                                       str_replace("y/a", "yds_per_att") %>%
                                       str_replace("y/g", "yds_per_gm") %>%
                                       str_replace("y/c", "yds_per_completion") %>%
                                       str_replace("tgt", "target") %>%
                                       str_replace("r/g", "rec_per_gm") %>%
                                       str_replace("y/g", "yds_per_gm") %>%
                                       str_replace("y/r", "yds_per_rec") %>%
                                       str_replace("y/target", "yds_per_target")
                                     
                                     # Adding prefix to column names
                                     if(x == "rushing") {
                                       colnames(d) <- paste("ytd_rush", d_names, sep = "_")
                                     } else {
                                       if(x == "receiving") {
                                       colnames(d) <- paste("ytd_rec", d_names, sep = "_")
                                       } else {
                                       colnames(d) <- paste("ytd_pass", d_names, sep = "_")
                                       }
                                     }
                                     
                                     # Converting the catch % into numeric
                                     if(x == "receiving") {
                                       d[["ytd_rec_ctch_per"]] <- as.numeric(sapply(d[["ytd_rec_ctch_per"]],
                                                                          function(x) str_remove(x, '%')))
                                     }
                                     
                                     # Making Columns numeric
                                     d[,c(4:length(d))] <- apply(d[,c(4:length(d))], 
                                                                 2, 
                                                                 function(x) as.numeric(as.character(x)))
                                     
                                     # Making a few columns per game
                                     
                                       if(x == "passing") {
                                         d <- mutate(d,
                                                     ytd_pass_comp = ytd_pass_comp / ytd_pass_g,
                                                     ytd_pass_att = ytd_pass_att / ytd_pass_g,
                                                     ytd_pass_yds = ytd_pass_yds / ytd_pass_g,
                                                     ytd_pass_td = ytd_pass_td / ytd_pass_g,
                                                     ytd_pass_int = ytd_pass_int / ytd_pass_g)
                                        } else {
                                          if(x == "receiving") {
                                            d <- mutate(d,
                                                    ytd_rec_target = ytd_rec_target / ytd_rec_g,
                                                    ytd_rec_rec = ytd_rec_rec / ytd_rec_g,
                                                    ytd_rec_yds = ytd_rec_yds / ytd_rec_g,
                                                    ytd_rec_td = ytd_rec_td / ytd_rec_g)
                                          } else {
                                            d <- mutate(d,
                                                        ytd_rush_att = ytd_rush_att / ytd_rush_g,
                                                        ytd_rush_yds = ytd_rush_yds / ytd_rush_g,
                                                        ytd_rush_td = ytd_rush_td / ytd_rush_g)
                                       }}
                                     
                              })
  
  
  # Picking data which is relevant for the player - similar to red zone
  ytd_data <- switch(position,
                    QB = as.data.frame(ytd_df[[1]]),
                    WR = as.data.frame(ytd_df[[3]]),
                    TE = as.data.frame(ytd_df[[3]]),
                    RB = left_join(as.data.frame(ytd_df[[2]]), 
                                   as.data.frame(ytd_df[[3]]),
                                   by = c("ytd_rush_player" = "ytd_rec_player")))
  

# Merging all data --------------------------------------------------------
  
  if(position == "QB") {
    
    all_data <- inner_join(wk_data, ytd_data, by = c("player"= "ytd_pass_player", 
                                                     "tm" = "ytd_pass_tm",
                                                     "pos" = "ytd_pass_pos")) %>%
                inner_join(rz_data, by = c("player" = "passing_player",
                                           "tm" = "passing_tm")) %>%
                inner_join(all_d_data, by = c("opp" = "def_tm"))
    
  } else {
    if(position == "RB") {
      
      all_data <- left_join(wk_data, ytd_data, by = c("player" = "ytd_rush_player",
                                                       "tm" = "ytd_rush_tm",
                                                       "pos" = "ytd_rush_pos")) %>%
                  left_join(rz_data, by = c("player" = "rushing_player",
                                   "tm" = "rushing_tm")) %>%
                  left_join(all_d_data, by = c("opp" = "def_tm"))
      
    } else {
        
      all_data <- left_join(wk_data, ytd_data, by = c("player"= "ytd_rec_player", 
                                                       "tm" = "ytd_rec_tm",
                                                       "pos" = "ytd_rec_pos")) %>%
                  left_join(rz_data, by = c("player" = "receiving_player",
                                   "tm" = "receiving_tm")) %>%
                  left_join(all_d_data, by = c("opp" = "def_tm"))
      
      }
  }

# Current Week Projections ------------------------------------------------

  proj_data <- read.csv(paste("~/ff_shiny_app/ff_app/data/4for4_W",wk_num,"_projections.csv", sep = "")) %>% 
              subset(Pos == position) %>%
              select(-PID,-Season,-XP,-Fum,-FG,-Grade,-Pa1D,-Ru1D,-Rec1D) %>%
              mutate(Player = as.character(Player),
                     Pos = as.character(Pos),
                     Team = as.character(Team),
                     Opp = as.character(Opp)) 
  
  # Column Processing
  proj_names <- colnames(proj_data) %>%
                str_to_lower() %>%
                str_replace("[.]","_") %>%
                str_replace("team", "tm")
  
  colnames(proj_data) <- paste("proj", proj_names, sep = "_") 
  
  # Making abbreviations
  proj_data[["proj_tm"]] <- sapply(proj_data[["proj_tm"]], function(x) find_names(x, "fff_abbreviation"))
  proj_data[["proj_opp"]] <- sapply(proj_data[["proj_opp"]], function(x) find_names(x, "fff_abbreviation"))
  
  # Adding to full dataframe
  all_data <- inner_join(all_data, proj_data, by = c("player"= "proj_player", 
                                                   "tm" = "proj_tm",
                                                   "pos" = "proj_pos")) %>%
              mutate(week = wk_num)
  

# Return ------------------------------------------------------------------

  return(all_data)  
}
