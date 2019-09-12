# Function for extracting performance data

#wk_num = 2
#position = "QB"

position_stats <- function(position,wk) {
  
  library(tidyverse)
  library(stringr)
  library(rvest)
  
  # Reading in team name chart
  tm_names <- readxl::read_xlsx("~/ff_shiny_app/ff_app/data/team_names.xlsx")
  
  #function for use later
  find_names <- function(x,col) {
    
    # Checking if the opponent is listed as '@' - or away
    if(str_detect(x,"^@") == T) {
    
      # Get name
      d <- filter(tm_names, tm_names[[col]] == str_extract(x,"(?<=@)\\w+")) %>%
           .[["pfr_abbreviation"]]
      d <- paste("@",d,sep = "") # Adding back the away
    } 
    else
    {
      # Get name
      d <- filter(tm_names, tm_names[[col]] == x) %>%
           .[["pfr_abbreviation"]]
    } 
    return(d)
  }
  
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
  colnames(wk_data) <- wk_data[1,]
  
  # Columns to include based on position
  columns <- switch(position,
                    QB = c())
  
  # Selecting relevant columns
  wk_data <- wk_data[-1,c(2,3,7:9,14:28,32:36)] %>%
             subset(Player != 'Player')
  
  # Fixing a few column names
  colnames(wk_data) <- c("player","position", "tm","field","opponent","pass_comp","pass_att","comp_percentage",
                         "pass_yd","pass_td","int","qb_rating","sack","sk_yds","yds_per_att","adj_yds_att",
                         "rush_att","rush_yd","rush_yds_att","rush_td","fd_pts","rec","rec_yds","rec_td","fum") 
  
  # Making Home field = 1, visitor = 2
  wk_data$field <- ifelse(wk_data$field == "@",2,1)
  

# Red Zone Stats ----------------------------------------------------------

  # Reading in Red Zone stats
  rz_data <- "http://www.pro-football-reference.com/years/2018/redzone-passing.htm" %>%
             read_html() %>%
             html_table(fill = T) %>%
             .[[1]]
  
  # Fixing column names
  rz_names <- paste(colnames(rz_data), rz_data[1,]) %>%
              str_to_lower() %>%
              str_remove_all("inside") %>%
              str_remove_all("^\\s+") %>%
              str_replace_all("%","_per") %>%
              str_replace_all("20","twenty") %>%
              str_replace_all("10","ten") %>%
              str_replace_all("\\s+","_") %>%
              str_replace_all("cmp","comp")
  
  # Adding into new columns
  colnames(rz_data) <- rz_names
  
  rz_data <- rz_data %>%
             subset(player != "Player") %>%
             select(-link)
  

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

# Year to Date Stats ------------------------------------------------------
  
  ytd_data <- "http://www.pro-football-reference.com/years/2019/passing.htm" %>%
              read_html() %>%
              html_table(fill = T) %>%
              .[[1]] %>%
              .[,-1] %>%
              subset(Pos == position) %>%
              select(-Age, -GS, -QBrec, -Lng, -Yds.1)
              
              
  # Converting data into numeric
  ytd_data[,c(4:length(ytd_data))] <- apply(ytd_data[,c(4:length(ytd_data))], 
                                            2, 
                                            function(x) as.numeric(as.character(x)))
  
  ## TODO: subset > 2 (when possible) ytd <- subset(ytd, passing.G > 2)
  
  # Fixing Column names
  ytd_names <- colnames(ytd_data) %>%
    str_to_lower() %>%
    str_replace_all("%","_per") %>%
    str_replace_all("cmp","comp") %>%
    str_replace("any/a", "avg_net_yds_per_att") %>%
    str_replace("ay/a", "avg_yds_per_att") %>%
    str_replace("ny/a", "net_yds_per_att") %>%
    str_replace("y/a", "yds_per_att") %>%
    str_replace("y/g", "yds_per_gm") %>%
    str_replace("y/c", "yds_per_completion")
  
  colnames(ytd_data) <- paste("ytd", ytd_names, sep = "_")  
  
  # Making a few columns per game
  ytd_data <- ytd_data %>%
              mutate(ytd_comp = ytd_comp / ytd_g,
                     ytd_att = ytd_att / ytd_g,
                     ytd_yds = ytd_yds / ytd_g,
                     ytd_td = ytd_td / ytd_g,
                     ytd_int = ytd_int / ytd_g)
  

# Merging all data --------------------------------------------------------
  
  all_data <- inner_join(wk_data, ytd_data, by = c("player"= "ytd_player", 
                                                   "tm" = "ytd_tm",
                                                   "position" = "ytd_pos")) %>%
              inner_join(rz_data, by = c("player","tm")) %>%
              inner_join(all_d_data, by = c("opponent" = "tm"))
  

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
                                                   "position" = "proj_pos"))
  

# Return ------------------------------------------------------------------

  return(all_data)  
}
