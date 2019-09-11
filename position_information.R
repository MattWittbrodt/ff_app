# Function for extracting performance data
wk_num = 2

position_stats <- function(position,wk) {
  
  library(tidyverse)
  library(stringr)
  library(rvest)
  
  ### Defensive links
  team_d_url <- 
  drive_d_url <- 

# Previous Week Data ------------------------------------------------------

  wk_data <- paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=2019&year_max=2019&season_start=1&season_end=-1&age_min=0&pos=0&league_id=&team_id=&opp_id=&career_game_num_min=0&career_game_num_max=499&game_num_min=0&game_num_max=99&week_num_min=",
                  as.character(wk_num-1), 
                  "&week_num_max=",
                  as.character(wk_num-1), 
                  "&stadium_id=&game_day_of_week=&game_month=&c1stat=pass_att&c1comp=gt&c1val=1&c2stat=fanduel_points&c2comp=gt&c3stat=rush_att&c3comp=gt&c4comp=gt&c5comp=choose&c5gtlt=lt&c6mult=1.0&c6comp=choose&order_by=pass_rating", sep = "") %>% 
            read_html() %>%
            html_table(fill = T) %>%
            .[[1]] 
  
  # Getting actual column names
  colnames(wk_data) <- wk_data[1,]
  
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
  
  

# Year to Date Stats ------------------------------------------------------

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # 
  # # Modify DF to be able to be appended
  # total.d[,c(2:26)] <- apply(total.d[,c(2:26)], 2, function(x) as.numeric(as.character(x)))
  # total.d[,c(3:4,6,8,15:16,21:22)] <- apply(total.d[,c(3:4,6,8,15:16,21:22)], 2, function(x) x / total.d$passing.G) # Converting some stats into per game stats
  # names(total.d) <- c("Team","Games","dAverage.Completions","dAverage.Attempts","dCompletion.Percentage","dAverage.TD","dTDPerAtt",
  #                     "Average.Int","dIntPer","dYdsAtt","dAdjYdsAtt","dYdsCompletion","dYdsGame","dPassRating","dSack",
  #                     "dSackYds","dNetGainAtt","dAdjNetYdsAtt","SackPer","dExpectedPts","dDrivesPerGame","dPlaysPerGame","dPerOffScore",
  #                     "dPerTurnover","dPlaysPerDrive","dAvgPtsPerDrive")
  # 
  # total_d$total_Tm <- as.character(total_d$total_Tm)
  # total_d$total_Tm <- ifelse(total_d$total_Tm == "New York Jets", "NYJ", 
  #                            ifelse(total_d$total_Tm == "San Diego Chargers", "SDG", 
  #                                   ifelse(total_d$total_Tm == "New York Giants", "NYG", 
  #                                          ifelse(total_d$total_Tm == "New Orleans Saints", "NOR",
  #                                                 ifelse(total_d$total_Tm == "Jacksonville Jaguars", "JAX",
  #                                                        ifelse(total_d$total_Tm == "Los Angeles Rams", "LAR",
  #                                                               ifelse(total_d$total_Tm == "San Francisco 49ers", "SFO", 
  #                                                                      ifelse(total_d$total_Tm == "New England Patriots", "NWE", total_d$total_Tm))))))))
  # total_d$total_Tm <- sapply(total_d$total_Tm, function(x) toupper(substring(x,1,3))) # Prepping for merge (4ffor4 uses abbreviations)
  # total_d <- sapply(total_d, as.numeric )
  
  
  ##########################################################################################
  ###### Year to Date Stats 
  ##########################################################################################
  ytd_url <- GET("http://www.pro-football-reference.com/years/2018/passing.htm")
  ytd <- as.data.frame(readHTMLTable(rawToChar(ytd_url$content))) %>% 
    .[,c(2,6,9:11, 13:16, 18:23, 26:27)] %>% 
    .[!(.$passing. ==""),]
  
  ytd[,c(2:length(ytd))] <- apply(ytd[,c(2:length(ytd))], 2, function(x) as.numeric(as.character(x)))
  ytd <- subset(ytd, passing.G > 2)
  ytd[,c(3:4,6,8)] <- apply(ytd[,c(3:4,6,8)], 2, function(x) x/ytd$passing.G)
  
  names(ytd) <- c("Player","ytdGames","ytdP.Comp","ytdP.Att","ytdComp.Per","ytdP.TD","ytdP.TD.Per","ytdINT","ytdINT.Per",
                  "ytdYds.Att","ytdAdj.Yds.Att","ytdYd.Compl","ytdYds.Game","ytdPRat","ytdQBR","ytdNetYd.Att","ytdAdjNetYd.Att") 
  
  ######
  ###### Create Large File (to be added into cumulative stats)
  ######
  
  all_qb <- merge(wk, total_d, by.x = "Opponent", by.y = "total_Tm") %>%
    merge(ytd, by.x = "Player", by.y = "Player")
  
  yearly <- read.csv("~/Dropbox/fantasyFootball/newQBYearly.csv") %>% .[,-1]
  
  all.qb <- rbind(yearly, all.qb)
  
  ###### MAKE SURE YOU ARE READY TO WRITE write.csv(all.qb, "C:/Users/Matt/OneDrive/fantasyFootball/newQBYearly.csv")
  write.csv(all_qb,"C:/Users/mattw/Google Drive/fantasyFootball/qb_2018.csv")
  
  #####
  ##### Current week projections, combine with red zone and defense
  #####
  
  qb.dfs <- read.csv(paste("~/Dropbox/fantasyFootball/DFS_Wk",as.character(wk.num),".csv", sep = "")) %>% 
    subset(Pos == "QB") %>% .[,c(4,7:8,10:17,40:41,52:54)] %>% 
    subset(FanDuel..Projected.points. >1) %>%
    mutate(Field = ifelse(substr(Opp,1,1) == "@", 2,1),
           Opp = as.character(Opp),
           Opp = ifelse(substr(Opp,1,1) == "@", substr(Opp,2,4), Opp),
           Cmp.Per = (Pass.Comp/Pass.Att)*100,
           Adj.Yds.Att = (Pass.Yds+(20*Pass.TD)-(45*INT))/Pass.Att,
           Yds.Att = Pass.Yds/Pass.Att,
           Rush.Avg = Rush.Yds/Rush.Att) 
  
  names(qb.dfs) <- c("Player","Opp","aFPA","Pass.Comp","Pass.Att","Pass.Yds","Pass.TD","INT","Rush.Att",
                     "Rush.Yds","Rush.TD","FanDuel..Projected.Points.","FanDeul..Price.","O.U","Total",
                     "Team.O.U","Field","Cmp.Per","Adj.Yds.Att","Yds.Att","Rush.Avg") 
  
  qb.dfs$Opp <- ifelse(qb.dfs$Opp == "KC", "KAN", 
                       ifelse(qb.dfs$Opp == "LA", "LAR", 
                              ifelse(qb.dfs$Opp == "KC", "KAN",
                                     ifelse(qb.dfs$Opp == "SD", "SDG",
                                            ifelse(qb.dfs$Opp == "NO", "NOR",
                                                   ifelse(qb.dfs$Opp == "TB", "TAM", 
                                                          ifelse(qb.dfs$Opp == "SF", "SFO",
                                                                 ifelse(qb.dfs$Opp == "NE", "NWE",
                                                                        ifelse(qb.dfs$Opp == "GB", "GRE", qb.dfs$Opp)))))))))
  
  qb.dfs <- merge(qb.dfs, total.d, by.x = "Opp", by.y = "Team")
  qb.dfs <- merge(qb.dfs, rz, by.x = "Player", by.y = "Player")
  qb.dfs <- merge(qb.dfs, ytd, by.x = "Player", by.y = "Player")
  
  
  
  
  
  
  
  
}