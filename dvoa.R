# Getting Football Outsider Information
# https://www.r-bloggers.com/using-rvest-to-scrape-an-html-table/


dvoa <- function(playoffs) {
  
  library(rvest)
  library(tidyverse)
  

# Total Team Efficiency ---------------------------------------------------

  # Total Team Efficiency
  total_efficiency <- "https://www.footballoutsiders.com/stats/nfl/team-efficiency/2020" %>%
         read_html() %>%
         html_nodes(xpath = '//*[@id="node-76118"]/div/div/table[1]') %>%
         html_table() %>%
         .[[1]] %>%
         .[,-1]
  
  if(playoffs == T) {
    
    total_efficiency2 <- "https://www.footballoutsiders.com/stats/teameff/2019" %>%
      read_html() %>%
      html_nodes(xpath = '//*[@id="node-62599"]/div/div/table[2]') %>%
      html_table() %>%
      .[[1]] %>%
      .[,-1]
    
    total_efficiency <- left_join(total_efficiency,total_efficiency2, by = "TEAM") %>%
                        select(-contains("WINS"),
                               -contains("W-L"))
  }
  
  # Getting into a standardized set of names
  total_names <- colnames(total_efficiency) %>%
                 str_to_lower() %>%
                 str_replace_all("[^\\w]+","_") %>% 
                 str_replace_all("(?<=offense|defense|total)dvoa","_dvoa") %>%
                 str_replace_all("(?<=total)dave","_dave") %>%
                 str_replace_all("off_", "offense_") %>%
                 str_replace_all("def_", "defense_") 
  
  colnames(total_efficiency) <- total_names
  
  # subsetting columns
  total_efficiency <- total_efficiency %>%
                      select(-any_of(c("last_week","w_l")),
                             -contains("s_t_"),
                             ) %>%
                      mutate(total_dvoa = as.numeric(sapply(total_dvoa, function(x) str_remove(x, '%'))),
                             total_dave = as.numeric(sapply(total_dave, function(x) str_remove(x, '%'))),
                             #wei_dvoa = as.numeric(sapply(wei_dvoa, function(x) str_remove(x, '%'))),
                             #non_adj_tot_voa = as.numeric(sapply(non_adj_tot_voa, function(x) str_remove(x, '%'))),
                             offense_dvoa = as.numeric(sapply(offense_dvoa, function(x) str_remove(x, '%'))),
                             defense_dvoa = as.numeric(sapply(defense_dvoa, function(x) str_remove(x, '%'))))
  

# Offense Efficiency ------------------------------------------------------

  offense <- "https://www.footballoutsiders.com/stats/nfl/team-offense/2020" %>%
              read_html() %>%
              html_nodes(xpath = '//*[@id="node-76119"]/div/div/table[1]') %>%
              html_table() %>%
              .[[1]] %>%
              .[,-1]
  
  # Altering offense names
  offense_names <- colnames(offense) %>%
                   str_to_lower() %>%
                   str_replace_all("[^\\w]+","_") %>% 
                   str_replace_all("(?<=offense)dvoa","_dvoa") %>%
                   str_replace_all("(?<=offense)dave","_dave") %>%
                   str_remove("_\\d") %>%
                   str_replace("passoff", "pass_off") %>%
                   str_replace("rushoff","rush_off")
  
  # Adding in descriptors for rank column
  for(ii in 1:length(offense_names)) {
    if(offense_names[ii] == "rk" & offense_names[ii] != "team") {
      offense_names[ii] <- paste(offense_names[ii-1], offense_names[ii], sep = "_")
    } else {
      if(str_detect(offense_names[ii], "_off") == TRUE) {
        offense_names[ii] <- paste(offense_names[ii], "dvoa", sep = "_")
      }
    }
  }
  
  colnames(offense) <- offense_names
  
  # subsetting columns
  offense <- offense %>% 
             select(-contains("last"),
                    -offense_dave,
                    -offense_dvoa) %>%
             mutate(#wei_offense_dvoa = as.numeric(sapply(wei_offense_dvoa, function(x) str_remove(x, '%'))),
                    pass_off_dvoa = as.numeric(sapply(pass_off_dvoa, function(x) str_remove(x, '%'))),
                    rush_off_dvoa = as.numeric(sapply(rush_off_dvoa, function(x) str_remove(x, '%'))))

# Defense Efficiency ------------------------------------------------------

  defense <- "https://www.footballoutsiders.com/stats/nfl/team-defense/2020" %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="node-76120"]/div/div/table[1]') %>%
    html_table() %>%
    .[[1]] %>%
    .[,-1]
  
  # Altering offense names
  defense_names <- colnames(defense) %>%
    str_to_lower() %>%
    str_replace_all("[^\\w]+","_") %>% 
    str_replace_all("(?<=defense)dvoa","_dvoa") %>%
    str_replace_all("(?<=defense)dave","_dave") %>%
    str_remove("_\\d") %>%
    str_replace("passdef", "pass_def") %>%
    str_replace("rushdef","rush_def") %>%
    str_replace("defdave", "def_dave")
  
  # Adding in descriptors for rank column
  for(ii in 1:length(defense_names)) {
    if(defense_names[ii] == "rk" & defense_names[ii] != "team") {
      defense_names[ii] <- paste(defense_names[ii-1], defense_names[ii], sep = "_")
    } else {
      if(str_detect(defense_names[ii], "_def") == TRUE) {
        defense_names[ii] <- paste(defense_names[ii], "dvoa", sep = "_")
      }
    }
  }
  
  colnames(defense) <- defense_names
  
  # subsetting columns
  defense <- defense %>% 
    select(-contains("last"),
           -defense_dvoa) %>%
    mutate(#wei_defense_dvoa = as.numeric(sapply(wei_defense_dvoa, function(x) str_remove(x, '%'))),
           pass_def_dvoa = as.numeric(sapply(pass_def_dvoa, function(x) str_remove(x, '%'))),
           rush_def_dvoa = as.numeric(sapply(rush_def_dvoa, function(x) str_remove(x, '%'))))
  
# D Line Stats ------------------------------------------------------------

  dline <- "https://www.footballoutsiders.com/stats/nfl/defensive-line/2020" %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="node-76127"]/div/div/table[1]') %>%
    html_table() %>%
    .[[1]] %>%
    .[,-c(1,(ncol(.)-3):ncol(.))]
  
  # colnames are in first line, so correcting for that
  colnames(dline) <- dline[1,]
  dline <- dline[-1,]
  
  # Further editing names
  dline_names <- colnames(dline) %>%
    str_to_lower() %>%
    str_replace_all("[^\\w]+","_") %>% 
    str_replace_all("(?<=defense)dvoa","_dvoa") %>%
    str_remove("_\\d") %>%
    str_replace("powersuccess", "power_success") %>%
    str_replace("lineyards", "line_yards")
  
  for(ii in 1:length(dline_names)) {
    if(dline_names[ii] != "team") {
      dline_names[ii] <- paste("dline", dline_names[ii], sep = "_")
    }
  }
  
  colnames(dline) <- dline_names
  
  dline <- dline %>%
           subset(team != "NFL") %>%
           mutate(dline_power_success = as.numeric(sapply(dline_power_success, function(x) str_remove(x, '%'))),
                  dline_stuffed = as.numeric(sapply(dline_stuffed, function(x) str_remove(x, '%'))),
                  dline_adj_line_yards = round(as.numeric(dline_adj_line_yards) - mean(as.numeric(dline_adj_line_yards)),2))
  
  # Quick dline passing stats (on website as same table but not same rank)
  dline_pass <- "https://www.footballoutsiders.com/stats/nfl/defensive-line/2020" %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="node-76127"]/div/div/table[1]') %>%
    html_table() %>%
    .[[1]] %>%
    .[,c((ncol(.)-3):ncol(.))]
  
  # colnames are in first line, so correcting for that
  dline_pass_names <- dline_pass[1,] %>%
                      str_to_lower() %>%
                      str_replace_all("[^\\w]+","_") %>%
                      str_replace("adjustedsack", "adjusted_sack")
  
  # Addind prefix
  for(ii in 1:length(dline_pass_names)) {
    if(dline_pass_names[ii] != "team") {
      dline_pass_names[ii] <- paste("dline_pass", dline_pass_names[ii], sep = "_")
    }
  }
  
  colnames(dline_pass) <- dline_pass_names
  
  dline_pass <- dline_pass[-1,] %>%
                subset(team != "NFL") %>%
                mutate(dline_pass_rank = as.numeric(dline_pass_rank),
                       dline_pass_sacks = as.numeric(dline_pass_sacks),
                       dline_pass_adjusted_sack_rate = as.numeric(sapply(dline_pass_adjusted_sack_rate, function(x) str_remove(x, '%'))))
  

# OLine Stats -------------------------------------------------------------

  oline <- "https://www.footballoutsiders.com/stats/nfl/offensive-line/2020" %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="node-76126"]/div/div/table[1]') %>%
    html_table() %>%
    .[[1]] %>%
    .[,-c(1,(ncol(.)-3):ncol(.))]
  
  # colnames are in first line, so correcting for that
  colnames(oline) <- oline[1,]
  oline <- oline[-1,]
  
  # Further editing names
  oline_names <- colnames(oline) %>%
    str_to_lower() %>%
    str_replace_all("[^\\w]+","_") %>% 
    str_replace_all("(?<=offense)dvoa","_dvoa") %>%
    str_remove("_\\d")
  
  for(ii in 1:length(oline_names)) {
    if(oline_names[ii] != "team") {
      oline_names[ii] <- paste("oline", oline_names[ii], sep = "_")
    }
  }
  
  colnames(oline) <- oline_names
  
  oline <- oline %>%
    subset(team != "NFL") %>%
    mutate(oline_power_success = as.numeric(sapply(oline_power_success, function(x) str_remove(x, '%'))),
           oline_stuffed = as.numeric(sapply(oline_stuffed, function(x) str_remove(x, '%'))),
           oline_adj_line_yards = round(as.numeric(oline_adj_line_yards) - mean(as.numeric(oline_adj_line_yards)),2))
  
  # Quick dline passing stats (on website as same table but not same rank)
  oline_pass <- "https://www.footballoutsiders.com/stats/nfl/offensive-line/2020" %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="node-76126"]/div/div/table[1]') %>%
    html_table() %>%
    .[[1]] %>%
    .[,c((ncol(.)-3):ncol(.))]
  
  # colnames are in first line, so correcting for that
  oline_pass_names <- oline_pass[1,] %>%
    str_to_lower() %>%
    str_replace_all("[^\\w]+","_")
  
  # Addind prefix
  for(ii in 1:length(oline_pass_names)) {
    if(oline_pass_names[ii] != "team") {
      oline_pass_names[ii] <- paste("oline_pass", oline_pass_names[ii], sep = "_")
    }
  }
  
  colnames(oline_pass) <- oline_pass_names
  
  oline_pass <- oline_pass[-1,] %>%
    subset(team != "NFL") %>%
    mutate(oline_pass_rank = as.numeric(oline_pass_rank),
           oline_pass_sacks = as.numeric(oline_pass_sacks),
           oline_pass_adjusted_sack_rate = as.numeric(sapply(oline_pass_adjusted_sack_rate, function(x) str_remove(x, '%'))))

# Return List of DFs ------------------------------------------------------

# Defense
  
  # Removing offense stats
  total_efficiency_d <- dplyr::select(total_efficiency, -contains("off"))
  
  def_all <- left_join(total_efficiency_d,defense, by = "team") %>%
             left_join(dline_pass, by = "team") %>%
             left_join(dline, by = "team")
  
  # Make all columns more distinguishable from later on
  d_all_col <- colnames(def_all) %>%
               str_remove("def_") %>%
               str_remove("defense") %>%
               str_remove("^_")
  # Fixing issue when col name is just 'rank'
  for(ii in 1:length(d_all_col)) {if(d_all_col[ii] == "rank") d_all_col[ii] <- paste(d_all_col[[ii-1]],d_all_col[ii], sep = "_")}
  
  d_all_col <- paste("def", d_all_col, sep = "_")
  
  colnames(def_all) <- d_all_col
  
# Offense
  # Removing defense stats
  total_efficiency_o <- dplyr::select(total_efficiency, -contains("def"))
  
  off_all <- left_join(total_efficiency_o,offense, by = "team") %>%
             left_join(oline_pass, by = "team") %>%
             left_join(oline, by = "team")
  
  # Make all columns more distinguishable from later on
  o_all_col <- colnames(off_all) %>%
               str_remove("off_") %>%
               str_remove("offense") %>%
               str_remove("^_")
  
  # Fixing issue when col name is just 'rank'
  for(ii in 1:length(o_all_col)) {if(o_all_col[ii] == "rank") o_all_col[ii] <- paste(o_all_col[[ii-1]],o_all_col[ii], sep = "_")}
  
  o_all_col <- paste("off", o_all_col, sep = "_")
  
  colnames(off_all) <- o_all_col
  
  
  return(list(defense = def_all,
              offense = off_all))
  
}
