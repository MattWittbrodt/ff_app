
advanced_passing_stats <- function() {

  # Getting link and reading in HTML table
  link <- "https://www.pro-football-reference.com/years/2020/passing_advanced.htm" %>%
           read_html() %>%
           html_table(fill = T)

  # returned tables are in a list, so returning those
  dfs <- lapply(link, function(x) {

    # Making better column names which are in the first row
    descriptions <- x[1,]
    x <- x[-1,] # removing first row

    names <- colnames(x)

    # Creating a combination column name
    for(ii in 1:length(names)) {

      names[ii] <- paste("adv",names[ii],descriptions[ii], sep = "_") %>%
                   str_to_lower() %>%
                   str_replace("__","_") %>%
                   str_replace("/", "_per_") %>%
                   str_replace("%","_per")
    }

    # Add back into the DF
    colnames(x) <- names

    # Selecting onl the relevant columns
    x2 <- x %>%
          select(-adv_rk,
                 -adv_tm,
                 -adv_age,
                 -adv_pos,
                 -adv_games_g,
                 -adv_games_gs,
                 -adv_passing_cmp,
                 -adv_passing_att,
                 -adv_passing_yds) %>%
          filter(adv_player != "Player") %>%
          mutate(adv_player = str_remove(adv_player, "[:punct:]"))

    return(x2)

  })

  # Merging into one DF
  all_df <- left_join(dfs[[1]], dfs[[2]], by = "adv_player") %>%
            left_join(dfs[[3]], by = "adv_player") %>%
            left_join(dfs[[4]], by = "adv_player")

  # Fixing a few issues with columns
  all_df[,-1] <- apply(all_df[,-1], c(1,2), function(x) {str_remove(x, "%") %>% as.numeric()})

  all_df <- name_fixes(all_df, 1, 2, 3)

  return(all_df)

}

advanced_rushing_stats <- function() {

  # Reading in data
  link <- "https://www.pro-football-reference.com/years/2020/rushing_advanced.htm" %>%
          read_html()
  link_comments <- gsub("!--","", link) # the actual information is commented out, so need to remove in HTML

  link2 <- read_html(link_comments) %>% html_table(fill = T)
  link_data <- link2[[1]]

  # Fixing column names, which are a combination of column names and first row
  descriptions <- link_data[1,]
  link_data <- link_data[-1,] # removing first row

  names <- colnames(link_data)

  # Creating a combination column name
  for(ii in 1:length(names)) {

    names[ii] <- paste("adv",names[ii],descriptions[ii], sep = "_") %>%
      str_to_lower() %>%
      str_replace("__","_") %>%
      str_replace("/", "_per_") %>%
      str_replace("%","_per")
  }

  # Add back into the DF
  colnames(link_data) <- names

  # Selecting onl the relevant columns
  link_data2 <- link_data %>%
                select(-adv_rk,
                       -adv_tm,
                       -adv_age,
                       -adv_pos,
                       -adv_games_g,
                       -adv_games_gs,
                       -adv_rushing_att,
                       -adv_rushing_yds) %>%
                filter(adv_player != "Player") %>%
                mutate(adv_player = str_remove(adv_player, "[:punct:]"))
  link_data2[,2:ncol(link_data2)] <- apply(link_data2[,2:ncol(link_data2)], 2, function(x) as.numeric(x))
  return(link_data2)

}

advanced_receiving_stats <- function() {

  # Reading in data
  link <- "https://www.pro-football-reference.com/years/2020/receiving_advanced.htm" %>%
          read_html()
  link_comments <- gsub("!--","", link) # the actual information is commented out, so need to remove in HTML

  link2 <- read_html(link_comments) %>% html_table(fill = T)
  link_data <- link2[[1]]

  # Fixing column names
  names <- colnames(link_data)

  # Creating a combination column name
  for(ii in 1:length(names)) {

    names[ii] <- paste("adv_receiving",names[ii], sep = "_") %>%
      str_to_lower() %>%
      str_replace("__","_") %>%
      str_replace("/", "_per_") %>%
      str_replace("%","_per")
  }

  # Add back into the DF
  colnames(link_data) <- names

  # Selecting onl the relevant columns
  link_data2 <- link_data %>%
                select(-adv_receiving_rk,
                       -adv_receiving_age,
                       -adv_receiving_pos,
                       -adv_receiving_gs,
                       -adv_receiving_td) %>%
                filter(adv_receiving_player != "Player") %>%
                mutate(adv_receiving_player = str_remove_all(adv_receiving_player, "[:punct:]"))
  link_data2[,3:ncol(link_data2)] <- apply(link_data2[,3:ncol(link_data2)], 2, function(x) {as.numeric(x)})

  # Calculating a few advanced receiving metrics - air yards, RACR,
  link_data3 <- link_data2 %>% mutate(
                               adv_receiving_air_yards = as.numeric(adv_receiving_adot)*(adv_receiving_tgt/adv_receiving_g),
                               adv_receiving_racr = round((adv_receiving_yds/adv_receiving_g)/adv_receiving_air_yards,2)) %>%
                filter(adv_receiving_tm != "2TM")

  # Getting team totals (target and air yards)
  team_totals <- link_data3 %>%
                 group_by(adv_receiving_tm) %>%
                 summarize(team_air_yards = sum(adv_receiving_air_yards), team_targets = sum(adv_receiving_tgt)) %>%
                 filter(adv_receiving_tm != "2TM")

  # Looping through to get team share
  link_data3$adv_receiving_target_share <- 0
  link_data3$adv_receiving_air_yard_share <- 0

  get_market_share <- function(stat_col, df, team_sum_col, new_col) {

    for(player in 1:nrow(df)) {

      # Getting a smaller DF
      player_val <- df[player,stat_col]
      print(player_val)
      team <- as.character(df[player,2])
      print(team)
      team_sum <- as.numeric(team_totals[team_totals[,1] == team,team_sum_col])
      print(team_sum)

      # Calculating target share
      market_share = round((player_val / team_sum)*100,2)
      print(market_share)

      # Putting back in DF
      df[player,new_col] <- market_share

    }

  }




  (player in 1:nrow(link_data3)) {

      # Getting a smaller DF
      targets <- link_data3[player,4]
      ay <- link_data3[player,19]
      team <- as.character(link_data3[player,2])
      team_target <- team_totals[team_totals[,1] == team, 2]

      taget_share = round((tgt / team_target)*100,2)

      # Putting back in DF
      d2[player,4] <- taget_share

    }
  #
  #
  # }







  return(link_data2)

}






