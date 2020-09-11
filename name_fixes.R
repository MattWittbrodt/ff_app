# Getting players with trades and no positions

name_fixes <- function(df, player_col, team_col, pos_col) {
  
  # Removing punctuation
  for(n in 1:nrow(df)) {df[n,player_col] <- str_remove_all(df[n,player_col], "[:punct:]")}
  for(n in 1:nrow(df)) {df[n,player_col] <- str_remove_all(df[n,player_col], "\\W{1,2}$")} # Removing any weird '+' for pro bowl
  
  # Adjusting Team Names
  if(team_col > 0) {
    for(p in 1:nrow(df)) {
        df[p,team_col] <- switch(df[p,player_col],
                                 "Emmanuel Sanders" = "SFO",
                                 "Mohamed Sanu" = "NWE",
                                 "Zay Jones" = "OAK",
                                 "Kenyan Drake" = "ARI",
                                 "Josh Gordon" = "SEA",
                                 df[p, team_col])
    }
  }
  
  # Adjusting Position Names
  if(pos_col > 0) {
    for(p in 1:nrow(df)) {
        df[p,pos_col] <- switch(df[p,player_col],
                                "Dion Lewis" = "RB",
                                "Brian Hill" = "RB",
                                "Darrell Henderson" = "RB",
                                "Raheem Mostert" = "RB",
                                "Kenyan Drake" = "RB",
                                "Royce Freeman" = "RB",
                                "Alexander Mattison" = "RB",
                                "Gus Edwards" = "RB",
                                "Rashaad Penny" = "RB",
                                "Giovani Bernard" = "RB",
                                "Darrell Henderson" = "RB",
                                "Darrel Williams" = "RB",
                                "Kareem Hunt" = "RB",
                                "Reggie Bonnafon" = "RB",
                                "TJ Yeldon" = "RB",
                                "Trey Edmunds" = "RB",
                                "Mohamed Sanu" = "WR",
                                "Emmanuel Sanders" = "WR",
                                "Josh Gordon" = "WR",
                                "Zay Jones" = "WR",
                                "Seth Roberts" = "WR",
                                "Laquon Treadwell" = "WR",
                                "Tavon Austin" = "WR",
                                "Chris Herndon" = "TE",
                                "Breshad Perriman" = "WR",
                                "Russell Shepard" = "WR",
                                "Michael Thomas" = "WR",
                                "Cooper Kupp" = "WR",
                                "Robert Woods" = "WR",
                                "Jared Cook" = "TE",
                                "George Kittle" = "TE",
                                "Tyler Higbee" = "TE",
                                "Deebo Samuel" = "WR",
                                "DK Metcalf" = "WR",
                                str_to_upper(df[p, pos_col]))
    }
  }
  
  return(df)
}