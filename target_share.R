###
### Target Share Calculation
###

get_target_share <-  function(x) {

  # First lapply through all positions, returning only reception data
  d <- lapply(x, function(pos) {
    d2 <- select(pos, contains("player"), ytd_rec_tm, ytd_rec_target)
    colnames(d2) <- c("player", "team", "target")
    return(d2)
  })

  # Converting list to DF for further analysis; removing OAK and LeVeon Bell (2TM)
  # The averge is to only return 1 instance for each player
  d2 <- do.call(rbind, d) %>%
        filter(is.na(target) == F & team != "OAK" & team != "2TM") %>%
        group_by(player, team) %>%
        summarize(target = mean(target))

  # Getting team totals / game
  teams <- d2 %>% group_by(team) %>% summarize(total_target = sum(target))

  # Now, going through each player and getting taget share from the team data
  d2 <- mutate(d2, target_share = 0)

  for(player in 1:nrow(d2)) {

    # Getting a smaller DF
    tgt <- d2[player,3]
    team <- as.character(d2[player,2])
    team_target <- teams[teams[,1] == team, 2]

    taget_share = round((tgt / team_target)*100,2)

    # Putting back in DF
    d2[player,4] <- taget_share

  }

}
