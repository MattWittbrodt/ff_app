# Function for extracting performance data

#wk_num = 2
#position = "QB"

position_stats <- function(position, wk_num, data, tm_names) {

  library(tidyverse)
  library(stringr)
  library(rvest)

  # Reading in team name chart
  #tm_names <- readxl::read_xlsx("data/team_names.xlsx")
  #source("~/ff_shiny_app/ff_app/find_names.R", local = T)

# Previous Week Data ------------------------------------------------------

  wk_data2 <- switch(position, QB = wk_data$QB, RB = wk_data$RB, WR = wk_data$WR, TE = wk_data$TE)
  print("In position stats: Previous week data Successful")

# Red Zone Stats ----------------------------------------------------------

  rz_data <- switch(position, QB = rz_data$QB, RB = rz_data$RB, WR = rz_data$WR, TE = rz_data$TE)
  print("In position stats: RZ Data Successful")

# Team Defense Integration ------------------------------------------------

  # getting all_d_data from main script

# Year to Date Stats ------------------------------------------------------

  ytd_data <- switch(position, QB = data$ytd_df$QB, RB = data$ytd_df$RB, WR = data$ytd_df$WR, TE = data$ytd_df$TE)
  cat(paste0("In position data: ", position, " YTD Data Successful \n"))

# Current Week Projections ------------------------------------------------

  proj_data <- filter(proj_data, proj_pos == position)
  cat(paste0("In position data: ", position, " projections successfull \n"))

# Fantasy Points Against Data ---------------------------------------------

  source("~/ff_shiny_app/ff_app/pts_against_function.R")
  pts_vs <- pts_against(position)
  cat("In position stats: Pts VS Data Successful \n")

# Merging all data --------------------------------------------------------

  if(position == "QB") {

    # Currently, PFR does not have many QB under position, so fixing that
    ytd_data <- ytd_data %>% filter(ytd_pass_yds > 10) %>% mutate(ytd_pass_pos = "QB")

    all_data <- full_join(proj_data, wk_data2, by = c("proj_player" = "prev_wk_player",
                                                       "proj_tm" = "prev_wk_tm",
                                                       "proj_pos" = "prev_wk_pos")) %>%
                full_join(ytd_data, by = c("proj_player"= "ytd_pass_player",
                                                     "proj_tm" = "ytd_pass_tm",
                                                     "proj_pos" = "ytd_pass_pos")) %>%
                left_join(rz_data, by = c("proj_player" = "passing_player")) %>% # look at inner join later - issues with RZ information currently
                inner_join(all_d_data, by = c("proj_opp" = "def_tm")) %>%
                left_join(pts_vs, by = c("proj_opp" = "pts_vs_tm")) %>%
                select(-ytd_pass_comp,
                       -ytd_pass_att,
                       -ytd_pass_yds,
                       -ytd_pass_sk,
                       -ytd_pass_yds.1,
                       -ytd_pass_sk_per,
                       -ytd_pass_4qc,
                       -ytd_pass_gwd) %>%
                filter(proj_ffpts > 0)
    cat("In position stats: QB Data merged \n")

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
      cat("In position stats: RB Data merged \n")
    } else {

      all_data <- full_join(proj_data, wk_data2, by = c("proj_player" = "prev_wk_player",
                                                         "proj_tm" = "prev_wk_tm",
                                                         "proj_pos" = "prev_wk_pos")) %>%
                  left_join(ytd_data, by = c("proj_player"= "ytd_rec_player",
                                                       "proj_tm" = "ytd_rec_tm")) %>%
                                                       #"proj_pos" = "ytd_rec_pos")) %>% issues current with WR as position
                  left_join(rz_data, by = c("proj_player" = "receiving_player")) %>%
                  left_join(all_d_data, by = c("proj_opp" = "def_tm")) %>%
                  left_join(pts_vs, by = c("proj_opp" = "pts_vs_tm")) %>%
                  filter(proj_ffpts > 0) %>% #, proj_pos != "RB", proj_pos != "TE", proj_pos != "HB")
                  select(-ytd_rec_pos) # removing the position from before
      }
  }

# Return ------------------------------------------------------------------

  return(all_data)
}
