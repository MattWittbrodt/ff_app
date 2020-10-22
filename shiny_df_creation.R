# DFS 2019 PreProcessing for Shiny App

shiny_df <- function(wk_num) {

library(tidyverse)
library(mattDFS)

# Logging into stathead ----
stathead <- stathead_login()
fff <- fff_login()

# Download projections ----
proj_data <- get_projections(wk_num)

# Reading in team name chart ----------------------------------------------
source("~/ff_shiny_app/ff_app/name_fixes.R", local = T)

# Vegas Lines -------------------------------------------------------------
vegas <- vegas_lines()
print('Vegas Lines Successful')

# DVOA Data ---------------------------------------------------------------
dvoa <- dvoa(playoffs = FALSE)
dvoa_defense <- dvoa$defense
dvoa_offense <- dvoa$offense

dvoa_defense[["def_team"]] <- sapply(dvoa_defense[["def_team"]], function(x) find_names(x, "fff_abbreviation"))
dvoa_offense[["off_team"]] <- sapply(dvoa_offense[["off_team"]], function(x) find_names(x, "fff_abbreviation"))
print('DVOA Successful')

# Last Week Data ----------------------------------------------------------

wk_data <- get_last_wk_data(wk_num, stathead)
print("Last Week Data Successful")

# Red Zone Data -----------------------------------------------------------
rz_data <- get_red_zone()
print("Red Zone Offensive Data Successful")

# Year to Date Data -------------------------------------------------------
ytd_df <- get_ytd_data()
print("Year to Data Data Successful")

# Combinging into one list ------------------------------------------------
data <- list(wk_data = wk_data,
             #rz_df = rz_df,
             ytd_df = ytd_df)
# Positions ---------------------------------------------------------------
source("~/ff_shiny_app/ff_app/position_information.R", local = T)

# QB
qb <- position_stats("QB",wk_num, data, tm_names) %>%
      mutate(proj_tm = ifelse(proj_tm == "character(0)", "LVE", proj_tm),
             proj_opp = ifelse(proj_opp == "character(0)", "LVE", proj_opp))

# RB
rb <- position_stats("RB",wk_num,data, tm_names) %>%
  mutate(proj_tm = ifelse(proj_tm == "character(0)", "LVE", proj_tm),
         proj_opp = ifelse(proj_opp == "character(0)", "LVE", proj_opp))

# WR
wr <- position_stats("WR",wk_num,data,tm_names) %>%
  mutate(proj_tm = ifelse(proj_tm == "character(0)", "LVE", proj_tm),
         proj_opp = ifelse(proj_opp == "character(0)", "LVE", proj_opp))

# Getting WR matchup
wr_matchup <- wr_matchups(wk_num,fff)

# Adding in matchup stats
wr <- left_join(wr, wr_matchup, by = c("proj_player" = "vs_cb_wr"))

# TE
te <- position_stats("TE",wk_num,data, tm_names) %>%
  mutate(proj_tm = ifelse(proj_tm == "character(0)", "LVE", proj_tm),
         proj_opp = ifelse(proj_opp == "character(0)", "LVE", proj_opp))

print("Positional Data Successful")

# Combinng into one DF ----------------------------------------------------

all_positions <- merge(qb,rb, all = TRUE) %>%
                 merge(wr, all = TRUE) %>%
                 merge(te, all = TRUE) %>%
                 left_join(vegas, by = c("proj_tm" = "team")) %>%
                 left_join(dvoa_defense, by = c("proj_opp" = "def_team")) %>%
                 left_join(dvoa_offense, by = c("proj_tm" = "off_team"))


# Adding previous week DVOA -----------------------------------------------

dvoa_previous <- dvoa_defense
names(dvoa_previous) <- paste("prev_wk_dvoa", names(dvoa_previous), sep = "_")

all_positions <- left_join(all_positions, dvoa_previous, by = c("prev_wk_opp" = "prev_wk_dvoa_def_team"))
print("Previous Week DVOA Successful")

# Adding Leverage Scores --------------------------------------------------
leverage <- get_leverage(wk_num, fff)

# Adding into full DF
all_positions <- left_join(all_positions, leverage, by = c("proj_player" = "player",
                                                           "proj_pos" = "pos",
                                                           "proj_tm" = "tm"))

print("Leverage Score Successful")

# Adding pricing information ----
pricing <- get_pricing(wk_num, fff)

all_positions <- left_join(all_positions, pricing, by = c("proj_player" = "pricing_player",
                                                           "proj_pos" = "pricing_position",
                                                           "proj_tm" = "pricing_team"))
print("Pricing Data Import Successful")

# Adding in the advanced stats information ----
source("~/ff_shiny_app/ff_app/advanced_stats.R")
adv_pass <- advanced_passing_stats()
adv_rush <- advanced_rushing_stats()
adv_rec <- advanced_receiving_stats()

# Joining into one large dataframe
all_data <- all_positions %>%
            left_join(adv_pass, by = c("proj_player" = "adv_player")) %>%
            left_join(adv_rush, by = c("proj_player" = "adv_player")) %>%
            left_join(adv_rec, by = c("proj_player" = "adv_receiving_player"))

print("Advanced Stats Successful")

# Adding in individual information from Football Outsiders ----
source("~/ff_shiny_app/ff_app/football_outsiders_advanced_position.R")

# Running specific Data
qb_df <- fo_qb()
rb_df <- fo_rb()
wr_df <- fo_pass_catchers("wr")
te_df <- fo_pass_catchers("te")

# Merging into 1 DF
fo_all_positions <- full_join(qb_df, rb_df, by = c("pass_player" = "rush_player",
                                                   "pass_team" = "rush_team",
                                                   "rush_dyar","rush_eyds", "rush_dvoa",
                                                   "rush_yar","rush_voa","rush_yards")) %>%
                    full_join(wr_df, by = c("pass_player" = "rec_player",
                                            "pass_team" = "rec_team",
                                            "rec_dyar" , "rec_dvoa", "rec_eyds", "rec_catch_rate",
                                            "rec_yar","rec_voa","rec_yards")) %>%
                    full_join(te_df, by = c("pass_player" = "rec_player",
                                            "pass_team" = "rec_team",
                                            "rec_dyar" , "rec_dvoa", "rec_eyds", "rec_catch_rate",
                                            "rec_yar","rec_voa","rec_yards")) %>%
                    mutate(pass_player = ifelse(is.na(str_extract(pass_player, "^[:upper:]\\.[:upper:]\\.")) == T,
                                        pass_player,
                                        str_remove(pass_player, "\\.")),
                    pass_player = str_replace(pass_player, "[:punct:]", " "),
                    pass_player = str_remove_all(pass_player, "-"))

fo_all_positions[["pass_team"]] <- sapply(fo_all_positions[["pass_team"]], function(x) find_names(x, "fff_abbreviation"))

# Merging with rest of data
all_data2 <- all_data %>%
             mutate(proj_player_new = str_remove_all(proj_player, "(?<=[:upper:])[:lower:]{1,}(?=[:space:])"))


# Combining
all_data_fo_pos <- left_join(all_data2, fo_all_positions, by = c("proj_player_new" = "pass_player", "proj_tm" = "pass_team")) %>%
                   select(-proj_player_new)
print("FO All Advanced Position Stats Combined")

# Adding in pace of place stats ----
pace <- get_pace()
pace[["off_team"]] <- sapply(pace[["off_team"]], function(x) find_names(x, "fff_abbreviation"))
all_data_fo_pos_pace <- left_join(all_data_fo_pos, pace, by = c("proj_tm" = "off_team"))

print("Pace of Play Successful")

# Writing outexcel file
copy_and_write(all_data_fo_pos_pace,7)
print("Data Written Succesfully")

# Returning full DF ----
return(all_data_fo_pos_pace)

}

# Getting full dataframe --------------------------------------------------
ptm <- proc.time()
df <- shiny_df(7)
end <- proc.time() - ptm
