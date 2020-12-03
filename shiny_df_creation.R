# DFS 2019 PreProcessing for Shiny App

shiny_df <- function(wk_num) {

library(rvest)
library(tidyverse)
library(mattDFS)

# Path to tmp folder
tmp <- "C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/tmp/"

# Logging into stathead ----
stathead <- stathead_login()
fff <- fff_login()
fo <- fo_login()

# Download projections ----
proj_data <- get_projections(wk_num)

# Reading in team name chart ----------------------------------------------
#source("~/ff_shiny_app/ff_app/name_fixes.R", local = T)

# Vegas Lines -------------------------------------------------------------
vegas <- vegas_lines()
cat('Vegas Lines Successful\n')

# DVOA Data ---------------------------------------------------------------
dvoa <- dvoa(playoffs = FALSE, fo)

dvoa_defense <- dvoa$defense
dvoa_offense <- dvoa$offense

dvoa_defense[["def_team"]] <- sapply(dvoa_defense[["def_team"]], function(x) find_names(x, "fff_abbreviation"))
dvoa_offense[["off_team"]] <- sapply(dvoa_offense[["off_team"]], function(x) find_names(x, "fff_abbreviation"))
cat('DVOA Successful\n')

# Last Week Data ----------------------------------------------------------

wk_data <- get_last_wk_data(wk_num, stathead)
cat("Last Week Data Successful\n")

# Red Zone Data -----------------------------------------------------------
rz_data <- get_red_zone()
cat("Red Zone Offensive Data Successful\n")

# Year to Date Data -------------------------------------------------------
ytd_df <- get_ytd_data()
cat("Year to Data Data Successful \n")

# Team Defense ------------------------------------------------------------
all_d_data <- get_team_defense()
cat("Team Defense Successful \n")

# Positions ---------------------------------------------------------------
source("~/ff_shiny_app/ff_app/position_information.R", local = T)

# QB -----------------------------------------------------------------------
# Getting relevant information for each position and the specific functions
qb_wk <- wk_data$QB # previous week data
qb_rz <- rz_data$QB

# Getting YTD and ensuring we are getting people with reasonable passing yards & QBs
qb_ytd <- ytd_df$QB %>% filter(ytd_pass_yds > 10) %>% mutate(ytd_pass_pos = "QB")

qb_proj <- filter(proj_data, proj_pos == "QB")
qb_pts_vs <- pts_against("QB")

# Combining into on DF for QB
all_qb <- full_join(qb_proj, qb_wk, by = c("proj_player" = "prev_wk_player", "proj_tm" = "prev_wk_tm", "proj_pos" = "prev_wk_pos")) %>%
          full_join(qb_ytd, by = c("proj_player"= "ytd_pass_player",  "proj_tm" = "ytd_pass_tm", "proj_pos" = "ytd_pass_pos")) %>%
          left_join(qb_rz, by = c("proj_player" = "passing_player")) %>% # look at inner join later - issues with RZ information currently
          inner_join(all_d_data, by = c("proj_opp" = "def_tm")) %>%
          left_join(qb_pts_vs, by = c("proj_opp" = "pts_vs_tm")) %>%
          select(-ytd_pass_comp,
                 -ytd_pass_att,
                 -ytd_pass_yds,
                 -ytd_pass_sk,
                 -ytd_pass_yds.1,
                 -ytd_pass_sk_per,
                 -ytd_pass_4qc,
                 -ytd_pass_gwd,
                 -ytd_pass_qbrec) %>%
          filter(proj_ffpts > 0)
write.csv(all_qb, paste0(tmp,"all_qb.csv"), row.names = F)

# RB ----------------------------------------------------------------------
# Getting relevant information for each position and the specific functions
rb_wk <- wk_data$RB # previous week data
rb_rz <- rz_data$RB

# Getting YTD and ensuring we are getting people with reasonable passing yards & QBs
rb_ytd <- ytd_df$RB

rb_proj <- filter(proj_data, proj_pos == "RB")
rb_pts_vs <- pts_against("RB")

# Combining into on DF for QB
all_rb <- full_join(rb_proj, rb_wk, by = c("proj_player" = "prev_wk_player","proj_tm" = "prev_wk_tm","proj_pos" = "prev_wk_pos")) %>%
          full_join(rb_ytd, by = c("proj_player" = "ytd_rush_player",
                                     "proj_tm" = "ytd_rush_tm",
                                     "proj_pos" = "ytd_rush_pos")) %>%
          left_join(rb_rz, by = c("proj_player" = "rushing_player")) %>%
          left_join(all_d_data, by = c("proj_opp" = "def_tm")) %>%
          left_join(rb_pts_vs, by = c("proj_opp" = "pts_vs_tm")) %>%
          filter(proj_ffpts > 0) %>%
          mutate(total_touches = ytd_rec_target + ytd_rush_att,
                 high_value_touches = ytd_rec_target + round(rushing_ten_att/ytd_rush_g,2),
                 high_value_touches_per = round(high_value_touches / total_touches, 2))
write.csv(all_rb, paste0(tmp,"all_rb.csv"), row.names = F)

# WR ----------------------------------------------------------------------
# Getting relevant information for each position and the specific functions
wr_wk <- wk_data$WR # previous week data
wr_rz <- rz_data$WR

# Getting YTD and ensuring we are getting people with reasonable passing yards & QBs
wr_ytd <- ytd_df$WR

wr_proj <- filter(proj_data, proj_pos == "WR")
wr_pts_vs <- pts_against("WR")

all_wr <- full_join(wr_proj, wr_wk, by = c("proj_player" = "prev_wk_player","proj_tm" = "prev_wk_tm","proj_pos" = "prev_wk_pos")) %>%
          left_join(wr_ytd, by = c("proj_player"= "ytd_rec_player","proj_tm" = "ytd_rec_tm")) %>%
          left_join(wr_rz, by = c("proj_player" = "receiving_player")) %>%
          left_join(all_d_data, by = c("proj_opp" = "def_tm")) %>%
          left_join(wr_pts_vs, by = c("proj_opp" = "pts_vs_tm")) %>%
          filter(proj_ffpts > 0) %>% #, proj_pos != "RB", proj_pos != "TE", proj_pos != "HB")
          select(-ytd_rec_pos) # removing the position from before

# Getting the matchups script and adding to main function
wr_matchup <- wr_matchups(wk_num,fff)
all_wr <- left_join(all_wr, wr_matchup, by = c("proj_player" = "vs_cb_wr"))

write.csv(all_wr, paste0(tmp,"all_wr.csv"), row.names = F)

# TE ----------------------------------------------------------------------

# Getting relevant information for each position and the specific functions
te_wk <- wk_data$TE # previous week data
te_rz <- rz_data$TE

# Getting YTD and ensuring we are getting people with reasonable passing yards & QBs
te_ytd <- ytd_df$TE

te_proj <- filter(proj_data, proj_pos == "TE")
te_pts_vs <- pts_against("TE")

all_te <- full_join(te_proj, te_wk, by = c("proj_player" = "prev_wk_player","proj_tm" = "prev_wk_tm","proj_pos" = "prev_wk_pos")) %>%
          left_join(te_ytd, by = c("proj_player"= "ytd_rec_player","proj_tm" = "ytd_rec_tm")) %>%
          left_join(te_rz, by = c("proj_player" = "receiving_player")) %>%
          left_join(all_d_data, by = c("proj_opp" = "def_tm")) %>%
          left_join(te_pts_vs, by = c("proj_opp" = "pts_vs_tm")) %>%
          filter(proj_ffpts > 0) %>% #, proj_pos != "RB", proj_pos != "TE", proj_pos != "HB")
          select(-ytd_rec_pos) # removing the position from before
write.csv(all_te, paste0(tmp,"all_te.csv"), row.names = F)

# Current Week Projections ------------------------------------------------

# qb <- position_stats("QB",wk_num, data, tm_names) %>%
#       mutate(proj_tm = ifelse(proj_tm == "character(0)", "LVE", proj_tm),
#              proj_opp = ifelse(proj_opp == "character(0)", "LVE", proj_opp))
#
# # RB
# rb <- position_stats("RB",wk_num,data, tm_names) %>%
#   mutate(proj_tm = ifelse(proj_tm == "character(0)", "LVE", proj_tm),
#          proj_opp = ifelse(proj_opp == "character(0)", "LVE", proj_opp))
#
# # WR
# wr <- position_stats("WR",wk_num,data,tm_names) %>%
#   mutate(proj_tm = ifelse(proj_tm == "character(0)", "LVE", proj_tm),
#          proj_opp = ifelse(proj_opp == "character(0)", "LVE", proj_opp))
#
# # Getting WR matchup
# wr_matchup <- wr_matchups(wk_num,fff)
#
# # Adding in matchup stats
# wr <- left_join(wr, wr_matchup, by = c("proj_player" = "vs_cb_wr"))
#
# # TE
# te <- position_stats("TE",wk_num,data, tm_names) %>%
#   mutate(proj_tm = ifelse(proj_tm == "character(0)", "LVE", proj_tm),
#          proj_opp = ifelse(proj_opp == "character(0)", "LVE", proj_opp))

cat("Positional Data Successful\n")

# Combing into one DF ----------------------------------------------------

all_positions <- merge(all_qb,all_rb, all = TRUE) %>%
                 merge(all_wr, all = TRUE) %>%
                 merge(all_te, all = TRUE) %>%
                 left_join(vegas, by = c("proj_tm" = "team")) %>%
                 left_join(dvoa_defense, by = c("proj_opp" = "def_team")) %>%
                 left_join(dvoa_offense, by = c("proj_tm" = "off_team"))
write.csv(all_positions, paste0(tmp,"all_positions_one_df.csv"), row.names = F)

# Adding previous week DVOA -----------------------------------------------

dvoa_previous <- dvoa_defense
names(dvoa_previous) <- paste("prev_wk_dvoa", names(dvoa_previous), sep = "_")

all_positions <- left_join(all_positions, dvoa_previous, by = c("prev_wk_opp" = "prev_wk_dvoa_def_team"))
cat("Previous Week DVOA Successful\n")
write.csv(all_positions, paste0(tmp,"all_positions_prev_wk_dvoa.csv"), row.names = F)

# Adding Leverage Scores --------------------------------------------------
leverage <- get_leverage(wk_num, fff)

# Adding into full DF
all_positions <- left_join(all_positions, leverage, by = c("proj_player" = "player",
                                                           "proj_pos" = "pos",
                                                           "proj_tm" = "tm"))
write.csv(all_positions, paste0(tmp,"all_positions_leveraege.csv"), row.names = F)
cat("Leverage Score Successful\n")

# Adding pricing information ----
pricing <- get_pricing(wk_num, fff)

all_positions <- left_join(all_positions, pricing, by = c("proj_player" = "pricing_player",
                                                           "proj_pos" = "pricing_position",
                                                           "proj_tm" = "pricing_team"))
write.csv(all_positions, paste0(tmp,"all_positions_pricing.csv"), row.names = F)
cat("Pricing Data Import Successful\n")

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
write.csv(all_data, paste0(tmp,"all_data.csv"), row.names = F)
cat("Advanced Stats Successful\n")

# Adding in individual information from Football Outsiders ----
qb_df <- fo_qb(fo)
rb_df <- fo_rb(fo)
wr_df <- fo_pass_catchers("wr",fo)
te_df <- fo_pass_catchers("te",fo) %>% filter(rec_player != "T. Hill")

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
                                                pass_player, str_remove(pass_player, "\\.")),
                           pass_player = str_remove(pass_player, "[:punct:]"),
                           pass_player = str_remove_all(pass_player, "-"))

fo_all_positions[["pass_team"]] <- as.character(sapply(fo_all_positions[["pass_team"]], function(x) find_names(x, "fff_abbreviation")))

# Merging with rest of data
all_data2 <- all_data %>%
             mutate(proj_player_new = str_remove(proj_player, "(?<=[:upper:])([:lower:]{1,}[:upper:][:lower:]{1,}|[:lower:]{1,}|[:upper:]{1}[:lower:]{1,})(?=[:space:]|[:upper:])"),
                    proj_player_new = ifelse(proj_player == "Duke Johnson" & proj_tm == "HOU", "Duke Johnson", proj_player_new))


# Combining
all_data_fo_pos <- left_join(all_data2, fo_all_positions, by = c("proj_player_new" = "pass_player", "proj_tm" = "pass_team")) %>%
                   select(-proj_player_new)

write.csv(all_data_fo_pos, paste0(tmp,"all_data_fo_pos.csv"), row.names = F)
print("FO All Advanced Position Stats Combined")

# Adding in pace of place stats ----
pace <- get_pace(fo)
pace[["off_team"]] <- sapply(pace[["off_team"]], function(x) find_names(x, "fff_abbreviation"))
all_data_fo_pos_pace <- left_join(all_data_fo_pos, pace, by = c("proj_tm" = "off_team"))

print("Pace of Play Successful")

# Writing outexcel file
fname <- copy_and_write(all_data_fo_pos_pace,wk_num)
print("Data Written Succesfully")

# Returning full DF ----
#return(all_data_fo_pos_pace)
return(fname)

}

# Getting full dataframe --------------------------------------------------
# ptm <- proc.time()
#df <- shiny_df(7)
# end <- proc.time() - ptm
