## Global file for EMA app
## Inspiration: https://mraess.netlify.app/2018/07/the-awesomeness-that-is-the-global-r-file-or-how-to-clean-up-your-shiny-app/

library(tidyverse)
library(lubridate)
library(writexl)

# Reading in complete data for week ----
df <- readxl::read_xlsx("data/all_data_wk_4_2020.xlsx") %>%
      mutate(proj_opp = ifelse(proj_field == 2, paste("@",proj_opp, sep = ""), proj_opp))

#df <- readxl::read_xlsx("~/ff_shiny_app/ff_app/data/all_data_wk_4_2020.xlsx") # for use on computer

###
### Data Frame Creation
###

# Main DFS Panel ----
dfs_df <- select(df,
                 proj_player,
                 proj_pos,
                 proj_tm,
                 proj_opp,
                 fd_sal,
                 projected_own,
                 cash_odds,
                 gpp_odds,
                 implied_own,
                 fd_lev,
                 proj_ffpts,
                 proj_afpa,
                 proj_afpa_rk,
                 line,
                 total,
                 implied_total) %>%
  filter(fd_sal > 0) %>%
  mutate(points_per_1k = round(proj_ffpts / (fd_sal/1000),2))

names(dfs_df) <- str_remove(names(dfs_df), "proj_")

# QB Panel ----

# QB Opponent Defense
def_qb <- filter(df, proj_pos == "QB" & is.na(line) == F) %>%
          select(proj_player,
                proj_opp,
                 pts_vs_g,
                 pts_vs_passing_att,
                 pts_vs_passing_yds,
                 pts_vs_passing_td,
                 pts_vs_fantasy_per_game_fdpt,
                 def_red_zone_td,
                 def_red_zone_pct,
                 def_dvoa,
                 def_pass_dvoa,
                 def_dline_pass_rank,
                 def_dline_pass_adjusted_sack_rate,
                 def_pass_qb_rating_allowed,
                 def_pass_adj_net_yds_per_att,
                 off_oline_pass_adjusted_sack_rate) %>%
          mutate(DVOA_Diff = def_pass_dvoa - def_dvoa,
                 pts_vs_passing_att = round(as.numeric(pts_vs_passing_att) / as.numeric(pts_vs_g),2),
                 pts_vs_passing_yds = round(as.numeric(pts_vs_passing_yds) / as.numeric(pts_vs_g),2),
                 pts_vs_passing_td = round(as.numeric(pts_vs_passing_td) / as.numeric(pts_vs_g),2),
                 pts_vs_fantasy_per_game_fdpt = as.numeric(pts_vs_fantasy_per_game_fdpt)) %>%
          select(-pts_vs_g) %>%
          select(proj_player, proj_opp,
                 pts_vs_passing_att, pts_vs_passing_yds, pts_vs_passing_td, pts_vs_fantasy_per_game_fdpt,
                 def_red_zone_td, def_red_zone_pct,
                 def_dvoa, def_pass_dvoa, DVOA_Diff,
                 def_dline_pass_rank, def_dline_pass_adjusted_sack_rate,
                 def_pass_qb_rating_allowed,
                 def_pass_adj_net_yds_per_att, off_oline_pass_adjusted_sack_rate)

# QB Offense Stats
off_qb <- filter(df, proj_pos == "QB" & is.na(line) == F) %>%
  select(proj_player,
         proj_opp,
         ytd_pass_comp_per,
         ytd_pass_td,
         ytd_pass_yds_per_gm,
         ytd_pass_net_yds_per_att,
         passing_twenty_att,
         passing_twenty_td,
         passing_ten_att,
         passing_ten_td,
         off_dvoa,
         off_pass_dvoa,
         fd_sal,
         line)




# RB Panel ----

# RB Defense
rb_def <- filter(df, proj_pos == "RB" & is.na(line) == F) %>%
          select(proj_player,
                 ytd_rush_att,
                 proj_opp,
                 pts_vs_g,
                 pts_vs_fantasy_per_game_fdpt,
                 pts_vs_rec_tgt,
                 #pts_vs_rec_rec,
                 pts_vs_rec_yds,
                 pts_vs_rec_td,
                 pts_vs_rush_att,
                 pts_vs_rush_yds,
                 pts_vs_rush_td,
                 def_rush_yds_per_att,
                 #def_rush_yds_per_gm,
                 def_red_zone_td,
                 def_red_zone_pct,
                 def_dvoa,
                 def_rush_dvoa,
                 off_rush_dvoa,
                 def_dline_power_success,
                 def_dline_adj_line_yards,
                 def_dline_stuffed,
                 def_dline_2nd_level_yards,
                 def_dline_open_field_yards,
                 off_oline_adj_line_yards,
                 off_oline_power_success,
                 off_oline_stuffed,
                 off_oline_open_field_yards,
                 off_oline_2nd_level_yards
          ) %>%
          mutate(pts_vs_g =  as.numeric(pts_vs_g),
                 pts_vs_fantasy_per_game_fdpt = as.numeric(pts_vs_fantasy_per_game_fdpt),
                 pts_vs_rec_tgt = as.numeric(pts_vs_rec_tgt),
                 pts_vs_rec_yds = as.numeric(pts_vs_rec_yds),
                 pts_vs_rec_td = as.numeric(pts_vs_rec_td),
                 pts_vs_rush_att = as.numeric(pts_vs_rush_att),
                 pts_vs_rush_yds = as.numeric(pts_vs_rush_yds),
                 pts_vs_rush_td = as.numeric(pts_vs_rush_td),
                 dline_adj_line_yards = as.numeric(def_dline_adj_line_yards),
                 dline_2nd_level_yards = as.numeric(def_dline_2nd_level_yards),
                 dline_open_field_yards = as.numeric(def_dline_open_field_yards),
                 oline_adj_line_yards = as.numeric(off_oline_adj_line_yards),
                 oline_2nd_level_yards = as.numeric(off_oline_2nd_level_yards),
                 oline_open_field_yards = as.numeric(off_oline_open_field_yards),
                 pts_vs_rec_tgt = round(pts_vs_rec_tgt/pts_vs_g,1),
                 pts_vs_rec_yds = round(pts_vs_rec_yds/pts_vs_g,1),
                 pts_vs_rec_td = round(pts_vs_rec_td/pts_vs_g,1),
                 pts_vs_rush_att = round(pts_vs_rush_att/pts_vs_g,1),
                 pts_vs_rush_yds = round(pts_vs_rush_yds/pts_vs_g,1),
                 pts_vs_rush_td = round(pts_vs_rush_td/pts_vs_g,1),
                 pts_vs_total_touch = round(pts_vs_rush_att + pts_vs_rec_tgt,1),
                 DVOA_Advantage = round(def_rush_dvoa + off_rush_dvoa,1),
                 DVOA_Difference = round(def_rush_dvoa - def_dvoa,1),
                 net_adj_line_yd_diff = round(off_oline_adj_line_yards - (def_dline_adj_line_yards*-1),2),
                 power_success_diff = off_oline_power_success - def_dline_power_success,
                 second_level_yds_diff = as.numeric(off_oline_2nd_level_yards) - as.numeric(def_dline_2nd_level_yards),
                 open_fieldyards_diff = as.numeric(off_oline_open_field_yards) - as.numeric(def_dline_open_field_yards)) %>%
          filter(ytd_rush_att > 5) %>%
          select(proj_player,
                 proj_opp,
                 pts_vs_total_touch,
                 pts_vs_fantasy_per_game_fdpt,
                 pts_vs_rush_att,
                 pts_vs_rush_yds,
                 pts_vs_rush_td,
                 pts_vs_rec_tgt,
                 pts_vs_rec_yds,
                 def_dvoa,
                 def_rush_dvoa,
                 DVOA_Advantage,
                 DVOA_Difference,
                 def_dline_power_success,
                 power_success_diff,
                 def_dline_adj_line_yards,
                 net_adj_line_yd_diff)

# RB Offense

rb_off <- filter(df, proj_pos == "RB" & ytd_rush_att >5 & is.na(line) == F) %>%
          select(proj_player,
                 proj_opp,
                 ytd_rec_target,
                 ytd_rec_yds_per_gm,
                 ytd_rush_g,
                 ytd_rush_att,
                 ytd_rush_td,
                 ytd_rush_yds_per_att,
                 ytd_rush_yds_per_gm,
                 receiving_ten_tgt,
                 receiving_ten_td,
                 receiving_ten_per_tgt,
                 rushing_ten_att,
                 rushing_ten_td,
                 rushing_ten_per_rush,
                 rushing_five_att,
                 rushing_five_td,
                 rushing_five_per_rush,
                 line,
                 fd_sal) %>%
          mutate(ytd_rec_target = ifelse(is.na(ytd_rec_target) == T, 0, ytd_rec_target),
                 total_touches = ytd_rush_att + ytd_rec_target,
                 high_value_touches = ytd_rec_target + round(rushing_ten_att/ytd_rush_g,2),
                 high_value_touches_per = round(high_value_touches / total_touches, 2),
                 tt_per_thousand = round(total_touches / (fd_sal/1000),2),
                 hv_per_thousand = round(high_value_touches / (fd_sal/1000),2)) %>%
          select(proj_player, proj_opp,
                 total_touches, high_value_touches, high_value_touches_per,
                 ytd_rush_att:ytd_rush_yds_per_gm,
                 ytd_rec_target,ytd_rec_yds_per_gm,
                 receiving_ten_tgt:line,
                 fd_sal, tt_per_thousand, hv_per_thousand)


# WR Panel ----
