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

# QB Panels ----

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


