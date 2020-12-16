## Global file for EMA app
## Inspiration: https://mraess.netlify.app/2018/07/the-awesomeness-that-is-the-global-r-file-or-how-to-clean-up-your-shiny-app/

library(tidyverse)
library(lubridate)
library(writexl)

# Reading in complete data for week ----

# Finding the most recent week's data by leveraging the largest week #
data_files <- list.files(path = "/srv/connect/apps/ff_app/data/", pattern = "all_data_wk_\\d{1,}_2020\\.xlsx")
print(data_files)

weeks = as.numeric(str_extract(string = data_files, pattern = "\\d(?=_)"))
this_week_file <- data_files[weeks == max(weeks)]
cat(paste0("This weeks file is: ", this_week_file, " \n"))

# # Reading in specific excel file
df <- readxl::read_xlsx(paste0("data/", this_week_file)) %>%
     mutate(proj_opp = ifelse(proj_field == 2, paste("@",proj_opp, sep = ""), proj_opp))

#df <- readxl::read_xlsx("~/ff_shiny_app/ff_app/data/all_data_wk_15_2020.xlsx") # for use on computer

# Getting time
day_time <- Sys.time() - (60*60*6) # adjusting for time zone differences in shiny app

###
### Data Frame Creation
###

# Main DFS Panel ----
dfs_df <- select(df,
                 proj_player,
                 proj_pos,
                 proj_tm,
                 proj_opp,
                 pricing_current,
                 pricing_season_change,
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
  filter(pricing_current > 0) %>%
  mutate(points_per_1k = round(proj_ffpts / (pricing_current/1000),2))

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
                 def_dline_adjusted_sack_rate_rk,
                 def_dline_adjusted_sack_rate,
                 def_pass_qb_rating_allowed,
                 def_pass_adj_net_yds_per_att,
                 off_oline_adjusted_sack_rate) %>%
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
                 def_dline_adjusted_sack_rate_rk, def_dline_adjusted_sack_rate,
                 def_pass_qb_rating_allowed,
                 def_pass_adj_net_yds_per_att, off_oline_adjusted_sack_rate)

# QB Offense Stats ----

off_qb <- filter(df, proj_pos == "QB" & is.na(line) == F) %>%
          mutate(adv_passing_iay = round(as.numeric(adv_passing_iay) / as.numeric(ytd_pass_g), 2),
                 pass_dyar = round(pass_dyar/as.numeric(ytd_pass_g),2),
                 rush_yards = round(rush_yards / as.numeric(ytd_pass_g),2),
                 pass_yds_diff = round((pass_eyds - pass_yards)/as.numeric(ytd_pass_g),2),
                 rush_yds_diff = round((rush_eyds - rush_yards)/as.numeric(ytd_pass_g),2)) %>%
          select(proj_player,
                 proj_opp,
                 # YTD Raw Data
                 ytd_pass_yds_per_gm,
                 ytd_pass_td,
                 # Advanced passing stats
                 adv_passing_iay,
                 pass_dyar,
                 adv_passing_ontgt_per,
                 pass_yds_diff,
                 adv_passing_bad_per,
                 adv_passing_prss_per,
                 # Rushing stats
                 rush_dyar,
                 rush_yards,
                 rush_yds_diff,
                 # Red Zone stats
                 passing_twenty_att,
                 passing_twenty_td,
                 # DFS
                 pricing_current,
                 implied_total)



# RB Panel ----

# RB Defense
rb_def <- filter(df, proj_pos == "RB" & is.na(line) == F) %>%
          mutate(pts_vs_g = as.numeric(pts_vs_g),
                 pts_vs_fantasy_per_game_fdpt = as.numeric(pts_vs_fantasy_per_game_fdpt),
                 pts_vs_rec_tgt = as.numeric(pts_vs_rec_tgt),
                 pts_vs_rec_yds = as.numeric(pts_vs_rec_yds),
                 pts_vs_rec_td = as.numeric(pts_vs_rec_td),
                 pts_vs_rush_yds = as.numeric(pts_vs_rush_yds),
                 pts_vs_rush_td = as.numeric(pts_vs_rush_td),
                 def_dline_adjusted_line_yards = as.numeric(def_dline_adjusted_line_yards),
                 def_dline_second_level_yards = as.numeric(def_dline_second_level_yards),
                 def_dline_open_field_yards = as.numeric(def_dline_open_field_yards),
                 off_oline_adjusted_line_yards = as.numeric(off_oline_adjusted_line_yards),
                 off_oline_second_level_yards = as.numeric(off_oline_second_level_yards),
                 off_oline_open_field_yards = as.numeric(off_oline_open_field_yards),
                 pts_vs_rec_tgt = round(pts_vs_rec_tgt/pts_vs_g,1),
                 pts_vs_rec_yds = round(pts_vs_rec_yds/pts_vs_g,1),
                 pts_vs_rec_td = round(pts_vs_rec_td/pts_vs_g,1),
                 pts_vs_rush_att = round(as.numeric(pts_vs_rush_att)/as.numeric(pts_vs_g),1),
                 pts_vs_rush_yds = round(pts_vs_rush_yds/pts_vs_g,1),
                 pts_vs_rush_td = round(pts_vs_rush_td/pts_vs_g,1),
                 pts_vs_total_touch = round(as.numeric(pts_vs_rush_att) + as.numeric(pts_vs_rec_tgt),1),
                 DVOA_Advantage = round(def_rush_dvoa + off_rush_dvoa,1),
                 DVOA_Difference = round(def_rush_dvoa - def_dvoa,1),
                 net_adj_line_yd_diff = round(off_oline_adjusted_line_yards - (def_dline_adjusted_line_yards*-1),2),
                 power_success_diff = off_oline_power_success_rate - def_dline_power_success_rate,
                 second_level_yds_diff = as.numeric(off_oline_second_level_yards) - as.numeric(def_dline_second_level_yards),
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
                 def_dline_power_success_rate,
                 power_success_diff,
                 def_dline_adjusted_line_yards,
                 net_adj_line_yd_diff)

# RB Offense

rb_off <- filter(df, proj_pos == "RB" & ytd_rush_att >5 & is.na(line) == F) %>%
          # First, mutating a few columns
          mutate(ytd_rec_target = ifelse(is.na(ytd_rec_target) == T, 0, ytd_rec_target),
                 #total_touches = ytd_rush_att + ytd_rec_target,
                 #high_value_touches = ytd_rec_target + round(rushing_ten_att/ytd_rush_g,2),
                 #high_value_touches_per = round(high_value_touches / total_touches, 2),
                 tt_per_thousand = round(total_touches / (pricing_current/1000),2),
                 hv_per_thousand = round(high_value_touches / (pricing_current/1000),2),
                 rush_eyard_diff = round((rush_eyds - rush_yards)/ytd_rush_g,2),
                 rec_eyard_diff = round((rec_eyds - rec_yards)/ytd_rush_g,2),
                 rec_dyar = round(rec_dyar/ytd_rush_g,2),
                 rush_dyar = round(rush_dyar/ytd_rush_g,2)) %>%
          # Selected in order of presentation
          select(proj_player, proj_opp,
                 total_touches, high_value_touches, high_value_touches_per,
                 ytd_rush_att, ytd_rush_td, ytd_rush_yds_per_gm,
                 rush_dyar,rush_dvoa,rush_eyard_diff,rush_suc_rate,
                 ytd_rec_target,ytd_rec_yds_per_gm,
                 rec_dyar,rec_dvoa,rec_eyard_diff,
                 receiving_ten_tgt,
                 rushing_ten_att,rushing_ten_td,rushing_ten_per_rush,
                 line, pricing_current, tt_per_thousand, hv_per_thousand,
                 )

# WR Panel ----

# WR Defense
wr_def <- filter(df, proj_pos == "WR" & is.na(line) == F & ytd_rec_target > 2) %>%
          select(proj_player,
                 proj_opp,
                 pts_vs_g,
                 pts_vs_fantasy_per_game_fdpt,
                 pts_vs_rec_tgt,
                 pts_vs_rec_yds,
                 pts_vs_rec_td,
                 def_red_zone_td,
                 def_red_zone_pct,
                 def_pass_adj_net_yds_per_att,
                 def_pass_yds_per_gm,
                 def_tot_yds_per_play,
                 def_dvoa,
                 def_pass_dvoa,
                 off_pass_dvoa,
                 off_oline_adjusted_sack_rate,
                 def_dline_adjusted_sack_rate_rk,
                 def_dline_sacks,
                 def_dline_adjusted_sack_rate
          ) %>%
          mutate(pts_vs_g =  as.numeric(pts_vs_g),
                 pts_vs_fantasy_per_game_fdpt = as.numeric(pts_vs_fantasy_per_game_fdpt),
                 pts_vs_rec_tgt = as.numeric(pts_vs_rec_tgt),
                 pts_vs_rec_yds = as.numeric(pts_vs_rec_yds),
                 pts_vs_rec_td = round(as.numeric(pts_vs_rec_td),1),
                 oline_adjusted_sack_rate = as.numeric(off_oline_adjusted_sack_rate),
                 dline_rank = as.numeric(def_dline_adjusted_sack_rate_rk),
                 dline_sacks = as.numeric(def_dline_sacks),
                 dline_adjusted_sack_rate = as.numeric(def_dline_adjusted_sack_rate),
                 pts_vs_rec_tgt = round(pts_vs_rec_tgt/pts_vs_g,1),
                 pts_vs_rec_yds = round(pts_vs_rec_yds/pts_vs_g,1),
                 pts_vs_rec_td = round(pts_vs_rec_td/pts_vs_g,1),
                 DVOA_Advantage = def_pass_dvoa + off_pass_dvoa,
                 DVOA_Difference = def_pass_dvoa - def_dvoa,
                 sack_rate_diff = round(off_oline_adjusted_sack_rate - def_dline_adjusted_sack_rate,1)) %>%
          select(proj_player,
                 proj_opp,
                 pts_vs_fantasy_per_game_fdpt:pts_vs_rec_td,
                 def_red_zone_td:def_tot_yds_per_play,
                 def_dvoa,
                 def_pass_dvoa,
                 DVOA_Advantage,
                 DVOA_Difference,
                 dline_rank:dline_adjusted_sack_rate,
                 sack_rate_diff)

# WR Offense Table

wr_off <- filter(df, proj_pos == "WR"  & is.na(line) == F & ytd_rec_target > 3) %>%
          mutate(vs_cb_fpt = as.numeric(vs_cb_fpt),
                 tgt_per_thousand = round(ytd_rec_target / (pricing_current/1000),2),
                 rec_eyard_diff = round((rec_eyds - rec_yards)/ytd_rec_g,2),
                 rec_dyar = round(rec_dyar/ytd_rec_g,2),
                 #air_yards = as.numeric(adv_receiving_adot)*ytd_rec_target,
                 air_yds_per_thousand = round(adv_receiving_air_yards / (pricing_current/1000),2)) %>%
                 #racr = round(ytd_rec_yds_per_gm/air_yards,2)) %>%
         select(proj_player,
                proj_opp,
                ytd_rec_target,
                ytd_rec_yds_per_gm,
                ytd_rec_td,
                rec_dyar,
                rec_dvoa,
                rec_eyard_diff,
                adv_receiving_adot,
                adv_receiving_air_yards,
                adv_receiving_racr,
                adv_receiving_target_share,
                #adv_receiving_drop_per,
                adv_receiving_air_yard_share,
                #adv_receiving_rat,
                adv_receiving_wopr,
                receiving_twenty_tgt,
                #receiving_twenty_td,
                receiving_twenty_per_tgt,
                #off_pass_dvoa,
                vs_cb_tar,
                vs_cb_fpt,
                vs_cb_shad,
                vs_cb_matchup,
                pricing_current,
                tgt_per_thousand,
                implied_total)


# TE Panel Data ----

### TE Defense ----

te_def <- filter(df, proj_pos == "TE" & is.na(line) == F) %>%
  select(proj_player,
         proj_opp,
         pts_vs_g,
         pts_vs_fantasy_per_game_fdpt,
         pts_vs_rec_tgt,
         pts_vs_rec_yds,
         pts_vs_rec_td,
         def_red_zone_td,
         def_red_zone_pct,
         def_pass_adj_net_yds_per_att,
         def_pass_yds_per_gm,
         def_tot_yds_per_play,
         def_dvoa,
         def_pass_dvoa,
         off_pass_dvoa,
         off_oline_adjusted_sack_rate,
         def_dline_adjusted_sack_rate_rk,
         def_dline_sacks,
         def_dline_adjusted_sack_rate
  ) %>%
  mutate(pts_vs_g =  as.numeric(pts_vs_g),
         pts_vs_fantasy_per_game_fdpt = as.numeric(pts_vs_fantasy_per_game_fdpt),
         pts_vs_rec_tgt = as.numeric(pts_vs_rec_tgt),
         pts_vs_rec_yds = as.numeric(pts_vs_rec_yds),
         pts_vs_rec_td = round(as.numeric(pts_vs_rec_td),1),
         oline_pass_adjusted_sack_rate = as.numeric(off_oline_adjusted_sack_rate),
         dline_pass_rank = as.numeric(def_dline_adjusted_sack_rate_rk),
         dline_pass_sacks = as.numeric(def_dline_sacks),
         dline_pass_adjusted_sack_rate = as.numeric(def_dline_adjusted_sack_rate),
         pts_vs_rec_tgt = round(pts_vs_rec_tgt/pts_vs_g,1),
         pts_vs_rec_yds = round(pts_vs_rec_yds/pts_vs_g,1),
         pts_vs_rec_td = round(pts_vs_rec_td/pts_vs_g,1),
         DVOA_Advantage = round(def_pass_dvoa + off_pass_dvoa,2),
         DVOA_Difference = round(def_pass_dvoa - def_dvoa,2),
         sack_rate_diff = round(off_oline_adjusted_sack_rate - def_dline_adjusted_sack_rate,1)) %>%
  select(proj_player,
         proj_opp,
         pts_vs_fantasy_per_game_fdpt:pts_vs_rec_td,
         def_red_zone_td:def_tot_yds_per_play,
         def_dvoa,
         def_pass_dvoa,
         DVOA_Advantage,
         DVOA_Difference,
         def_dline_adjusted_sack_rate_rk:def_dline_adjusted_sack_rate,
         sack_rate_diff)

# TE Offense Table ----

te_off <- filter(df, proj_pos == "TE"  & is.na(line) == F & ytd_rec_target > 3) %>%
          mutate(tgt_per_thousand = round(ytd_rec_target / (pricing_current/1000),2),
                 rec_eyard_diff = round((rec_eyds - rec_yards)/ytd_rec_g,2),
                 rec_dyar = round(rec_dyar/ytd_rec_g,2),
                 air_yards = as.numeric(adv_receiving_adot)*ytd_rec_target,
                 air_yds_per_thousand = round(air_yards / (pricing_current/1000),2),
                 racr = round(ytd_rec_yds_per_gm/air_yards,2)) %>%
          select(proj_player,
                 proj_opp,
                 ytd_rec_target,
                 ytd_rec_yds_per_gm,
                 ytd_rec_td,
                 rec_dyar,
                 rec_dvoa,
                 rec_eyard_diff,
                 adv_receiving_adot,
                 adv_receiving_air_yards,
                 adv_receiving_racr,
                 adv_receiving_target_share,
                 adv_receiving_air_yard_share,
                 adv_receiving_wopr,
                 #adv_receiving_drop_per,
                 #adv_receiving_rat,
                 off_pass_dvoa,
                 receiving_twenty_tgt,
                 receiving_twenty_td,
                 receiving_twenty_per_tgt,
                 pricing_current,
                 tgt_per_thousand,
                 implied_total)



#######
####### Misc Text in App ----
#######

qb_off_legend <- '<span style="color: #666666;"><font size=-1><strong>Legend:</strong>
<br><strong>IAY</strong> = intended air yards (yards on all passes, complete or not) || <strong>DYAR</strong> = Defense-adjusted Yards Above Replacement. This gives the value of the quarterbacks performance compared to replacement level, adjusted for situation and opponent and then translated into yardage.
<br><strong>Eyds - yds</strong> = Effective yards vs actual yards - translates DVOA into a yards/att number. Greater difference = played better than standard yards indicates and vice versa. More dependent on usage than DYAR.
<br><strong>On Target </strong>= percentage of throws on target || <strong>Bad Thrrow %</strong> = poor throws per attempt || <strong>Pressure %</strong> = percentage of pressures per drop back<br><br></span></font>'

rb_def_legend <- '<span style="color: #666666;"><font size=-1><strong>Legend:</strong>
<br><strong>Total Touches:</strong> Rush Att + Target by RB vs. Defense ||&nbsp;<strong>Rushing Advantage</strong> = Rush D DVOA + Rush Off DVOA, higher is better || <strong>DVOA Difference</strong> = Rush DVOA - Defense DVOA, lower means rushing d is a strength (i.e., compartively better than overall)
<br><strong>Power Success</strong> = % runs on 3rd/4th down OR 1st/2nd &amp; goal from &lt;= 2 yds which were successful; Difference between offense is next column ||&nbsp;<strong>DVOA Difference</strong> = O Line success (%) - D Line Success (%) ||&nbsp;<strong>Adj Net Yards</strong> = Adjusted Yds allowed by D line
<br><strong>Difference vs Off</strong> = Adj Net Yds from Offense - Defense, higher is better<br><br></span></font>'

rb_off_legend <- '<span style="color: #666666;"><font size=-1><strong>Legend:</strong>
<br><strong>High Value</strong> = Rushes inside 10 yd line + target (also expressed as % total touches) || <strong>DYAR</strong> = Defense-adjusted Yards Above Replacement (performance on plays with RB carry/catch vs. replacement level, adjusted for situation and translated to yardage)
<br><strong>DVOA</strong> = &nbsp;Defense-adjusted Value Over Average. Value, per play, over an average running back in the same game situations. More positive DVOA = better performance (negative = below-average). The simple version: <em>DYAR means a running back with more total value. DVOA means a running back with more value per play</em>. || <strong>Diff</strong> = Difference between Effective Yards (translation of DVOA into a yards per attempt) and regular yards. Players with more Effective Yards vs standard yards played better than standard stats would otherwise indicate (this measure is dependent on usage than DYAR). || <strong>Suc.</strong> % = &nbsp;successful running plays (the definition of success being different based on down and distance) divided by total running plays. A player with higher DVOA and a low success rate mixes long runs with downs getting stuffed at the line of scrimmage. A player with lower DVOA and a high success rate generally gets the yards needed, but doesn&#39;t often get more. It is not adjusted for opponent.<br><br></span></font>'

receiver_def_legend <- '<span style="color: #666666;"><font size=-1><strong>Legend:</strong>
<br><strong>Def (DVOA):</strong> Total defense DVOA (<strong>passing only = Pass</strong>) || <strong>Pass Adv</strong> = Pass D DVOA (opponent) + Pass Off (own team) DVOA, higher is better || <strong>(DVOA) Difference</strong> = Pass DVOA - Defense DVOA, lower means passing d is a strength (i.e., compartively better than overall)
<br><strong>Adj Sack Rate</strong> % of sacks on each dropback by defensive live || <strong>Sake Rate Diff</strong> = Offensive O Line Sack Rate - Defense D Line Sack rate, greater = better and indicative of more time to throw<br><br></span></font>'

wr_off_legend <- '<span style="color: #666666;"><font size=-1><strong>Legend:</strong>
<p><strong>DYAR&nbsp;</strong>= Defense-adjusted Yards Above Replacement (performance on plays with WR catch vs. replacement level, adjusted for situation and translated to yardage) ||
<strong>DVOA&nbsp;</strong>= &nbsp;Defense-adjusted Value Over Average. Value, per play, over an average wide receiver in the same game situations. More positive DVOA = better performance (negative = below-average).
The simple version: DYAR means a wide receiver with more total value. DVOA means a wide receiver with more value per play. || <strong>EYds - Yds</strong> = Difference between Effective Yards (translation of DVOA into a yards per attempt) and regular yards. Players with more Effective Yards vs standard yards played better than standard stats would otherwise indicate (this measure is dependent on usage than DYAR). ||
<strong>ADOT&nbsp;</strong>= average depth of target (in yds) ||
<strong>Air Yards</strong> (yds/gm) = targets / gm * ADOT (yds/tgt); how many yards a player is targeted with, on average, per game. ||
<strong>RACR</strong>= Receiver Air Conversion Ratio (Receiving Yards / Air Yards). RACR is an efficiency metric that rolls up catch rate and yards after the catch into one number. It can also be thought of as the number of receiving yards a player creates for every air yard thrown at him. ||
<strong>Target and Air Yard %</strong> = Share (%) of total passes (target) and air yards for the player || <strong>WOPR</strong> = Weighted opportunity rating that incorporates a players share of team targets and air yards (1.5 x Target Market Share + 0.7 x Air Yards Market Share). This essentially is trying to equate slot receivers (high target #, low air yards) with deep threats (low target #, high air yards) to get a better picture of usage.<br><br></span></font>'

te_off_legend <- '<span style="color: #666666;"><font size=-1><strong>Legend:</strong>
<p><strong>DYAR</strong>= Defense-adjusted Yards Above Replacement (performance on plays with TE catch vs. replacement level, adjusted for situation and translated to yardage) ||
<strong>DVOA</strong> = Defense-adjusted Value Over Average. Value, per play, over an average tight end in the same game situations. More positive DVOA = better performance (negative = below-average). The simple version: DYAR means a tight end with more total value. DVOA means a wide receiver with more value per play. ||
<strong>EYds - Yds</strong> = Difference between Effective Yards (translation of DVOA into a yards per attempt) and regular yards. Players with more Effective Yards vs standard yards played better than standard stats would otherwise indicate (this measure is dependent on usage than DYAR). ||
<strong>ADOT</strong> = average depth of target (in yds) || <strong>Air Yards</strong> (yds/gm) = targets / gm * ADOT (yds/tgt); how many yards a player is targeted with, on average, per game. ||
<strong>RACR</strong>= Receiver Air Conversion Ratio (Receiving Yards / Air Yards). RACR is an efficiency metric that rolls up catch rate and yards after the catch into one number. It can also be thought of as the number of receiving yards a player creates for every air yard thrown at him. ||
<strong>Target and Air Yard %</strong> = Share (%) of total passes (target) and air yards for the player || <strong>WOPR</strong> = Weighted opportunity rating that incorporates a players share of team targets and air yards (1.5 x Target Market Share + 0.7 x Air Yards Market Share). This essentially is trying to equate slot receivers (high target #, low air yards) with deep threats (low target #, high air yards) to get a better picture of usage.
|| <strong> Pass DVOA </strong> = team passing DVOA <br><br></span></font>'


