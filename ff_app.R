#
# DFs Shiny
#

library(shiny)
library(tidyverse)
library(DT)
library(ggrepel)

df <- readxl::read_xlsx("data/all_data_wk_2_2020.xlsx") %>%
      mutate(proj_opp = ifelse(proj_field == 2, paste("@",proj_opp, sep = ""), proj_opp))
#df <- readxl::read_xlsx("~/ff_shiny_app/ff_app/data/all_data_wk_2_2020.xlsx")

#NOTE: 16 columns per table works relatively well

# DFS Specific Data -------------------------------------------------------
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
                 
            
# QB Data -----------------------------------------------------------------

# QB Opponent Defense Stats
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
                off_oline_pass_adjusted_sack_rate
                ) %>%
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
         def_pass_adj_net_yds_per_att, off_oline_pass_adjusted_sack_rate
  )

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
               

# RB Data -----------------------------------------------------------------


#
# RB Defense and Oline Table
#

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

#
# RB Offense Table
#

rb_off <- filter(df, proj_pos == "RB" & ytd_rush_att >5 & is.na(line) == F) %>%
          select(proj_player,
                 proj_opp,
                 ytd_rec_target,
                 ytd_rec_yds_per_gm,
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
                 tt_per_thousand = round(total_touches / (fd_sal/1000),2)) %>%
          select(proj_player, proj_opp,
                 total_touches, ytd_rush_att:ytd_rush_yds_per_gm,
                 ytd_rec_target,ytd_rec_yds_per_gm,
                 receiving_ten_tgt:line,
                 fd_sal, tt_per_thousand)

# WR Data -----------------------------------------------------------------

#
# WR Defense and Oline Table
#& ytd_rec_target > 2 --> hiding for week 1

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
         off_oline_pass_adjusted_sack_rate,
         def_dline_pass_rank,
         def_dline_pass_sacks,
         def_dline_pass_adjusted_sack_rate
  ) %>%
  mutate(pts_vs_g =  as.numeric(pts_vs_g),
         pts_vs_fantasy_per_game_fdpt = as.numeric(pts_vs_fantasy_per_game_fdpt),
         pts_vs_rec_tgt = as.numeric(pts_vs_rec_tgt),
         pts_vs_rec_yds = as.numeric(pts_vs_rec_yds),
         pts_vs_rec_td = round(as.numeric(pts_vs_rec_td),1),
         oline_pass_adjusted_sack_rate = as.numeric(off_oline_pass_adjusted_sack_rate),
         dline_pass_rank = as.numeric(def_dline_pass_rank),
         dline_pass_sacks = as.numeric(def_dline_pass_sacks),
         dline_pass_adjusted_sack_rate = as.numeric(def_dline_pass_adjusted_sack_rate),
         pts_vs_rec_tgt = round(pts_vs_rec_tgt/pts_vs_g,1),
         pts_vs_rec_yds = round(pts_vs_rec_yds/pts_vs_g,1),
         pts_vs_rec_td = round(pts_vs_rec_td/pts_vs_g,1),
         DVOA_Advantage = def_pass_dvoa + off_pass_dvoa,
         DVOA_Difference = def_pass_dvoa - def_dvoa,
         sack_rate_diff = round(off_oline_pass_adjusted_sack_rate - def_dline_pass_adjusted_sack_rate,1)) %>%
  select(proj_player,
         proj_opp,
         pts_vs_fantasy_per_game_fdpt:pts_vs_rec_td,
         def_red_zone_td:def_tot_yds_per_play,
         def_dvoa,
         def_pass_dvoa,
         DVOA_Advantage, 
         DVOA_Difference,
         dline_pass_rank:dline_pass_adjusted_sack_rate,
         sack_rate_diff)

#
# WR Offense Table
# & ytd_rec_target > 2 --> hiding for week 1

wr_off <- filter(df, proj_pos == "WR"  & is.na(line) == F & ytd_rec_target > 2) %>%
  select(proj_player,
         proj_opp,
         ytd_rec_target,
         ytd_rec_yds_per_target,
         ytd_rec_yds_per_gm,
         ytd_rec_td,
         receiving_twenty_tgt,
         receiving_twenty_td,
         receiving_twenty_per_tgt,
         receiving_ten_tgt,
         receiving_ten_td,
         receiving_ten_per_tgt,
         off_pass_dvoa,
         vs_cb_tar,
         vs_cb_fpt,
         vs_cb_shad,
         vs_cb_matchup,
         fd_sal,
         implied_total) %>%
      mutate(vs_cb_fpt = as.numeric(vs_cb_fpt),
             tgt_per_thousand = round(ytd_rec_target / (fd_sal/1000),2))
         #proj_rec_yds,
         #proj_rec_td)#,
         # vs_cb_tar,
         # vs_cb_c,
         # vs_cb_fpt,
         # vs_cb_shad,
         # vs_cb_matchup)

# TE Data -----------------------------------------------------------------

#
# TE Defense and Oline Table
# & ytd_rec_target > 2 --> hiding for week 1

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
         off_oline_pass_adjusted_sack_rate,
         def_dline_pass_rank,
         def_dline_pass_sacks,
         def_dline_pass_adjusted_sack_rate
  ) %>%
  mutate(pts_vs_g =  as.numeric(pts_vs_g),
         pts_vs_fantasy_per_game_fdpt = as.numeric(pts_vs_fantasy_per_game_fdpt),
         pts_vs_rec_tgt = as.numeric(pts_vs_rec_tgt),
         pts_vs_rec_yds = as.numeric(pts_vs_rec_yds),
         pts_vs_rec_td = round(as.numeric(pts_vs_rec_td),1),
         oline_pass_adjusted_sack_rate = as.numeric(off_oline_pass_adjusted_sack_rate),
         dline_pass_rank = as.numeric(def_dline_pass_rank),
         dline_pass_sacks = as.numeric(def_dline_pass_sacks),
         dline_pass_adjusted_sack_rate = as.numeric(def_dline_pass_adjusted_sack_rate),
         pts_vs_rec_tgt = round(pts_vs_rec_tgt/pts_vs_g,1),
         pts_vs_rec_yds = round(pts_vs_rec_yds/pts_vs_g,1),
         pts_vs_rec_td = round(pts_vs_rec_td/pts_vs_g,1),
         DVOA_Advantage = round(def_pass_dvoa + off_pass_dvoa,2),
         DVOA_Difference = round(def_pass_dvoa - def_dvoa,2),
         sack_rate_diff = round(off_oline_pass_adjusted_sack_rate - def_dline_pass_adjusted_sack_rate,1)) %>%
  select(proj_player,
         proj_opp,
         pts_vs_fantasy_per_game_fdpt:pts_vs_rec_td,
         def_red_zone_td:def_tot_yds_per_play,
         def_dvoa,
         def_pass_dvoa,
         DVOA_Advantage, 
         DVOA_Difference,
         dline_pass_rank:dline_pass_adjusted_sack_rate,
         sack_rate_diff)

#
# TE Offense Table
# & ytd_rec_target > 2 --> hiding for week 1

te_off <- filter(df, proj_pos == "TE" & is.na(line) == F) %>%
  select(proj_player,
         proj_opp,
         ytd_rec_target,
         ytd_rec_yds_per_target,
         ytd_rec_yds_per_gm,
         ytd_rec_td,
         receiving_twenty_tgt,
         receiving_twenty_td,
         receiving_twenty_per_tgt,
         receiving_ten_tgt,
         receiving_ten_td,
         receiving_ten_per_tgt,
         off_pass_dvoa,
         fd_sal,
         line,
         proj_rec_yds,
         proj_rec_td)

# UI Components -----------------------------------------------------------

# Fluidpage adjusts to window size
ui <- navbarPage("DFS Data",
    

# Main DFS Panel ------------------------------------------------------------

    
    tabPanel("DFS Specific",
                          

        # Application title
        fluidRow(
            column(3, 
                   selectInput("pos",
                                      h3("Position"),
                                      choices = list("QB","RB","WR","TE","DEF"),
                                      selected = "QB"))),
     
    
        # Sidebar with a slider input for number of bins 
        fluidRow(
            column(3,
                  sliderInput("salary",
                            "Minimum FanDuel Salary:",
                            min = min(dfs_df$fd_sal),
                            max = max(dfs_df$fd_sal),
                            value = c(min,max)
            )),
            column(3,
                   sliderInput("value",
                        "Points Per $1000",
                        min = min(dfs_df$points_per_1k, na.rm = T),
                        max = max(dfs_df$points_per_1k, na.rm = T),
                        value = c(min,max)
                   )),
            column(3,
                   sliderInput("lev",
                               "Leverage",
                               min = min(dfs_df$fd_lev),
                               max = max(dfs_df$fd_lev),
                               value = c(min,max)
                   )),
            column(3,
                   sliderInput("line",
                               "Line",
                               min = min(dfs_df[["line"]], na.rm = T),
                               max = max(dfs_df[["line"]], na.rm = T),
                               value = c(min,max)
                   ))),
    
            # Show a plot of the generated distribution
            fluidRow(
             column(12,
               DT::dataTableOutput("fanduel")
            )
        ),
        
        fluidRow(
            column(2,
                   selectInput("y_axis",
                               h3("Y Axis"),
                               choices = as.list(names(dfs_df)),
                               selected = "fd_sal")),
            column(2,
                   selectInput("x_axis",
                               h3("X Axis"),
                               choices = as.list(names(dfs_df)),
                               selected = "gpp_odds")),
            column(6,
                   plotOutput('plot', height = 500))
        ),
    
        fluidRow(column(12,textOutput("player_pool")))
        ),



# QB Panel ----------------------------------------------------------------

    tabPanel("QB",
             
             fluidRow(column(3,
                             checkboxGroupInput("qb_select",
                                                "Plot All Returned Players in Tables",
                                                choices = list("yes"),
                                                selected = c("yes")))),
             
             fluidRow(
               column(3,
                      sliderInput("qb_salary",
                                  "Minimum FanDuel Salary:",
                                  min = min(off_qb$fd_sal, na.rm = T),
                                  max = max(off_qb$fd_sal, na.rm = T),
                                  value = c(min,max)
                      )),
               column(3,
                      sliderInput("pass_dvoa",
                                  "Passing Offense DVOA",
                                  min = min(off_qb$off_pass_dvoa, na.rm = T),
                                  max = max(off_qb$off_pass_dvoa, na.rm = T),
                                  value = c(min,max)
                      )),
               column(3,
                      sliderInput("qb_yds_gm",
                                  "Pass Yds/Gm",
                                  min = min(off_qb$ytd_pass_yds_per_gm,  na.rm = T),
                                  max = max(off_qb$ytd_pass_yds_per_gm,  na.rm = T),
                                  value = c(min,max)
                      )),
               column(3,
                      sliderInput("qb_line",
                                  "Line",
                                  min = min(off_qb[["line"]], na.rm = T),
                                  max = max(off_qb[["line"]], na.rm = T),
                                  value = c(min,max)
                      ))),
             
             
             fluidRow(column(12, 
                             DT::dataTableOutput("off_qb"))),
             fluidRow(column(12,
                             p("Data Dictionary: Rz = Red Zone, ytd = Year to Date, 
                             DVOA = Defense-adjusted Value Over Average where negative is better for total, but not pass,
                             Dline = Defensive Line Ratings,
                             def_ = Raw Defense Stats"))),
             # fluidRow(
             #   column(2,
             #          selectInput("off_qb_y_axis",
             #                      h3("Y Axis"),
             #                      choices = as.list(names(off_qb)),
             #                      selected = "fd_sal")),
             #   column(2,
             #          selectInput("off_qb_x_axis",
             #                      h3("X Axis"),
             #                      choices = as.list(as.list(names(off_qb))),
             #                      selected = "ytd_pass_net_yds_per_att")),
             #   column(6,
             #          plotOutput('qb_off_plot', height = 500))
             # ),
             
             #
             # Sliders for Defense QB
             #
             
             fluidRow(
               column(3,
                      sliderInput("td_g",
                                  "TD Surrendered to QB / Game",
                                  min = min(def_qb[["pts_vs_passing_td"]], na.rm = T),
                                  max = max(def_qb[["pts_vs_passing_td"]], na.rm = T),
                                  value = c(min,max)
                      )),
               column(3,
                      sliderInput("fd_pts_gm",
                                  "FD Pts / Gm to QB",
                                  min = min(def_qb[["pts_vs_fantasy_per_game_fdpt"]], na.rm = T),
                                  max = max(def_qb[["pts_vs_fantasy_per_game_fdpt"]], na.rm = T),
                                  value = c(min,max)
                      )),
               column(3,
                      sliderInput("qb_dvoa_diff",
                                  "DVOA Difference",
                                  min = min(def_qb[["DVOA_Diff"]], na.rm = T),
                                  max = max(def_qb[["DVOA_Diff"]], na.rm = T),
                                  value = c(min,max)
                      )),
               column(3,
                      sliderInput("adj_net_yd_att",
                                  "Adj Net Yd / Att",
                                  min = min(def_qb[["def_pass_adj_net_yds_per_att"]], na.rm = T),
                                  max = max(def_qb[["def_pass_adj_net_yds_per_att"]], na.rm = T),
                                  value = c(min,max)
                      ))),
             
             
             fluidRow(
               column(12,DT::dataTableOutput("def_qb"))
             ),
             
             # fluidRow(
             #   column(2,
             #          selectInput("def_qb_y_axis",
             #                      h3("Y Axis"),
             #                      choices = as.list(names(def_qb)),
             #                      selected = "pts_vs_fantasy_per_game_fdpt")),
             #   column(2,
             #          selectInput("def_qb_x_axis",
             #                      h3("X Axis"),
             #                      choices = as.list(as.list(names(def_qb))),
             #                      selected = "pass_def_dvoa")),
             #   column(6,
             #          plotOutput('qb_def_plot', height = 500))),
             
             fluidRow(
               column(2,
                      selectInput("qb_y_axis",
                                  h3("Y Axis"),
                                  choices = as.list(c(names(def_qb), names(off_qb))),
                                  selected = "pts_vs_fantasy_per_game_fdpt")),
               column(2,
                      selectInput("qb_x_axis",
                                  h3("X Axis"),
                                  choices = as.list(c(names(def_qb), names(off_qb))),
                                  selected = "pass_def_dvoa")),
               column(2,
                      selectInput("qb_size",
                                  h3("Size"),
                                  choices = as.list(c(names(def_qb), names(off_qb))),
                                  selected = "pts_vs_passing_att")),
               column(6,
                      plotOutput('qbplot', height = 500))
             )

),

# RB Panel ----------------------------------------------------------------

tabPanel("RB",
         
         fluidRow(column(3,
                         checkboxGroupInput("rb_select",
                                            "Plot All Returned Players in Tables",
                                            choices = list("yes"),
                                            selected = c("yes")))),
         fluidRow(
           column(3,
                  sliderInput("rush_dvoa",
                              "Rushing DVOA",
                              min = min(rb_def$def_rush_dvoa, na.rm = T),
                              max = max(rb_def$def_rush_dvoa, na.rm = T),
                              value = c(min,max)
                  )),
           column(3,
                  sliderInput("net_yds_diff",
                              "Net Yards Differece",
                              min = min(rb_def$net_adj_line_yd_diff, na.rm = T),
                              max = max(rb_def$net_adj_line_yd_diff, na.rm = T),
                              value = c(min,max)
                  )),
           column(3,
                  sliderInput("dvoa_advantage",
                              "Rushing Advantage",
                              min = min(rb_def$DVOA_Advantage,  na.rm = T),
                              max = max(rb_def$DVOA_Advantage,  na.rm = T),
                              value = c(min,max)
                  )),
           column(3,
                  sliderInput("total_touches",
                              "Total Touches by RB",
                              min = min(rb_def$pts_vs_total_touch, na.rm = T),
                              max = max(rb_def$pts_vs_total_touch, na.rm = T),
                              value = c(min,max)
                  ))),

         
         column(12,
                div(DT::dataTableOutput("def_rb"), style = "font-size:95%")
         ),
         
         #
         # Offensive RB Stats
         #
         
         ## Sliders
         fluidRow(
           column(2,
                  sliderInput("tot_touches",
                              "Total Touches",
                              min = min(rb_off[["total_touches"]], na.rm = T),
                              max = max(rb_off[["total_touches"]], na.rm = T),
                              value = c(min,max)
                  )),
           column(2,
                  sliderInput("rush_att",
                              "Rushing Attempts / Gm",
                              min = min(rb_off[["ytd_rush_att"]], na.rm = T),
                              max = max(rb_off[["ytd_rush_att"]], na.rm = T),
                              value = c(min,max)
                  )),
           column(2,
                  sliderInput("rush_10_per",
                              "% Rushing Att Inside 10y",
                              min = min(rb_off[["rushing_ten_per_rush"]],  na.rm = T),
                              max = max(rb_off[["rushing_ten_per_rush"]],  na.rm = T),
                              value = c(min,max)
                  )),
           column(2,
                  sliderInput("rb_off_line",
                              "Line",
                              min = min(rb_off[["line"]], na.rm = T),
                              max = max(rb_off[["line"]], na.rm = T),
                              value = c(min,max)
                  )),
           column(2, 
                  sliderInput("rb_salary",
                              "Salary",
                              min = min(rb_off[["fd_sal"]],  na.rm = T),
                              max = max(rb_off[["fd_sal"]],  na.rm = T),
                              value = c(min,max)))
           ),
         
         ## RB Offense Table
         column(12,
                div(DT::dataTableOutput("off_rb"), style = "font-size:95%")
         ),
        
         #
         # RB Plot
         #
         fluidRow(
           column(2,
                  selectInput("rb_y_axis",
                              h3("Y Axis"),
                              choices = as.list(c(names(rb_def), names(rb_off))),
                              selected = "pts_vs_total_touch")),
           column(2,
                  selectInput("rb_x_axis",
                              h3("X Axis"),
                              choices = as.list(c(names(rb_def), names(rb_off))),
                              selected = "defense_dvoa")),
           column(2,
                  selectInput("rb_size",
                              h3("Size"),
                              choices = as.list(c(names(rb_def), names(rb_off))),
                              selected = "pts_vs_rush_att")),
           column(6,
                  plotOutput('rbplot', height = 500)))
),

# WR Panel ----------------------------------------------------------------
tabPanel("WR",
         
         fluidRow(column(3,
                         checkboxGroupInput("wr_select",
                                            "Plot All Returned Players in Tables",
                                            choices = list("yes"),
                                            selected = c("yes")))),
         fluidRow(
           column(3,
                  sliderInput("fd_pts_gm_wr",
                              "Fantasy Pts/G to WR",
                              min = min(wr_def[["pts_vs_fantasy_per_game_fdpt"]], na.rm = T),
                              max = max(wr_def[["pts_vs_fantasy_per_game_fdpt"]], na.rm = T),
                              value = c(min,max)
                  )),
           column(3,
                  sliderInput("wr_tg_vs",
                              "Targets/Gm to WR",
                              min = min(wr_def[["pts_vs_rec_tgt"]], na.rm = T),
                              max = max(wr_def[["pts_vs_rec_tgt"]], na.rm = T),
                              value = c(min,max)
                  )),
           column(3,
                  sliderInput("pass_yds_gm",
                              "Pass Yds/Gm",
                              min = min(wr_def[["def_pass_yds_per_gm"]],  na.rm = T),
                              max = max(wr_def[["def_pass_yds_per_gm"]],  na.rm = T),
                              value = c(min,max)
                  )),
           column(3,
                  sliderInput("wr_dvoa_advantage",
                              "DVOA Advantage",
                              min = min(wr_def[["DVOA_Advantage"]], na.rm = T),
                              max = max(wr_def[["DVOA_Advantage"]], na.rm = T),
                              value = c(min,max)
                  ))),
         
         fluidRow(column(12, 
                  div(DT::dataTableOutput("def_wr"), style = "font-size: 90%"))),
         
         #
         #  Offense WR Sliders and Table Presentation
         #
         
         fluidRow(
           column(2,
                  sliderInput("wr_tgt",
                              "YTD Targets Per Game",
                              min = min(wr_off[["ytd_rec_target"]], na.rm = T),
                              max = max(wr_off[["ytd_rec_target"]], na.rm = T),
                              value = c(min,max)
                  )),
           column(2,
                  sliderInput("wr_rz_20",
                              "% Targets Receiving Inside 20yd",
                              min = min(wr_off[["receiving_twenty_per_tgt"]], na.rm = T),
                              max = max(wr_off[["receiving_twenty_per_tgt"]], na.rm = T),
                              value = c(min,max)
                  )),
           column(2,
                  sliderInput("cb_pts_tgt",
                              "CB Matchup FD Pts/Tgt",
                              min = min(wr_off[["vs_cb_fpt"]],  na.rm = T),
                              max = max(wr_off[["vs_cb_fpt"]],  na.rm = T),
                              value = c(min,max)
                  )),
           column(2, 
                  sliderInput("wr_salary",
                              "Salary",
                              min = min(wr_off[["fd_sal"]],  na.rm = T),
                              max = max(wr_off[["fd_sal"]],  na.rm = T),
                              value = c(min,max))),
           column(2,
                  checkboxGroupInput("cb_matchup",
                              "CB Matchup Advantage",
                              choices = list("plus", "minus", "neutral"),
                              selected = c("plus", "minus", "neutral"))),
           column(2, 
                  selectInput("wr_opponent",
                              "Opponent",
                              choices = as.list(c("all", unique(wr_off$proj_opp))),
                              selected = "all"))),
         
         fluidRow(column(12,
                  div(DT::dataTableOutput("off_wr"), style = "font-size: 90%"))),
        
         #
         # WR Plot
         #
         fluidRow(
           column(2,
                  selectInput("wr_y_axis",
                              h3("Y Axis"),
                              choices = as.list(c(names(wr_def), names(wr_off))),
                              selected = "fd_sal")),
           column(2,
                  selectInput("wr_x_axis",
                              h3("X Axis"),
                              choices = as.list(c(names(wr_def), names(wr_off))),
                              selected = "defense_dvoa")),
           column(2,
                  selectInput("wr_size",
                              h3("Size"),
                              choices = as.list(c(names(wr_def), names(wr_off))),
                              selected = "pts_vs_rec_tgt")),
           column(6,
                  plotOutput('wrplot', height = 500)))
         
         
),

# TE Panel ----------------------------------------------------------------
tabPanel("TE",
         
         fluidRow(column(3,
                         checkboxGroupInput("te_select",
                                            "Plot All Returned Players in Tables",
                                            choices = list("yes"),
                                            selected = c("yes")))),
         
         #
         # TE Defense Stats
         #
         
         fluidRow(
           column(3,
                  sliderInput("fd_pts_gm_te",
                              "Fantasy Pts/G to TE",
                              min = min(te_def[["pts_vs_fantasy_per_game_fdpt"]], na.rm = T),
                              max = max(te_def[["pts_vs_fantasy_per_game_fdpt"]], na.rm = T),
                              value = c(min,max)
                  )),
           column(3,
                  sliderInput("te_tg_vs",
                              "Targets/Gm to TE",
                              min = min(te_def[["pts_vs_rec_tgt"]], na.rm = T),
                              max = max(te_def[["pts_vs_rec_tgt"]], na.rm = T),
                              value = c(min,max)
                  )),
           column(3,
                  sliderInput("pass_yds_gm_te",
                              "Pass Yds/Gm",
                              min = min(te_def[["def_pass_yds_per_gm"]],  na.rm = T),
                              max = max(te_def[["def_pass_yds_per_gm"]],  na.rm = T),
                              value = c(min,max)
                  )),
           column(3,
                  sliderInput("te_dvoa_advantage",
                              "DVOA Advantage",
                              min = min(te_def[["DVOA_Advantage"]], na.rm = T),
                              max = max(te_def[["DVOA_Advantage"]], na.rm = T),
                              value = c(min,max)
                  ))),
         
         fluidRow(column(12, 
                        div(DT::dataTableOutput("def_te")))),
         
         #
         #  Offense TE Sliders and Table Presentation
         #
         
         fluidRow(
           column(2,
                  sliderInput("te_tgt",
                              "YTD Targets Per Game",
                              min = min(te_off[["ytd_rec_target"]], na.rm = T),
                              max = max(te_off[["ytd_rec_target"]], na.rm = T),
                              value = c(min,max)
                  )),
           column(2,
                  sliderInput("te_rec_td",
                              "Receiving TD",
                              min = min(te_off[["ytd_rec_td"]],  na.rm = T),
                              max = max(te_off[["ytd_rec_td"]],  na.rm = T),
                              value = c(min,max)
                  )),
           column(2,
                  sliderInput("te_rz_20",
                              "% Targets Receiving Inside 20yd",
                              min = min(te_off[["receiving_twenty_per_tgt"]], na.rm = T),
                              max = max(te_off[["receiving_twenty_per_tgt"]], na.rm = T),
                              value = c(min,max)
                  )),
           column(2, 
                  sliderInput("te_salary",
                              "Salary",
                              min = min(te_off[["fd_sal"]],  na.rm = T),
                              max = max(te_off[["fd_sal"]],  na.rm = T),
                              value = c(min,max))),
           column(2,
                  sliderInput("off_pass_dvoa_te",
                              "Pass Off DVOA",
                              min = min(te_off[["off_pass_dvoa"]],  na.rm = T),
                              max = max(te_off[["off_pass_dvoa"]],  na.rm = T),
                              value = c(min,max)
                  ))),
           
         fluidRow(column(12,
                         div(DT::dataTableOutput("off_te")))),
         
         # fluidRow(
         #   column(3,
         #          sliderInput("te_salary",
         #                      "Minimum FanDuel Salary:",
         #                      min = min(te$fd_sal, na.rm = T),
         #                      max = max(te$fd_sal, na.rm = T),
         #                      value = c(min,max)
         #          )),
         #   column(3,
         #          sliderInput("te_dvoa",
         #                      "Total DVOA",
         #                      min = min(te$defense_dvoa, na.rm = T),
         #                      max = max(te$defense_dvoa, na.rm = T),
         #                      value = c(min,max)
         #          )),
         #   column(3,
         #          sliderInput("te_pass_dvoa",
         #                      "Pass D DVOA",
         #                      min = min(te$pass_def_dvoa,  na.rm = T),
         #                      max = max(te$pass_def_dvoa,  na.rm = T),
         #                      value = c(min,max)
         #          )),
         #   column(3,
         #          sliderInput("te_line",
         #                      "Line",
         #                      min = min(te[["line"]], na.rm = T),
         #                      max = max(te[["line"]], na.rm = T),
         #                      value = c(min,max)
         #          ))),
         # 
         # fluidRow(column(2,
         #                 
         #                 checkboxGroupInput("te_vars", "te columns to show:",
         #                                    names(te), selected = c("player",
         #                                                            "opp",
         #                                                            "ytd_rec_target",
         #                                                            "ytd_rec_rec",
         #                                                            "ytd_rec_yds_per_target",
         #                                                            "receiving_twenty_tgt",
         #                                                            "receiving_twenty_td",
         #                                                            "receiving_twenty_per_tgt",
         #                                                            "receiving_ten_tgt",
         #                                                            "receiving_ten_rec",
         #                                                            "receiving_ten_td",
         #                                                            "receiving_ten_per_tgt",
         #                                                            "defense_dvoa",
         #                                                            "pass_def_dvoa",
         #                                                            "dline_pass_rank",
         #                                                            "oline_pass_adjustedsack_rate",
         #                                                            "fd_sal",
         #                                                            "line"))
         # ),
         # 
         # 
         # column(10,
         #        div(DT::dataTableOutput("tetable"), style = "font-size:75%")
         # )),
         # fluidRow(column(12,
         #                 p("Data Dictionary: Rz = Red Zone, ytd = Year to Date, 
         #                     DVOA = Defense-adjusted Value Over Average where negative is better,
         #                     Dline = Defensive Line Ratings,
         #                     def_ = Raw Defense Stats"))),
         # 
         
         #
         # WR Plot
         #
         fluidRow(
           column(2,
                  selectInput("te_y_axis",
                              h3("Y Axis"),
                              choices = as.list(c(names(te_def), names(te_off))),
                              selected = "fd_sal")),
           column(2,
                  selectInput("te_x_axis",
                              h3("X Axis"),
                              choices = as.list(c(names(te_def), names(te_off))),
                              selected = "defense_dvoa")),
           column(2,
                  selectInput("te_size",
                              h3("Size"),
                              choices = as.list(c(names(te_def), names(te_off))),
                              selected = "pts_vs_rec_tgt")),
           column(6,
                  plotOutput('teplot', height = 500)))
         
         
))


# Server Function ---------------------------------------------------------
server <- function(input, output) {
    
    

# Main Panel Server -------------------------------------------------------
    
    output$fanduel <- renderDataTable({
        
        # Editing table for rendering
        render_table <- subset(dfs_df,
                               fd_sal >= input$salary[1] & fd_sal <= input$salary[2] &
                               points_per_1k >= input$value[1] & points_per_1k <= input$value[2] &
                               fd_lev >= input$lev[1] & fd_lev <= input$lev[2] &
                               line >= input$line[1] & line <= input$line[2] &
                               pos == input$pos | is.na(fd_sal)) %>%
                        select(-implied_own)
        
        # Selecting program
        render_table <- render_table[,c("player", "pos", "tm", "opp",
                                        "ffpts","points_per_1k","fd_lev",
                                        "afpa","afpa_rk",
                                        "fd_sal", "projected_own", "cash_odds", "gpp_odds",
                                        "line","total","implied_total")]

        # Better Output - customizing column names
        dfs_container <- htmltools::withTags(table(
                                            class = 'display',
                                            thead(
                                              tr(
                                                th(colspan = 4,''),
                                                th(class = 'dt-center', colspan = 3, 'Point Projections'),
                                                th(class = 'dt-center', colspan = 2, 'aFPA'),
                                                th(class = 'dt-center', colspan = 4, 'DFS Leverage'),
                                                th(class = 'dt-center', colspan = 3, 'Vegas')
                                              ),
                                              tr(
                                                th(colspan = 1, 'Player'),
                                                th(colspan = 1, 'Position'),
                                                th(colspan = 1, 'Team'),
                                                th(colspan = 1, 'Opp'),
                                                th(colspan = 1, 'Points'),
                                                th(colspan = 1, 'Pt/$1k'),
                                                th(colspan = 1, 'Leverage'),
                                                th(colspan = 1, 'aFPA'),
                                                th(colspan = 1, 'aFPA Rank'),
                                                th(colspan = 1, 'Salary ($)'),
                                                th(colspan = 1, 'Own (%)'),
                                                th(colspan = 1, 'Cash (%)'),
                                                th(colspan = 1, 'GPP (%)'),
                                                th(colspan = 1, 'Line'),
                                                th(colspan = 1, 'Total'),
                                                th(colspan = 1, 'Implied Total'))
                                            )
                                          ))

        datatable(render_table, 
                  rownames = F, 
                  container = dfs_container, 
                  options = list(pageLength = 20, 
                                 lengthMenu = c(10,20,30),
                                 columnDefs = list(list(className = 'dt-center', targets = 'all'))))

       })
    
    
    # Graph Output
    dat <- reactive({

        s1 <- input$fanduel_rows_selected

        render_table <- subset(dfs_df,
                               fd_sal >= input$salary[1] & fd_sal <= input$salary[2] &
                                   points_per_1k >= input$value[1] & points_per_1k <= input$value[2] &
                                   fd_lev >= input$lev[1] & fd_lev <= input$lev[2] &
                                   line >= input$line[1] & line <= input$line[2] &
                                   pos == input$pos) %>% 
                        select(-implied_own) %>%
                        .[s1,]

        return(render_table)
    })
    
    # Player pool (start for now)
    output$player_pool <- renderPrint({ 
      
      s1 <- input$fanduel_rows_selected
      
           dat() %>%
           .[s1,] %>%
           select(player)
    })
    
    output$plot = renderPlot({

        plot_data <- dat()
        ggplot(plot_data, aes(x = plot_data[[input$x_axis]], plot_data[[input$y_axis]])) +
              xlab(input$x_axis) +
              ylab(input$y_axis) +
              geom_point(size = 6, color = "#0000b7", alpha = 0.5) +
              geom_text_repel(aes(label = player), hjust = 0, vjust = -1) +
              theme_bw() +
              theme(
                axis.title = element_text(size = 12, face = "bold")
              )
    })

# QB Tab Data -------------------------------------------------------------

    #
    # Offense QB
    #
    
    # Getting Subsetted Database Based on Sliders
    output$off_qb <- renderDataTable({
      
      # Offense QB Container
      off_qb_container <- htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(colspan = 2,''),
            th(class = 'dt-center', colspan = 4, 'Year to Date Stats'),
            th(class = 'dt-center', colspan = 4, 'RZ Offense'),
            th(class = 'dt-center', colspan = 2, 'DVOA'),
            th(class = 'dt-center', colspan = 2, 'DFS Info')
          ),
          tr(
            th(colspan = 1, 'Player'),
            th(colspan = 1, 'Opp'),
            th(colspan = 1, 'Comp %'),
            th(colspan = 1, 'TD'),
            th(colspan = 1, 'Yds/Gm'),
            th(colspan = 1, 'Net Yds/Att'),
            th(colspan = 1, 'Att w/in 20yd'),
            th(colspan = 1, 'TD w/in 20yd'),
            th(colspan = 1, 'Att w/in 10yd'),
            th(colspan = 1, 'TD w/in 20yd'),
            th(colspan = 1, 'Offense'),
            th(colspan = 1, 'Pass Offense'),
            th(colspan = 1, 'Salary ($)'),
            th(colspan = 1, 'Line'))
        )
      ))
      
      render_qb <-  subset(off_qb,
                           fd_sal >= input$qb_salary[1] & fd_sal <= input$qb_salary[2] &
                           line >= input$qb_line[1] & line <= input$qb_line[2] &
                           off_pass_dvoa >= input$pass_dvoa[1] & off_pass_dvoa <= input$pass_dvoa[2] &
                           ytd_pass_yds_per_gm >= input$qb_yds_gm[1] & ytd_pass_yds_per_gm <= input$qb_yds_gm[2] | is.na(fd_sal))
      
      #DT::datatable(render_qb, rownames = F, options = list(pageLength = 15, lengthMenu = c(10,15,20)))
      
      datatable(render_qb, 
                rownames = F, 
                container = off_qb_container, 
                options = list(pageLength = 10, 
                               lengthMenu = c(10,20,30),
                               columnDefs = list(list(className = 'dt-center', targets = 'all'))))
      
    })
    
    # QB Graph Output
    qb_off_reactive <- reactive({

      qb_s1 <- input$off_qb_rows_selected

      qb_render_table <- subset(off_qb,
                               fd_sal >= input$qb_salary[1] & fd_sal <= input$qb_salary[2] &
                                 line >= input$qb_line[1] & line <= input$qb_line[2] &
                                 off_pass_dvoa >= input$pass_dvoa[1] & off_pass_dvoa <= input$pass_dvoa[2] &
                                 ytd_pass_yds_per_gm >= input$qb_yds_gm[1] & ytd_pass_yds_per_gm <= input$qb_yds_gm[2]| is.na(fd_sal))
      
      if(length(input$qb_select) == 0) {qb_render_table <- qb_render_table[qb_s1,]}
      
  
      return(qb_render_table)
    })
    
    # Output variable creation for QB
    output$qb_off_plot = renderPlot({

      qb_plot_data <- qb_off_reactive()
      ggplot(qb_plot_data, aes(x = qb_plot_data[[input$off_qb_x_axis]], qb_plot_data[[input$off_qb_y_axis]])) +
        xlab(input$off_qb_x_axis) +
        ylab(input$off_qb_y_axis) +
        geom_point(size = 6, color = "#0000b7", alpha = 0.5) +
        geom_text(aes(label = proj_player), hjust = 0, vjust = -1) +
        theme_bw() +
        theme(
          axis.title = element_text(size = 12, face = "bold")
        )
    })
    
    #
    # QB Defense vs Table Output - customizing column names
    # 
    
    output$def_qb <- renderDataTable({
      
    qb_def_container <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan = 2,''),
          th(class = 'dt-center', colspan = 4, 'Passing Def / G'),
          th(class = 'dt-center', colspan = 2, 'RZ Defense'),
          th(class = 'dt-center', colspan = 3, 'DVOA'),
          th(class = 'dt-center', colspan = 5, 'Defense Line Performance')
        ),
        tr(
          th(colspan = 1, 'Player'),
          th(colspan = 1, 'Opp'),
          th(colspan = 1, 'Att'),
          th(colspan = 1, 'Yds'),
          th(colspan = 1, 'TD'),
          th(colspan = 1, 'FD Pts'),
          th(colspan = 1, 'TD'),
          th(colspan = 1, 'Score %'),
          th(colspan = 1, 'Overall'),
          th(colspan = 1, 'Passing'),
          th(colspan = 1, 'Difference'),
          th(colspan = 1, 'Pass Rk'),
          th(colspan = 1, 'Adjust Sack Rate'),
          th(colspan = 1, 'Rating Allowed'),
          th(colspan = 1, 'Net Yds/Att'),
          th(colspan = 1, 'Oline Adj Sack Rate'))
      )
    ))
    
    render_def_qb <- subset(def_qb, 
                            pts_vs_passing_td >= input$td_g[1] & pts_vs_passing_td <= input$td_g[2] &
                            pts_vs_fantasy_per_game_fdpt >= input$fd_pts_gm[1] & pts_vs_fantasy_per_game_fdpt <= input$fd_pts_gm[2] &
                            DVOA_Diff >= input$qb_dvoa_diff[1] & DVOA_Diff <= input$qb_dvoa_diff[2] &
                            def_pass_adj_net_yds_per_att >= input$adj_net_yd_att[1] & def_pass_adj_net_yds_per_att <= input$adj_net_yd_att[2])
    
    datatable(render_def_qb, 
              rownames = F, 
              container = qb_def_container, 
              options = list(pageLength = 10, 
                             lengthMenu = c(10,20,30),
                             columnDefs = list(list(className = 'dt-center', targets = 'all'))))
    })
    
    # QB Graph Output
    qb_def_reactive <- reactive({
      
      def_qb_s1 <- input$def_qb_rows_selected
      
      qb_def_render_table <- subset(def_qb, 
                                    pts_vs_passing_td >= input$td_g[1] & pts_vs_passing_td <= input$td_g[2] &
                                    pts_vs_fantasy_per_game_fdpt >= input$fd_pts_gm[1] & pts_vs_fantasy_per_game_fdpt <= input$fd_pts_gm[2] &
                                    DVOA_Diff >= input$qb_dvoa_diff[1] & DVOA_Diff <= input$qb_dvoa_diff[2] &
                                    def_pass_adj_net_yds_per_att >= input$adj_net_yd_att[1] & def_pass_adj_net_yds_per_att <= input$adj_net_yd_att[2])
      
      if(length(input$qb_select) == 0) {qb_def_render_table <- qb_def_render_table[def_qb_s1,]}
      
      return(qb_def_render_table) #datatable(test, rownames = F)
    })
    
    # Output variable creation for QB
    output$qb_def_plot = renderPlot({
      
      qb_def_plot_data <- qb_def_reactive()
      ggplot(qb_def_plot_data, aes(x = qb_def_plot_data[[input$def_qb_x_axis]], qb_def_plot_data[[input$def_qb_y_axis]])) +
        xlab(input$def_qb_x_axis) +
        ylab(input$def_qb_y_axis) +
        geom_point(size = 6, color = "#0000b7", alpha = 0.5) +
        geom_text_repel(aes(label = proj_player), hjust = 0, vjust = -1) +
        theme_bw() +
        theme(
          axis.title = element_text(size = 12, face = "bold")
        )
    })
    
    
    
    # Combine for a big figure
    all_qb <- reactive({

      # Getting full dataframe
      all <- full_join(off_qb, def_qb, by = c("proj_player", "proj_opp"))

      off <- qb_def_reactive()
      def <- qb_off_reactive()
      all_qb_plot <- full_join(def, off, by = c("proj_player", "proj_opp")) %>%
                     select(proj_player, proj_opp) %>%
                     left_join(all, by = c("proj_player", "proj_opp"))
      
      return(all_qb_plot)

    })
    
    # Making into a plot
    output$qbplot <- renderPlot({
        
        qb_plot_data <- all_qb()
        ggplot(qb_plot_data, aes(x = qb_plot_data[[input$qb_x_axis]], qb_plot_data[[input$qb_y_axis]])) +
          xlab(input$qb_x_axis) +
          ylab(input$qb_y_axis) +
          geom_point(aes(size = qb_plot_data[[input$qb_size]]), color = "#0000b7", alpha = 0.5) +
          scale_size(name = input$qb_size) +
          geom_text_repel(aes(label = proj_player), hjust = 0, vjust = -1) +
          theme_bw() +
          theme(
            axis.title = element_text(size = 12, face = "bold")
          )
      })
        
# RB Tab Data -------------------------------------------------------------
    
    #
    # Defense RB
    #
    output$def_rb <- renderDataTable({
      
      # Defense RB Container
      def_rb_container <- htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(colspan = 2,''),
            th(class = 'dt-center', colspan = 7, 'Def Performance Against RB'),
            th(class = 'dt-center', colspan = 4, 'DVOA Metrics'),
            th(class = 'dt-center', colspan = 4, 'D Line Performance')
          ),
          tr(
            th(colspan = 1, 'Player'),
            th(colspan = 1, 'Opp'),
            th(colspan = 1, 'Total Touches'),
            th(colspan = 1, 'FD Pts Against'),
            th(colspan = 1, 'Rush Att'),
            th(colspan = 1, 'Rush Yds'),
            th(colspan = 1, 'Rush TD'),
            th(colspan = 1, 'Targets'),
            th(colspan = 1, 'Rec Yds'),
            th(colspan = 1, 'Defense'),
            th(colspan = 1, 'Rushing'),
            th(colspan = 1, 'Rushing Adv'),
            th(colspan = 1, 'Difference'),
            th(colspan = 1, 'Power Success'),
            th(colspan = 1, 'Difference vs Off'),
            th(colspan = 1, 'Adj Net Yards'),
            th(colspan = 1, 'Difference vs Off'))
        )
      ))
      
      render_def_rb <-  subset(rb_def,
                               def_rush_dvoa >= input$rush_dvoa[1] & def_rush_dvoa <= input$rush_dvoa[2] &
                               net_adj_line_yd_diff >= input$net_yds_diff[1] & net_adj_line_yd_diff <= input$net_yds_diff[2] &
                               DVOA_Advantage >= input$dvoa_advantage[1] & DVOA_Advantage <= input$dvoa_advantage[2] &
                               pts_vs_total_touch >= input$total_touches[1] & pts_vs_total_touch <= input$total_touches[2])
      
      datatable(render_def_rb, 
                rownames = F, 
                container = def_rb_container,
                options = list(pageLength = 10, 
                               lengthMenu = c(5,10,15,20),
                               columnDefs = list(list(className = 'dt-center', targets = 'all'))),
                caption = htmltools::tags$caption(
                               style = 'caption-side: bottom; text-align: left;',
                               'Legend: Total Touches = Rush Att + Target by RB vs. Defense ||| 
                                Rushing Advantage = Rush D DVOA + Rush Off DVOA, higher is better ||| 
                                Difference = Rush DVOA - Defense DVOA, lower means rushing d is a strength (i.e., compartively better than overall) |||
                                Power Success = % runs on 3rd/4th down OR 1st/2nd & goal from <= 2 yds which were successful |||
                                Difference = O Line success (rank) - D Line Success (rank) |||
                                Adj Net Yards = Adjusted Yds allowed by D line |||
                                Difference vs Off = Adj Net Yds from Offense - Defense, higher is better'))
    })
    
    #
    # Offense RB
    #
    
    output$off_rb <- renderDataTable({

      # Defense RB Container
      off_rb_container <- htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(colspan = 3,''),
            th(class = 'dt-center', colspan = 4, 'YTD Rush / Game'),
            th(class = 'dt-center', colspan = 2, 'YTD Rec / Game'),
            th(class = 'dt-center', colspan = 3, 'RZ Rec inside 10y'),
            th(class = 'dt-center', colspan = 3, 'RZ Rush inside 10y'),
            th(class = 'dt-center', colspan = 3, 'RZ Rush inside 5y'),
            th(class = 'dt-center', colspan = 3, 'DFS')
          ),
          tr(
            th(colspan = 1, 'Player'),
            th(colspan = 1, 'Opp'),
            th(colspan = 1, 'Total Touches'),
            th(colspan = 1, 'Att'),
            th(colspan = 1, 'TD'),
            th(colspan = 1, 'Yd/Att'),
            th(colspan = 1, 'Yds'),
            th(colspan = 1, 'Targets'),
            th(colspan = 1, 'Yds'),
            th(colspan = 1, 'Tgt'),
            th(colspan = 1, 'TD'),
            th(colspan = 1, 'Tgt %'),
            th(colspan = 1, 'Att'),
            th(colspan = 1, 'TD'),
            th(colspan = 1, 'Att %'),
            th(colspan = 1, 'Att'),
            th(colspan = 1, 'TD'),
            th(colspan = 1, 'Att %'),
            th(colspan = 1, 'Line'),
            th(colspan = 1, '$'),
            th(colspan = 1, 'Touches / $1k'))
        )
      ))

      render_off_rb <-  subset(rb_off,
                               total_touches >= input$tot_touches[1] & total_touches <= input$tot_touches[2] &
                               ytd_rush_att >= input$rush_att[1] & ytd_rush_att <= input$rush_att[2] &
                               rushing_ten_per_rush >= input$rush_10_per[1] & rushing_ten_per_rush <= input$rush_10_per[2] &
                               line >= input$rb_off_line[1] & line <= input$rb_off_line[2] &
                               fd_sal >= input$rb_salary[1] & fd_sal <= input$rb_salary[2 ]| is.na(total_touches) | is.na(rushing_ten_per_rush))

      datatable(render_off_rb,
                rownames = F,
                container = off_rb_container,
                options = list(pageLength = 10,
                               lengthMenu = c(10,20,30),
                               columnDefs = list(list(className = 'dt-center', targets = 'all'))))

    })
    
    
    ###### Graphing
    
    # RB Offense  Graph Output
    rb_reactive <- reactive({
      
      ## Offense
      rb_off_s1 <- input$off_rb_rows_selected
      
      reactive_off_rb <-  subset(rb_off,
                          total_touches >= input$tot_touches[1] & total_touches <= input$tot_touches[2] &
                          ytd_rush_att >= input$rush_att[1] & ytd_rush_att <= input$rush_att[2] &
                          rushing_ten_per_rush >= input$rush_10_per[1] & rushing_ten_per_rush <= input$rush_10_per[2] &
                          line >= input$rb_off_line[1] & line <= input$rb_off_line[2] | is.na(total_touches) | is.na(rushing_ten_per_rush))
      
      if(length(input$rb_select) == 0) {reactive_off_rb <- reactive_off_rb[rb_off_s1,]}
      
      ## Defense
      rb_def_s1 <- input$def_rb_rows_selected
      
      reactive_def_rb <-  subset(rb_def,
                               def_rush_dvoa >= input$rush_dvoa[1] & def_rush_dvoa <= input$rush_dvoa[2] &
                               net_adj_line_yd_diff >= input$net_yds_diff[1] & net_adj_line_yd_diff <= input$net_yds_diff[2] &
                               DVOA_Advantage >= input$dvoa_advantage[1] & DVOA_Advantage <= input$dvoa_advantage[2] &
                               pts_vs_total_touch >= input$total_touches[1] & pts_vs_total_touch <= input$total_touches[2])
      
      if(length(input$rb_select) == 0) {reactive_def_rb <- reactive_def_rb[rb_def_s1,]}
      
      ## Join Togeather
      all_rb <- full_join(rb_def, rb_off, by = c("proj_player", "proj_opp"))
      
      all_rb_plot <- full_join(reactive_def_rb, reactive_off_rb, by = c("proj_player", "proj_opp")) %>%
                     select(proj_player, proj_opp) %>%
                     left_join(all_rb, by = c("proj_player", "proj_opp"))
      
      return(all_rb_plot)
    })
    
   # Making into a plot
   output$rbplot <- renderPlot({
      
      rb_plot_data <- rb_reactive()
      
      ggplot(rb_plot_data, aes(x = rb_plot_data[[input$rb_x_axis]], rb_plot_data[[input$rb_y_axis]])) +
        xlab(input$rb_x_axis) +
        ylab(input$rb_y_axis) +
        geom_point(aes(size = rb_plot_data[[input$rb_size]]), color = "#0000b7", alpha = 0.5) +
        scale_size(name = input$rb_size) +
        geom_text_repel(aes(label = proj_player), hjust = 0, vjust = -1) +
        theme_bw() +
        theme(
          axis.title = element_text(size = 12, face = "bold")
        )
    })  
      

# WR Tab Data -------------------------------------------------------------
    
    
    # Def WR Table
    output$def_wr <- renderDataTable({
      
      # Defense WR Container
      def_wr_container <- htmltools::withTags(table(
          class = 'display',
          thead(
            tr(
              th(colspan = 2,''),
              th(class = 'dt-center', colspan = 4, 'Points Against to WR'),
              th(class = 'dt-center', colspan = 2, 'RZ Defense'),
              th(class = 'dt-center', colspan = 3, 'Defense Efficiency'),
              th(class = 'dt-center', colspan = 5, 'DVOA'),
              th(class = 'dt-center', colspan = 4, 'D Line Performance')
            ),
            tr(
              th(colspan = 1, 'Player'),
              th(colspan = 1, 'Opp'),
              th(colspan = 1, 'FD Pts'),
              th(colspan = 1, 'Tgts'),
              th(colspan = 1, 'Yds'),
              th(colspan = 1, 'TD'),
              th(colspan = 1, 'TD'),
              th(colspan = 1, 'Score %'),
              th(colspan = 1, 'Adj Net Yds/Att'),
              th(colspan = 1, 'Pass Yds/G'),
              th(colspan = 1, 'Total Yds/Play'),
              th(colspan = 1, 'Def'),
              th(colspan = 1, 'Pass'),
              th(colspan = 1, 'Pass Adv'),
              th(colspan = 1, 'Difference'),
              th(colspan = 1, 'Pass Rk'),
              th(colspan = 1, 'Sacks'),
              th(colspan = 1, 'Adj Sack Rate'),
              th(colspan = 1, 'Sack Rate Diff'))
          )))
      
      wr_def_render <- subset(wr_def, 
                              pts_vs_fantasy_per_game_fdpt >= input$fd_pts_gm_wr[1] & pts_vs_fantasy_per_game_fdpt <= input$fd_pts_gm_wr[2] &
                              pts_vs_rec_tgt >= input$wr_tg_vs[1] & pts_vs_rec_tgt <= input$wr_tg_vs[2] &
                              def_pass_yds_per_gm >= input$pass_yds_gm[1] & def_pass_yds_per_gm <= input$pass_yds_gm[2] &
                              DVOA_Advantage >= input$wr_dvoa_advantage[1] & DVOA_Advantage <= input$wr_dvoa_advantage[2])
      
      datatable(wr_def_render, 
                rownames = F,
                container = def_wr_container,
                options = list(pageLength = 15, lengthMenu = c(10,15,20)),
                caption = htmltools::tags$caption(
                          style = 'caption-side: bottom; text-align: left;',
                          'Legend: Pass Adv = Pass D DVOA + Pass Off DVOA, higher is better ||| 
                                Difference = Pass DVOA - Defense DVOA, lower means passing d is a strength (i.e., compartively better than overall) |||
                                Adj Sack Rate = sacks (plus intentional grounding penalties) per pass attempt adjusted for down, distance, and opponent |||
                                Sack Rate Difference = round(oline_pass_adjusted_sack_rate - dline_pass_adjusted_sack_rate,1))'))
    
      })
    
    # Offense WR
    output$off_wr <- renderDataTable({

      # Offense WR Container
      off_wr_container <- htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(colspan = 2,''),
            th(class = 'dt-center', colspan = 4, 'YTD Stats'),
            th(class = 'dt-center', colspan = 3, 'RZ Within 20y'),
            th(class = 'dt-center', colspan = 3, 'RZ Within 10y'),
            th(class = 'dt-center', colspan = 1, 'DVOA'),
            th(class = 'dt-center', colspan = 4, 'CB Matchup'),
            th(class = 'dt-center', colspan = 3, 'DFS Info')),
          tr(
            th(colspan = 1, 'Player'),
            th(colspan = 1, 'Opp'),
            th(colspan = 1, 'Target'),
            th(colspan = 1, 'Yds/Tgt'),
            th(colspan = 1, 'Yds/Gm'), ### ADD TD!
            th(colspan = 1, 'TD'),
            th(colspan = 1, 'Tgt'),
            th(colspan = 1, 'TD'),
            th(colspan = 1, '%Tgt'),
            th(colspan = 1, 'Tgt'),
            th(colspan = 1, 'TD'),
            th(colspan = 1, '%Tgt'),
            th(colspan = 1, 'Offense'),
            th(colspan = 1, 'Target (%)'),
            th(colspan = 1, 'Pt / Tgt'),
            th(colspan = 1, 'Shadow'),
            th(colspan = 1, 'Matchup'),
            th(colspan = 1, 'Salary ($)'),
            th(colspan = 1, 'Implied Total'),
            th(colspan = 1, 'Target / $1k'))
        )))
      
      wr_off_render <- subset(wr_off, 
                              (ytd_rec_target >= input$wr_tgt[1] & ytd_rec_target <= input$wr_tgt[2] | is.na(ytd_rec_target)) &
                              (receiving_twenty_per_tgt >= input$wr_rz_20[1] & receiving_twenty_per_tgt <= input$wr_rz_20[2] | is.na(receiving_twenty_per_tgt)) &
                              (vs_cb_fpt >= input$cb_pts_tgt[1] & vs_cb_fpt <= input$cb_pts_tgt[2] | is.na(vs_cb_fpt)) &
                              fd_sal >= input$wr_salary[1] & fd_sal <= input$wr_salary[2])
      # Team Selection
      if(input$wr_opponent != 'all') {wr_off_render <- subset(wr_off_render, proj_opp == input$wr_opponent)}
                       
      
      # for +/-/neutral CB pairings
      if(length(input$cb_matchup) == 1) {wr_off_render <- filter(wr_off_render, vs_cb_matchup == input$cb_matchup)
      
      } else if(length(input$cb_matchup) == 2)  {wr_off_render <- filter(wr_off_render, vs_cb_matchup == input$cb_matchup[1] | vs_cb_matchup == input$cb_matchup[2])
        } else {wr_off_render <- filter(wr_off_render, vs_cb_matchup == input$cb_matchup[1] | vs_cb_matchup == input$cb_matchup[2] | vs_cb_matchup == input$cb_matchup[3])}
    
      datatable(wr_off_render,
                rownames = F,
                container = off_wr_container,
                options = list(pageLength = 15, lengthMenu = c(10,15,20)))
    })
    
    
    # WR Table
    output$wrtable <- renderDataTable({
      
      render_wr <-  subset(wr,
                           fd_sal >= input$wr_salary[1] & fd_sal <= input$wr_salary[2]) #&
                           # defense_dvoa >= input$wr_dvoa[1] & defense_dvoa <= input$wr_dvoa[2] &
                           # pass_def_dvoa >= input$wr_pass_dvoa[1] & pass_def_dvoa <= input$wr_pass_dvoa[2] &
                           # line >= input$wr_line[1] & line <= input$wr_line[2])
      
      DT::datatable(render_wr[, input$wr_vars], rownames = F, options = list(pageLength = 15, lengthMenu = c(10,15,20)))
      
    })
    
    # WR Graph Output
    wr_dat <- reactive({
      
      wr_s1 <- input$wrtable_rows_selected
      
      wr_render_table <- subset(wr,
                                fd_sal >= input$wr_salary[1] & fd_sal <= input$wr_salary[2] &
                                defense_dvoa >= input$wr_dvoa[1] & defense_dvoa <= input$wr_dvoa[2] &
                                pass_def_dvoa >= input$wr_pass_dvoa[1] & pass_def_dvoa <= input$wr_pass_dvoa[2] &
                                line >= input$wr_line[1] & line <= input$wr_line[2]) %>% 
                        .[wr_s1,]
      
      return(wr_render_table)
    })
    
    # WR Offense  Graph Output
    wr_reactive <- reactive({
      
      ## Offense
      wr_off_s1 <- input$off_wr_rows_selected
      
      reactive_off_wr <-  subset(wr_off, 
                                 ytd_rec_target >= input$wr_tgt[1] & ytd_rec_target <= input$wr_tgt[2] &
                                 receiving_twenty_per_tgt >= input$wr_rz_20[1] & receiving_twenty_per_tgt <= input$wr_rz_20[2] &
                                 vs_cb_fpt >= input$cb_pts_tgt[1] & vs_cb_fpt <= input$cb_pts_tgt[2])
      
      if(length(input$wr_select) == 0) {reactive_off_wr <- reactive_off_wr[wr_off_s1,]}
      
      ## Defense
      wr_def_s1 <- input$def_wr_rows_selected
      
      reactive_def_wr <-  subset(wr_def, 
                                 pts_vs_fantasy_per_game_fdpt >= input$fd_pts_gm_wr[1] & pts_vs_fantasy_per_game_fdpt <= input$fd_pts_gm_wr[2] &
                                 pts_vs_rec_tgt >= input$wr_tg_vs[1] & pts_vs_rec_tgt <= input$wr_tg_vs[2] &
                                 def_pass_yds_per_gm >= input$pass_yds_gm[1] & def_pass_yds_per_gm <= input$pass_yds_gm[2] &
                                 DVOA_Advantage >= input$wr_dvoa_advantage[1] & DVOA_Advantage <= input$wr_dvoa_advantage[2])
      
      if(length(input$wr_select) == 0) {reactive_def_wr <- reactive_def_wr[wr_def_s1,]}

      ## Join Togeather
      all_wr <- full_join(wr_def, wr_off, by = c("proj_player", "proj_opp"))
      
      all_wr_plot <- full_join(reactive_def_wr, reactive_off_wr, by = c("proj_player", "proj_opp")) %>%
                     select(proj_player, proj_opp) %>%
                     left_join(all_wr, by = c("proj_player", "proj_opp"))
      
      return(all_wr_plot)
    })
    
    # Making into a plot
    output$wrplot <- renderPlot({
      
      wr_plot_data <- wr_reactive()
      
      ggplot(wr_plot_data, aes(x = wr_plot_data[[input$wr_x_axis]], wr_plot_data[[input$wr_y_axis]])) +
        xlab(input$wr_x_axis) +
        ylab(input$wr_y_axis) +
        geom_point(aes(size = wr_plot_data[[input$wr_size]]), color = "#0000b7", alpha = 0.5) +
        scale_size(name = input$wr_size) +
        geom_text_repel(aes(label = proj_player), hjust = 0, vjust = -1) +
        theme_bw() +
        theme(
          axis.title = element_text(size = 12, face = "bold")
        )
    })
    
# TE Tab Data -------------------------------------------------------------

    # Def TE Table
    
    output$def_te <- renderDataTable({
      
      # Defense TE Container
      def_te_container <- htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(colspan = 2,''),
            th(class = 'dt-center', colspan = 4, 'Points Against to WR'),
            th(class = 'dt-center', colspan = 2, 'RZ Defense'),
            th(class = 'dt-center', colspan = 3, 'Defense Efficiency'),
            th(class = 'dt-center', colspan = 5, 'DVOA'),
            th(class = 'dt-center', colspan = 4, 'D Line Performance')
          ),
          tr(
            th(colspan = 1, 'Player'),
            th(colspan = 1, 'Opp'),
            th(colspan = 1, 'FD Pts'),
            th(colspan = 1, 'Targets'),
            th(colspan = 1, 'Yds'),
            th(colspan = 1, 'TD'),
            th(colspan = 1, 'TD'),
            th(colspan = 1, 'Score %'),
            th(colspan = 1, 'Adj Net Yds/Att'),
            th(colspan = 1, 'Pass Yds/G'),
            th(colspan = 1, 'Total Yds/Play'),
            th(colspan = 1, 'Defense'),
            th(colspan = 1, 'Passing'),
            th(colspan = 1, 'Pass Adv'),
            th(colspan = 1, 'Difference'),
            th(colspan = 1, 'Pass Rk'),
            th(colspan = 1, 'Sacks'),
            th(colspan = 1, 'Adj Sack Rate'),
            th(colspan = 1, 'Sack Rate Diff'))
        )))
      
      te_def_render <- subset(te_def, 
                              pts_vs_fantasy_per_game_fdpt >= input$fd_pts_gm_te[1] & pts_vs_fantasy_per_game_fdpt <= input$fd_pts_gm_te[2] &
                              pts_vs_rec_tgt >= input$te_tg_vs[1] & pts_vs_rec_tgt <= input$te_tg_vs[2] &
                              def_pass_yds_per_gm >= input$pass_yds_gm_te[1] & def_pass_yds_per_gm <= input$pass_yds_gm_te[2] &
                              DVOA_Advantage >= input$te_dvoa_advantage[1] & DVOA_Advantage <= input$te_dvoa_advantage[2])
      
      datatable(te_def_render, 
                rownames = F,
                container = def_te_container,
                options = list(pageLength = 15, lengthMenu = c(10,15,20)),
                caption = htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: left;',
                  'Legend: Pass Adv = Pass D DVOA + Pass Off DVOA, higher is better ||| 
                                Difference = Pass DVOA - Defense DVOA, lower means passing d is a strength (i.e., compartively better than overall) |||
                                Adj Sack Rate = sacks (plus intentional grounding penalties) per pass attempt adjusted for down, distance, and opponent'))
      
    })
    
    # Offense TE
    output$off_te <- renderDataTable({
      
      # Offense TE Container
      off_te_container <- htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(colspan = 2,''),
            th(class = 'dt-center', colspan = 4, 'YTD Stats'),
            th(class = 'dt-center', colspan = 3, 'RZ Within 20y'),
            th(class = 'dt-center', colspan = 3, 'RZ Within 10y'),
            th(class = 'dt-center', colspan = 1, 'DVOA'),
            th(class = 'dt-center', colspan = 1, 'FD Info'),
            th(class = 'dt-center', colspan = 1, 'Vegas'),
            th(class = 'dt-center', colspan = 2, 'Projections')
          ),
          tr(
            th(colspan = 1, 'Player'),
            th(colspan = 1, 'Opp'),
            th(colspan = 1, 'Target'),
            th(colspan = 1, 'Yds/Tgt'),
            th(colspan = 1, 'Yds/Gm'), ### ADD TD!
            th(colspan = 1, 'TD'),
            th(colspan = 1, 'Tgt'),
            th(colspan = 1, 'TD'),
            th(colspan = 1, '%Tgt'),
            th(colspan = 1, 'Tgt'),
            th(colspan = 1, 'TD'),
            th(colspan = 1, '%Tgt'),
            th(colspan = 1, 'Offense'),
            th(colspan = 1, 'Salary ($)'),
            th(colspan = 1, 'Line'),
            th(colspan = 1, 'Rec Yds'),
            th(colspan = 1, 'Rec TD'))
        )))
      
      # Editing Table to render
      te_off_render <- subset(te_off, 
                              ytd_rec_target >= input$te_tgt[1] & ytd_rec_target <= input$te_tgt[2] &
                              ytd_rec_td >= input$te_rec_td[1] & ytd_rec_td <= input$te_rec_td[2] &
                              receiving_twenty_per_tgt >= input$te_rz_20[1] & receiving_twenty_per_tgt <= input$te_rz_20[2] &
                              off_pass_dvoa >= input$off_pass_dvoa_te[1] & off_pass_dvoa <= input$off_pass_dvoa_te[2] &
                              fd_sal >= input$te_salary[1] & fd_sal <= input$te_salary[2])
      
      datatable(te_off_render,
                rownames = F,
                container = off_te_container,
                options = list(pageLength = 15, lengthMenu = c(10,15,20)))
    })
    
    
    # # TE Table
    # output$tetable <- renderDataTable({
    #   
    #   render_te <-  subset(te,
    #                        fd_sal >= input$te_salary[1] & fd_sal <= input$te_salary[2]) #&
    #   # defense_dvoa >= input$te_dvoa[1] & defense_dvoa <= input$te_dvoa[2] &
    #   # pass_def_dvoa >= input$te_pass_dvoa[1] & pass_def_dvoa <= input$te_pass_dvoa[2] &
    #   # line >= input$te_line[1] & line <= input$te_line[2])
    #   
    #   DT::datatable(render_te[, input$te_vars], rownames = F, options = list(pageLength = 15, lengthMenu = c(10,15,20)))
    #   
    # })
    # 
    # # TE Graph Output
    # te_dat <- reactive({
    #   
    #   te_s1 <- input$tetable_rows_selected
    #   
    #   te_render_table <- subset(te,
    #                             fd_sal >= input$te_salary[1] & fd_sal <= input$te_salary[2] &
    #                               defense_dvoa >= input$te_dvoa[1] & defense_dvoa <= input$te_dvoa[2] &
    #                               pass_def_dvoa >= input$te_pass_dvoa[1] & pass_def_dvoa <= input$te_pass_dvoa[2] &
    #                               line >= input$te_line[1] & line <= input$te_line[2]) %>% 
    #     .[te_s1,]
    #   
    #   return(te_render_table)
    # })
    # 
    # # Output variable creation for QB
    # output$te_plot = renderPlot({
    #   
    #   te_plot_data <- te_dat()
    #   ggplot(te_plot_data, aes(x = te_plot_data[[input$te_x_axis]], te_plot_data[[input$te_y_axis]])) +
    #     xlab(input$te_x_axis) +
    #     ylab(input$te_y_axis) +
    #     geom_point(size = 6, color = "#0000b7", alpha = 0.5) +
    #     geom_text(aes(label = player), hjust = 0, vjust = -1) +
    #     theme_bw() +
    #     theme(
    #       axis.title = element_text(size = 12, face = "bold")
    #     )
    # })
    
    
    
    # TE Offense  Graph Output
    te_reactive <- reactive({
      
      ## Offense
      te_off_s1 <- input$off_te_rows_selected
      
      reactive_off_te <-  subset(te_off, 
                                 ytd_rec_target >= input$te_tgt[1] & ytd_rec_target <= input$te_tgt[2] &
                                 ytd_rec_td >= input$te_rec_td[1] & ytd_rec_td <= input$te_rec_td[2] &
                                 receiving_twenty_per_tgt >= input$te_rz_20[1] & receiving_twenty_per_tgt <= input$te_rz_20[2] &
                                 off_pass_dvoa >= input$off_pass_dvoa_te[1] & off_pass_dvoa <= input$off_pass_dvoa_te[2])
      
      if(length(input$te_select) == 0) {reactive_off_te <- reactive_off_te[te_off_s1,]}
      
      ## Defense
      te_def_s1 <- input$def_te_rows_selected
      
      reactive_def_te <-  subset(te_def, 
                                 pts_vs_fantasy_per_game_fdpt >= input$fd_pts_gm_te[1] & pts_vs_fantasy_per_game_fdpt <= input$fd_pts_gm_te[2] &
                                 pts_vs_rec_tgt >= input$te_tg_vs[1] & pts_vs_rec_tgt <= input$te_tg_vs[2] &
                                 def_pass_yds_per_gm >= input$pass_yds_gm_te[1] & def_pass_yds_per_gm <= input$pass_yds_gm_te[2] &
                                 DVOA_Advantage >= input$te_dvoa_advantage[1] & DVOA_Advantage <= input$te_dvoa_advantage[2])
      
      if(length(input$te_select) == 0) {reactive_def_te <- reactive_def_te[te_def_s1,]}
      
      ## Join Togeather
      all_te <- full_join(te_def, te_off, by = c("proj_player", "proj_opp"))
      
      all_te_plot <- full_join(reactive_def_te, reactive_off_te, by = c("proj_player", "proj_opp")) %>%
                     select(proj_player, proj_opp) %>%
                     left_join(all_te, by = c("proj_player", "proj_opp"))
      
      
      return(all_te_plot)
    })
    
    # Making into a plot
    output$teplot <- renderPlot({
      
      te_plot_data <- te_reactive()
      
      ggplot(te_plot_data, aes(x = te_plot_data[[input$te_x_axis]], te_plot_data[[input$te_y_axis]])) +
        xlab(input$te_x_axis) +
        ylab(input$te_y_axis) +
        geom_point(aes(size = te_plot_data[[input$te_size]]), color = "#0000b7", alpha = 0.5) +
        scale_size(name = input$te_size) +
        geom_text_repel(aes(label = proj_player), hjust = 0, vjust = -1) +
        theme_bw() +
        theme(
          axis.title = element_text(size = 12, face = "bold")
        )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
