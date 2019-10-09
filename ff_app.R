#
# DFs Shiny
#

library(shiny)
library(tidyverse)
library(DT)

df <- readxl::read_xlsx("data/all_data_wk_5.xlsx")
#df <- readxl::read_xlsx("~/ff_shiny_app/ff_app/data/all_data_wk_5.xlsx")

#NOTE: 16 columns per table works relatively well

# DFS Specific Data -------------------------------------------------------
dfs_df <- select(df,
                 proj_player,
                 proj_pos,
                 proj_tm,
                 proj_field,
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
            mutate(points_per_1k = round(proj_ffpts / (fd_sal/1000),2),
                   proj_opp = ifelse(proj_field == 2, paste("@",proj_opp, sep = ""), proj_opp)) %>%
            select(-proj_field)

names(dfs_df) <- str_remove(names(dfs_df), "proj_")
                 
            

# QB Data -----------------------------------------------------------------

qb <- filter(df, proj_pos == "QB") %>%
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
           defense_dvoa,
           pass_def_dvoa,
           dline_pass_rank,
           dline_pass_adjusted_sack_rate,
           def_third_d_per,
           def_pass_comp_per,
           def_pass_qb_rating_allowed,
           def_pass_adj_net_yds_per_att,
           def_pass_yds_per_gm,
           oline_pass_adjusted_sack_rate,
           fd_sal,
           projected_own,
           line) %>%
           mutate(DVOA_Diff = pass_def_dvoa - defense_dvoa)

qb_names <- names(qb) %>%
            str_replace("passing", "rz") %>%
            str_remove("\\.x") %>%
            str_remove("proj_") %>%
            str_replace("def_third_d","defense_3rd_conv") %>%
            str_remove("(?<=\\w)pass") %>%
            str_replace("_+","_") %>%
            str_replace("_per_","/") %>%
            str_replace("_per","%") %>%
            str_replace("twenty", "20") %>%
            str_replace("ten", "10") %>%
            str_replace("adjusted","adj_")
            
names(qb) <- qb_names


# QB Opponent Defense Stats
def_qb <- filter(df, proj_pos == "QB") %>%
         select(proj_player,
                proj_opp,
                pts_vs_g,
                pts_vs_passing_att,
                pts_vs_passing_yds,
                pts_vs_passing_td,
                pts_vs_fantasy_per_game_fdpt,
                def_red_zone_td,
                def_red_zone_pct,
                defense_dvoa,
                pass_def_dvoa,
                dline_pass_rank,
                dline_pass_adjusted_sack_rate,
                def_pass_qb_rating_allowed,
                def_pass_adj_net_yds_per_att,
                oline_pass_adjusted_sack_rate
                ) %>%
  mutate(DVOA_Diff = pass_def_dvoa - defense_dvoa,
         pts_vs_passing_att = round(as.numeric(pts_vs_passing_att) / as.numeric(pts_vs_g),2),
         pts_vs_passing_yds = round(as.numeric(pts_vs_passing_yds) / as.numeric(pts_vs_g),2),
         pts_vs_passing_td = round(as.numeric(pts_vs_passing_td) / as.numeric(pts_vs_g),2)) %>%
  select(-pts_vs_g) %>%
  select(proj_player, proj_opp, 
         pts_vs_passing_att, pts_vs_passing_yds, pts_vs_passing_td, pts_vs_fantasy_per_game_fdpt,
         def_red_zone_td, def_red_zone_pct,
         defense_dvoa, pass_def_dvoa, DVOA_Diff,
         dline_pass_rank, dline_pass_adjusted_sack_rate, def_pass_qb_rating_allowed, def_pass_adj_net_yds_per_att, oline_pass_adjusted_sack_rate
  )

off_qb <- filter(df, proj_pos == "QB") %>%
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
                 offense_dvoa,
                 pass_off_dvoa,
                 fd_sal,
                 line)
               

# RB Data -----------------------------------------------------------------

# rb <- filter(df, proj_pos == "RB") %>%
#       select(proj_player,
#              proj_opp,
#              ytd_rush_att,
#              ytd_rush_yds_per_att,
#              ytd_rush_yds_per_gm,
#              ytd_rush_td,
#              ytd_rec_target,
#              ytd_rec_rec,
#              ytd_rec_yds,
#              ytd_rec_rec_per_gm,
#              rushing_twenty_att,
#              rushing_twenty_yds,
#              rushing_twenty_td,
#              rushing_twenty_per_rush,
#              rushing_ten_att,
#              rushing_ten_yds,
#              rushing_ten_td,
#              rushing_ten_per_rush,
#              rushing_five_att,
#              rushing_five_yds,
#              rushing_five_td,
#              receiving_twenty_tgt,
#              receiving_twenty_yds,
#              receiving_twenty_td,
#              receiving_twenty_per_tgt,
#              receiving_ten_tgt,
#              receiving_ten_rec,
#              receiving_ten_yds,                        
#              receiving_ten_td,
#              receiving_ten_per_tgt,
#              defense_dvoa,
#              rush_def_dvoa,
#              dline_stuffed,
#              dline_rbyards,
#              dline_2nd_levelyards,
#              oline_adj_lineyards,
#              oline_powerrank,
#              fd_sal,
#              line,
#              pts_vs_rush_att,
#              pts_vs_rush_yds,
#              pts_vs_rush_td,   
#              pts_vs_rec_tgt,
#              pts_vs_rec_rec,
#              pts_vs_rec_yds,
#              pts_vs_rec_td,
#              pts_vs_fantasy_per_game_fdpt) %>%
#         filter(ytd_rush_att > 5) %>%
#         mutate(pts_vs_rush_att = as.numeric(pts_vs_rush_att),
#                pts_vs_rush_yds = as.numeric(pts_vs_rush_yds),
#                pts_vs_rush_td = as.numeric(pts_vs_rush_yds),   
#                pts_vs_rec_tgt = as.numeric(pts_vs_rec_tgt),
#                pts_vs_rec_rec = as.numeric(pts_vs_rec_rec),
#                pts_vs_rec_yds = as.numeric(pts_vs_rec_yds),
#                pts_vs_rec_td = as.numeric(pts_vs_rec_tgt),
#                pts_vs_fantasy_per_game_fdpt = as.numeric(pts_vs_fantasy_per_game_fdpt))
# 
# rb_names <- names(rb) %>%
#             str_remove("proj_") %>%
#             str_replace("rushing","rz_rush") %>%
#             str_replace("receiving", "rz_rec")
# 
# names(rb) <- rb_names


#
# RB Defense and Oline Table
#

# The following stats are not adjusted for opponent:
#   
#   RB Yards: Yards per carry by running backs against this defense, according to standard NFL numbers.
#   Power Success: Percentage of runs on third or fourth down, two yards or less to go, that achieved a first down or touchdown. Also includes runs on first-and-goal or second-and-goal from the two-yard line or closer. This is the only statistic on this page that includes quarterbacks. Teams are ranked from lowest power success percentage allowed (#1) to highest power success percentage allowed (#32).
#   Stuffed: Percentage of runs where the running back is tackled at or behind the line of scrimmage.Ranked from most stuffs (#1) to fewest stuffs (#32).
#   Second Level Yards: Yards earned by opposing running backs against this team between 5-10 yards past the line of scrimmage, divided by total running back carries.
#   Open Field Yards: Yards earned by opposing running backs against this team more than 10 yards past the line of scrimmage, divided by total running back carries.

rb_def <- filter(df, proj_pos == "RB") %>%
          select(proj_player,
                 ytd_rush_att,
                 proj_opp,
                 pts_vs_g,
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
                 defense_dvoa,
                 rush_def_dvoa,
                 rush_off_dvoa,
                 dline_powersuccess,
                 dline_adj_lineyards,
                 dline_stuffed,
                 dline_2nd_levelyards,
                 dline_open_fieldyards,
                 oline_adj_lineyards,
                 oline_powersuccess,
                 oline_stuffed,
                 oline_open_fieldyards,
                 oline_2nd_levelyards
                 ) %>%
          mutate(pts_vs_g =  as.numeric(pts_vs_g),
                 pts_vs_rec_tgt = as.numeric(pts_vs_rec_tgt),
                 pts_vs_rec_yds = as.numeric(pts_vs_rec_yds),
                 pts_vs_rec_td = as.numeric(pts_vs_rec_td),
                 pts_vs_rush_att = as.numeric(pts_vs_rush_att),
                 pts_vs_rush_yds = as.numeric(pts_vs_rush_yds),                           
                 pts_vs_rush_td = as.numeric(pts_vs_rush_td),
                 dline_adj_lineyards = as.numeric(dline_adj_lineyards),
                 dline_2nd_levelyards = as.numeric(dline_2nd_levelyards),
                 dline_open_fieldyards = as.numeric(dline_open_fieldyards),
                 oline_adj_lineyards = as.numeric(oline_adj_lineyards),
                 oline_2nd_levelyards = as.numeric(oline_2nd_levelyards),
                 oline_open_fieldyards = as.numeric(oline_open_fieldyards),
                 pts_vs_rec_tgt = round(pts_vs_rec_tgt/pts_vs_g,1),
                 pts_vs_rec_yds = round(pts_vs_rec_yds/pts_vs_g,1),
                 pts_vs_rec_td = pts_vs_rec_td/pts_vs_g,
                 pts_vs_rush_att = round(pts_vs_rush_att/pts_vs_g,1),
                 pts_vs_rush_yds = pts_vs_rush_yds/pts_vs_g,    
                 pts_vs_rush_td = pts_vs_rush_td/pts_vs_g,                           
                 pts_vs_total_touch = round(pts_vs_rush_att + pts_vs_rec_tgt,1),
                 DVOA_Advantage = rush_def_dvoa + rush_off_dvoa,
                 DVOA_Difference = rush_def_dvoa - defense_dvoa,
                 net_adj_line_yd_diff = round(oline_adj_lineyards - dline_adj_lineyards,1),
                 power_success_diff = oline_powersuccess - dline_powersuccess,
                 second_level_yds_diff = oline_2nd_levelyards - oline_2nd_levelyards,
                 open_fieldyards_diff = oline_open_fieldyards - dline_open_fieldyards) %>%
          filter(ytd_rush_att > 5) %>%
          select(proj_player,
                 proj_opp,
                 pts_vs_total_touch,
                 pts_vs_rush_att,
                 pts_vs_rush_yds,                           
                 pts_vs_rush_td,
                 pts_vs_rec_tgt,
                 pts_vs_rec_yds,
                 defense_dvoa,
                 rush_def_dvoa,
                 DVOA_Advantage,
                 DVOA_Difference,
                 dline_powersuccess,
                 power_success_diff,
                 dline_adj_lineyards,
                 net_adj_line_yd_diff)

#
# RB Offense Table
#

rb_off <- filter(df, proj_pos == "RB" & ytd_rush_att >5) %>%
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
                 line) %>%
          mutate(total_touches = ytd_rush_att + ytd_rec_target) %>%
          select(proj_player, proj_opp,
                 total_touches, ytd_rush_att:ytd_rush_yds_per_gm,
                 ytd_rec_target,ytd_rec_yds_per_gm,
                 receiving_ten_tgt:line)

# WR Data -----------------------------------------------------------------
wr <- filter(df, proj_pos == "WR") %>%
    select(proj_player,
           proj_opp,
           ytd_rec_target,
           ytd_rec_rec,
           ytd_rec_yds,
           ytd_rec_rec_per_gm,
           ytd_rec_yds_per_rec,
           ytd_rec_yds_per_target,
           ytd_rec_ctch_per,
           receiving_twenty_tgt,
           receiving_twenty_yds,
           receiving_twenty_td,
           receiving_twenty_per_tgt,
           receiving_ten_tgt,
           receiving_ten_rec,
           receiving_ten_yds,                        
           receiving_ten_td,
           receiving_ten_per_tgt,
           defense_dvoa,
           pass_def_dvoa,
           dline_pass_rank,
           oline_pass_adjusted_sack_rate,
           dline_2nd_levelyards,
           oline_adj_lineyards,
           oline_powerrank,
           fd_sal,
           projected_own,
           line,
           pts_vs_rec_tgt,
           pts_vs_rec_rec,
           pts_vs_rec_yds,
           pts_vs_rec_td,
           pts_vs_fantasy_per_game_fdpt) %>%
    filter(ytd_rec_target > 3) %>%
    mutate(pts_vs_rec_tgt = as.numeric(pts_vs_rec_tgt),
           pts_vs_rec_rec = as.numeric(pts_vs_rec_rec),
           pts_vs_rec_yds = as.numeric(pts_vs_rec_yds),
           pts_vs_rec_td = as.numeric(pts_vs_rec_tgt),
           pts_vs_fantasy_per_game_fdpt = as.numeric(pts_vs_fantasy_per_game_fdpt))


wr_names <- names(wr) %>%
  str_remove("proj_") %>%
  str_replace("rushing","rz_rush") %>%
  str_replace("receiving", "rz_rec")

names(wr) <- wr_names


# TE Data -----------------------------------------------------------------
te <- filter(df, proj_pos == "TE") %>%
  select(proj_player,
         proj_opp,
         ytd_rec_target,
         ytd_rec_rec,
         ytd_rec_yds,
         ytd_rec_rec_per_gm,
         ytd_rec_yds_per_rec,
         ytd_rec_yds_per_target,
         ytd_rec_ctch_per,
         receiving_twenty_tgt,
         receiving_twenty_yds,
         receiving_twenty_td,
         receiving_twenty_per_tgt,
         receiving_ten_tgt,
         receiving_ten_rec,
         receiving_ten_yds,                        
         receiving_ten_td,
         receiving_ten_per_tgt,
         defense_dvoa,
         pass_def_dvoa,
         dline_pass_rank,
         oline_pass_adjusted_sack_rate,
         dline_2nd_levelyards,
         oline_adj_lineyards,
         oline_powerrank,
         fd_sal,
         projected_own,
         line,
         pts_vs_rec_tgt,
         pts_vs_rec_rec,
         pts_vs_rec_yds,
         pts_vs_rec_td,
         pts_vs_fantasy_per_game_fdpt) %>%
  filter(ytd_rec_target > 2) %>%
  mutate(pts_vs_rec_tgt = as.numeric(pts_vs_rec_tgt),
         pts_vs_rec_rec = as.numeric(pts_vs_rec_rec),
         pts_vs_rec_yds = as.numeric(pts_vs_rec_yds),
         pts_vs_rec_td = as.numeric(pts_vs_rec_tgt),
         pts_vs_fantasy_per_game_fdpt = as.numeric(pts_vs_fantasy_per_game_fdpt))


te_names <- names(te) %>%
  str_remove("proj_") %>%
  str_replace("rushing","rz_rush") %>%
  str_replace("receiving", "rz_rec")

names(te) <- te_names

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
                               choices = list("fd_sal",
                                              "projected_own",
                                              "cash_odds",
                                              "gpp_odds",
                                              "fd_lev",
                                              "proj_ffpts",
                                              "points_per_1k",
                                              "line",
                                              "total",
                                              "implied_total"),
                               selected = "fd_sal")),
            column(2,
                   selectInput("x_axis",
                               h3("X Axis"),
                               choices = list("fd_sal",
                                              "projected_own",
                                              "cash_odds",
                                              "gpp_odds",
                                              "fd_lev",
                                              "proj_ffpts",
                                              "points_per_1k",
                                              "line",
                                              "total",
                                              "implied_total"),
                               selected = "gpp_odds")),
            column(6,
                   plotOutput('plot', height = 500))
        ),
    
        fluidRow(column(12,textOutput("player_pool")))
        ),



# QB Panel ----------------------------------------------------------------

    tabPanel("QB",
             
             fluidRow(
               column(3,
                      sliderInput("qb_salary",
                                  "Minimum FanDuel Salary:",
                                  min = min(off_qb$fd_sal, na.rm = T),
                                  max = max(off_qb$fd_sal, na.rm = T),
                                  value = c(min,max)
                      )),
               # column(3,
               #        sliderInput("qb_dvoa",
               #                    "Total DVOA",
               #                    min = min(qb$defense_dvoa, na.rm = T),
               #                    max = max(qb$defense_dvoa, na.rm = T),
               #                    value = c(min,max)
               #        )),
               # column(3,
               #        sliderInput("qb_pass_dvoa",
               #                    "Pass D DVOA",
               #                    min = min(qb$pass_def_dvoa,  na.rm = T),
               #                    max = max(qb$pass_def_dvoa,  na.rm = T),
               #                    value = c(min,max)
               #        )),
               column(3,
                      sliderInput("qb_line",
                                  "Line",
                                  min = min(off_qb[["line"]], na.rm = T),
                                  max = max(off_qb[["line"]], na.rm = T),
                                  value = c(min,max)
                      ))),
             
             # fluidRow(column(2,
             #     
             #     checkboxGroupInput("qb_vars", "QB columns to show:",
             #                        names(qb), selected = c("player",
             #                                                "opp",
             #                                                "ytd_td",
             #                                                "ytd_yds/gm",
             #                                                "ytd_net_yds/att",
             #                                                "rz_20_att",
             #                                                "rz_10_att",
             #                                                "defense_dvoa",
             #                                                "pass_def_dvoa",
             #                                                "dline_rank",
             #                                                "def_qb_rating_allowed",
             #                                                "def_adj_net_yds/att",
             #                                                "fd_sal",
             #                                                "line"))
             # ),
             # 
             #     column(10,
             #            div(DT::dataTableOutput("qbtable"), style = "font-size:85%")
             #     )),
             fluidRow(column(12, 
                             DT::dataTableOutput("off_qb"))),
             fluidRow(column(12,
                             p("Data Dictionary: Rz = Red Zone, ytd = Year to Date, 
                             DVOA = Defense-adjusted Value Over Average where negative is better for total, but not pass,
                             Dline = Defensive Line Ratings,
                             def_ = Raw Defense Stats"))),
             fluidRow(
               column(2,
                      selectInput("qb_y_axis",
                                  h3("Y Axis"),
                                  choices = as.list(names(off_qb)),
                                  selected = "fd_sal")),
               column(2,
                      selectInput("qb_x_axis",
                                  h3("X Axis"),
                                  choices = as.list(as.list(names(off_qb))),
                                  selected = "ytd_pass_net_yds_per_att")),
               column(6,
                      plotOutput('qb_off_plot', height = 500))
             ),
             
             
             fluidRow(
               column(12,DT::dataTableOutput("def_qb"))
             ),
             
             fluidRow(
               column(2,
                      selectInput("def_qb_y_axis",
                                  h3("Y Axis"),
                                  choices = as.list(names(def_qb)),
                                  selected = "pts_vs_fantasy_per_game_fdpt")),
               column(2,
                      selectInput("def_qb_x_axis",
                                  h3("X Axis"),
                                  choices = as.list(as.list(names(def_qb))),
                                  selected = "pass_def_dvoa")),
               column(6,
                      plotOutput('qb_def_plot', height = 500)))

),


# RB Panel ----------------------------------------------------------------

tabPanel("RB",
         
         # fluidRow(
         #   column(3,
         #          sliderInput("rb_salary",
         #                      "Minimum FanDuel Salary:",
         #                      min = min(rb$fd_sal, na.rm = T),
         #                      max = max(rb$fd_sal, na.rm = T),
         #                      value = c(min,max)
         #          )),
         #   column(3,
         #          sliderInput("rb_dvoa",
         #                      "Total DVOA",
         #                      min = min(rb$defense_dvoa, na.rm = T),
         #                      max = max(rb$defense_dvoa, na.rm = T),
         #                      value = c(min,max)
         #          )),
         #   column(3,
         #          sliderInput("rb_rush_dvoa",
         #                      "Rush D DVOA",
         #                      min = min(rb$rush_def_dvoa,  na.rm = T),
         #                      max = max(rb$rush_def_dvoa,  na.rm = T),
         #                      value = c(min,max)
         #          )),
         #   column(3,
         #          sliderInput("rb_line",
         #                      "Line",
         #                      min = min(rb[["line"]], na.rm = T),
         #                      max = max(rb[["line"]], na.rm = T),
         #                      value = c(min,max)
         #          ))),
         
         # fluidRow(column(2,
         #                 
         #                 checkboxGroupInput("rb_vars", "RB columns to show:",
         #                                    names(rb), 
         #                                    selected = c("player",
         #                                                 "opp",
         #                                                 "ytd_rush_att",
         #                                                 "ytd_rush_yds_per_gm",
         #                                                 "ytd_rush_td",
         #                                                 "ytd_rec_target",
         #                                                 "ytd_rec_rec_per_gm",
         #                                                 "rz_rush_five_att",
         #                                                 "rz_rec_twenty_tgt",
         #                                                 "defense_dvoa",
         #                                                 "rush_def_dvoa",
         #                                                 "dline_stuffed",
         #                                                 "oline_powerrank",
         #                                                 "fd_sal",
         #                                                 "line"))
         # ),
         
         
         column(12,
                div(DT::dataTableOutput("def_rb"), style = "font-size:95%")
         ),
         
         column(12,
                div(DT::dataTableOutput("off_rb"), style = "font-size:95%")
         )#,
        
         #
         # RB Plot
         #
         # fluidRow(
         #   column(2,
         #          selectInput("rb_y_axis",
         #                      h3("Y Axis"),
         #                      choices = as.list(names(rb)),
         #                      selected = "fd_sal")),
         #   column(2,
         #          selectInput("rb_x_axis",
         #                      h3("X Axis"),
         #                      choices = as.list(as.list(names(rb))),
         #                      selected = "defense_dvoa")),
         #   column(6,
         #          plotOutput('rb_plot', height = 500)))
),

# WR Panel ----------------------------------------------------------------
tabPanel("WR",
         
         fluidRow(
           column(3,
                  sliderInput("wr_salary",
                              "Minimum FanDuel Salary:",
                              min = min(wr$fd_sal, na.rm = T),
                              max = max(wr$fd_sal, na.rm = T),
                              value = c(min,max)
                  )),
           column(3,
                  sliderInput("wr_dvoa",
                              "Total DVOA",
                              min = min(wr$defense_dvoa, na.rm = T),
                              max = max(wr$defense_dvoa, na.rm = T),
                              value = c(min,max)
                  )),
           column(3,
                  sliderInput("wr_pass_dvoa",
                              "Pass D DVOA",
                              min = min(wr$pass_def_dvoa,  na.rm = T),
                              max = max(wr$pass_def_dvoa,  na.rm = T),
                              value = c(min,max)
                  )),
           column(3,
                  sliderInput("wr_line",
                              "Line",
                              min = min(wr[["line"]], na.rm = T),
                              max = max(wr[["line"]], na.rm = T),
                              value = c(min,max)
                  ))),
         
         fluidRow(column(2,
                         
                         checkboxGroupInput("wr_vars", "WR columns to show:",
                                            names(wr), selected = c("player",
                                                                    "opp",
                                                                    "ytd_rec_target",
                                                                    "ytd_rec_rec",
                                                                    "ytd_rec_yds_per_target",
                                                                    "receiving_twenty_tgt",
                                                                    "receiving_twenty_td",
                                                                    "receiving_twenty_per_tgt",
                                                                    "receiving_ten_tgt",
                                                                    "receiving_ten_rec",
                                                                    "receiving_ten_td",
                                                                    "receiving_ten_per_tgt",
                                                                    "defense_dvoa",
                                                                    "pass_def_dvoa",
                                                                    "dline_pass_rank",
                                                                    "oline_pass_adjustedsack_rate",
                                                                    "fd_sal",
                                                                    "line"))
         ),
         
         
         column(10,
                div(DT::dataTableOutput("wrtable"), style = "font-size:75%")
         )),
         fluidRow(column(12,
                         p("Data Dictionary: Rz = Red Zone, ytd = Year to Date, 
                             DVOA = Defense-adjusted Value Over Average where negative is better,
                             Dline = Defensive Line Ratings,
                             def_ = Raw Defense Stats"))),
         
         #
         # WR Plot
         #
         fluidRow(
           column(2,
                  selectInput("wr_y_axis",
                              h3("Y Axis"),
                              choices = as.list(names(wr)),
                              selected = "fd_sal")),
           column(2,
                  selectInput("wr_x_axis",
                              h3("X Axis"),
                              choices = as.list(as.list(names(wr))),
                              selected = "defense_dvoa")),
           column(6,
                  plotOutput('wr_plot', height = 500)))
         
         
),

# TE Table ----------------------------------------------------------------
tabPanel("TE",
         
         fluidRow(
           column(3,
                  sliderInput("te_salary",
                              "Minimum FanDuel Salary:",
                              min = min(te$fd_sal, na.rm = T),
                              max = max(te$fd_sal, na.rm = T),
                              value = c(min,max)
                  )),
           column(3,
                  sliderInput("te_dvoa",
                              "Total DVOA",
                              min = min(te$defense_dvoa, na.rm = T),
                              max = max(te$defense_dvoa, na.rm = T),
                              value = c(min,max)
                  )),
           column(3,
                  sliderInput("te_pass_dvoa",
                              "Pass D DVOA",
                              min = min(te$pass_def_dvoa,  na.rm = T),
                              max = max(te$pass_def_dvoa,  na.rm = T),
                              value = c(min,max)
                  )),
           column(3,
                  sliderInput("te_line",
                              "Line",
                              min = min(te[["line"]], na.rm = T),
                              max = max(te[["line"]], na.rm = T),
                              value = c(min,max)
                  ))),
         
         fluidRow(column(2,
                         
                         checkboxGroupInput("te_vars", "te columns to show:",
                                            names(te), selected = c("player",
                                                                    "opp",
                                                                    "ytd_rec_target",
                                                                    "ytd_rec_rec",
                                                                    "ytd_rec_yds_per_target",
                                                                    "receiving_twenty_tgt",
                                                                    "receiving_twenty_td",
                                                                    "receiving_twenty_per_tgt",
                                                                    "receiving_ten_tgt",
                                                                    "receiving_ten_rec",
                                                                    "receiving_ten_td",
                                                                    "receiving_ten_per_tgt",
                                                                    "defense_dvoa",
                                                                    "pass_def_dvoa",
                                                                    "dline_pass_rank",
                                                                    "oline_pass_adjustedsack_rate",
                                                                    "fd_sal",
                                                                    "line"))
         ),
         
         
         column(10,
                div(DT::dataTableOutput("tetable"), style = "font-size:75%")
         )),
         fluidRow(column(12,
                         p("Data Dictionary: Rz = Red Zone, ytd = Year to Date, 
                             DVOA = Defense-adjusted Value Over Average where negative is better,
                             Dline = Defensive Line Ratings,
                             def_ = Raw Defense Stats"))),
         
         #
         # TE Plot
         #
         fluidRow(
           column(2,
                  selectInput("te_y_axis",
                              h3("Y Axis"),
                              choices = as.list(names(te)),
                              selected = "fd_sal")),
           column(2,
                  selectInput("te_x_axis",
                              h3("X Axis"),
                              choices = as.list(as.list(names(te))),
                              selected = "defense_dvoa")),
           column(6,
                  plotOutput('te_plot', height = 500)))
         
         
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
                               pos == input$pos) %>%
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
              geom_text(aes(label = player), hjust = 0, vjust = -1) +
              theme_bw() +
              theme(
                axis.title = element_text(size = 12, face = "bold")
              )
    })

# QB Tab Data -------------------------------------------------------------

    # output$qbtable <- renderDataTable({
    #   
    #   render_qb <-  subset(qb,
    #                        fd_sal >= input$qb_salary[1] & fd_sal <= input$qb_salary[2] &
    #                        defense_dvoa >= input$qb_dvoa[1] & defense_dvoa <= input$qb_dvoa[2] &
    #                        pass_def_dvoa >= input$qb_pass_dvoa[1] & pass_def_dvoa <= input$qb_pass_dvoa[2] &
    #                        line >= input$qb_line[1] & line <= input$qb_line[2])
    #   
    #   DT::datatable(render_qb[, input$qb_vars], rownames = F, options = list(pageLength = 15, lengthMenu = c(10,15,20)))
    #   
    # })
    
    #
    # Offense QB
    #
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
                           line >= input$qb_line[1] & line <= input$qb_line[2])
      
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
                                line >= input$qb_line[1] & line <= input$qb_line[2]) %>%
                         .[qb_s1,]

      return(qb_render_table) #datatable(test, rownames = F)
    })
    
    # Output variable creation for QB
    output$qb_off_plot = renderPlot({

      qb_plot_data <- qb_off_reactive()
      ggplot(qb_plot_data, aes(x = qb_plot_data[[input$qb_x_axis]], qb_plot_data[[input$qb_y_axis]])) +
        xlab(input$qb_x_axis) +
        ylab(input$qb_y_axis) +
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
    
    datatable(def_qb, 
              rownames = F, 
              container = qb_def_container, 
              options = list(pageLength = 10, 
                             lengthMenu = c(10,20,30),
                             columnDefs = list(list(className = 'dt-center', targets = 'all'))))
    })
    
    #
    # Defensive QB Stats
    # 
    
    # QB Graph Output
    qb_def_reactive <- reactive({
      
      def_qb_s1 <- input$def_qb_rows_selected
      
      qb_def_render_table <- def_qb %>% .[def_qb_s1,]
      
      return(qb_def_render_table) #datatable(test, rownames = F)
    })
    
    # Output variable creation for QB
    output$qb_def_plot = renderPlot({
      
      qb_def_plot_data <- qb_def_reactive()
      ggplot(qb_def_plot_data, aes(x = qb_def_plot_data[[input$def_qb_x_axis]], qb_def_plot_data[[input$def_qb_y_axis]])) +
        xlab(input$def_qb_x_axis) +
        ylab(input$def_qb_y_axis) +
        geom_point(size = 6, color = "#0000b7", alpha = 0.5) +
        geom_text(aes(label = proj_player), hjust = 0, vjust = -1) +
        theme_bw() +
        theme(
          axis.title = element_text(size = 12, face = "bold")
        )
    })
    
        
# RB Tab Data -------------------------------------------------------------
    
    # RB Table
    # output$rbtable <- renderDataTable({
    #   
    #   render_rb <-  subset(rb,
    #                        fd_sal >= input$rb_salary[1] & fd_sal <= input$rb_salary[2] &
    #                        defense_dvoa >= input$rb_dvoa[1] & defense_dvoa <= input$rb_dvoa[2] &
    #                        rush_def_dvoa >= input$rb_rush_dvoa[1] & rush_def_dvoa <= input$rb_rush_dvoa[2] &
    #                        line >= input$rb_line[1] & line <= input$rb_line[2])
    #   
    #   DT::datatable(render_rb[, input$rb_vars], rownames = F, options = list(pageLength = 15, lengthMenu = c(10,15,20)))
    #   
    # })
    # 
    # # RB Graph Output
    # rb_dat <- reactive({
    #   
    #   rb_s1 <- input$rbtable_rows_selected
    #   
    #   rb_render_table <- subset(rb,
    #                             fd_sal >= input$rb_salary[1] & fd_sal <= input$rb_salary[2] &
    #                             defense_dvoa >= input$rb_dvoa[1] & defense_dvoa <= input$rb_dvoa[2] &
    #                             rush_def_dvoa >= input$rb_rush_dvoa[1] & rush_def_dvoa <= input$rb_rush_dvoa[2] &
    #                             line >= input$rb_line[1] & line <= input$rb_line[2]) %>% 
    #                     .[rb_s1,]
    #   
    #   return(rb_render_table)
    # })
    
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
            th(class = 'dt-center', colspan = 6, 'Def Performance Against RB'),
            th(class = 'dt-center', colspan = 4, 'DVOA Metrics'),
            th(class = 'dt-center', colspan = 4, 'D Line Performance')
          ),
          tr(
            th(colspan = 1, 'Player'),
            th(colspan = 1, 'Opp'),
            th(colspan = 1, 'Total Touches'),
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
      
      # render_qb <-  subset(off_qb,
      #                      fd_sal >= input$qb_salary[1] & fd_sal <= input$qb_salary[2] &
      #                        line >= input$qb_line[1] & line <= input$qb_line[2])
      
      datatable(rb_def, 
                rownames = F, 
                container = def_rb_container,
                options = list(pageLength = 10, 
                               lengthMenu = c(10,20,30),
                               columnDefs = list(list(className = 'dt-center', targets = 'all'))),
                caption = htmltools::tags$caption(
                               style = 'caption-side: bottom; text-align: left;',
                               'Legend: Total Touches = Rush Att + Target by RB vs. Defense | 
                                Rushing Adv = Rush D DVOA + Rush Off DVOA, higher is better | 
                                Difference = Rush DVOA - Defense DVOA, lower means rushing d is a strength (i.e., compartively better than overall) |
                                Power Success = % runs on 3rd/4th down OR 1st/nd & goal from <= 2 yds which were successful |
                                Difference = O Line success (rank) - D Line Success (rank) |
                                Adj Net Yards = Adjusted Yds allowed by D line |
                                Difference vs Off = Adj Net Yds from Offense - Defense, higher is better'))
    })
    
    # Output variable creation for QB
    # output$rb_plot = renderPlot({
    #   
    #   rb_plot_data <- rb_dat()
    #   ggplot(rb_plot_data, aes(x = rb_plot_data[[input$rb_x_axis]], rb_plot_data[[input$rb_y_axis]])) +
    #     xlab(input$rb_x_axis) +
    #     ylab(input$rb_y_axis) +
    #     geom_point(size = 6, color = "#0000b7", alpha = 0.5) +
    #     geom_text(aes(label = player), hjust = 0, vjust = -1) +
    #     theme_bw() +
    #     theme(
    #       axis.title = element_text(size = 12, face = "bold")
    #     )
    # })
    
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
            th(class = 'dt-center', colspan = 1, '')
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
            th(colspan = 1, 'Line'))
        )
      ))

      # render_qb <-  subset(off_qb,
      #                      fd_sal >= input$qb_salary[1] & fd_sal <= input$qb_salary[2] &
      #                        line >= input$qb_line[1] & line <= input$qb_line[2])

      datatable(rb_off,
                rownames = F,
                container = off_rb_container,
                options = list(pageLength = 10,
                               lengthMenu = c(10,20,30),
                               columnDefs = list(list(className = 'dt-center', targets = 'all'))))

    })
      
      
      
      
      
      
      
      
    #})
    
    

# WR Tab Data -------------------------------------------------------------
    
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
    
    # Output variable creation for QB
    output$wr_plot = renderPlot({
      
      wr_plot_data <- wr_dat()
      ggplot(wr_plot_data, aes(x = wr_plot_data[[input$wr_x_axis]], wr_plot_data[[input$wr_y_axis]])) +
        xlab(input$wr_x_axis) +
        ylab(input$wr_y_axis) +
        geom_point(size = 6, color = "#0000b7", alpha = 0.5) +
        geom_text(aes(label = player), hjust = 0, vjust = -1) +
        theme_bw() +
        theme(
          axis.title = element_text(size = 12, face = "bold")
        )
    })
    
  
# TE Tab Data -------------------------------------------------------------

    # TE Table
    output$tetable <- renderDataTable({
      
      render_te <-  subset(te,
                           fd_sal >= input$te_salary[1] & fd_sal <= input$te_salary[2]) #&
      # defense_dvoa >= input$te_dvoa[1] & defense_dvoa <= input$te_dvoa[2] &
      # pass_def_dvoa >= input$te_pass_dvoa[1] & pass_def_dvoa <= input$te_pass_dvoa[2] &
      # line >= input$te_line[1] & line <= input$te_line[2])
      
      DT::datatable(render_te[, input$te_vars], rownames = F, options = list(pageLength = 15, lengthMenu = c(10,15,20)))
      
    })
    
    # TE Graph Output
    te_dat <- reactive({
      
      te_s1 <- input$tetable_rows_selected
      
      te_render_table <- subset(te,
                                fd_sal >= input$te_salary[1] & fd_sal <= input$te_salary[2] &
                                  defense_dvoa >= input$te_dvoa[1] & defense_dvoa <= input$te_dvoa[2] &
                                  pass_def_dvoa >= input$te_pass_dvoa[1] & pass_def_dvoa <= input$te_pass_dvoa[2] &
                                  line >= input$te_line[1] & line <= input$te_line[2]) %>% 
        .[te_s1,]
      
      return(te_render_table)
    })
    
    # Output variable creation for QB
    output$te_plot = renderPlot({
      
      te_plot_data <- te_dat()
      ggplot(te_plot_data, aes(x = te_plot_data[[input$te_x_axis]], te_plot_data[[input$te_y_axis]])) +
        xlab(input$te_x_axis) +
        ylab(input$te_y_axis) +
        geom_point(size = 6, color = "#0000b7", alpha = 0.5) +
        geom_text(aes(label = player), hjust = 0, vjust = -1) +
        theme_bw() +
        theme(
          axis.title = element_text(size = 12, face = "bold")
        )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
