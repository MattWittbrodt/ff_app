#
# DFs Shiny
#

library(shiny)
library(tidyverse)
library(DT)
library(ggrepel)
library(reactable)

source("./global_new.R")

#NOTE: 16 columns per table works relatively well

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
                                      choices = list("QB","RB","WR","TE"),
                                      selected = "QB"))),


        # Sidebar with a slider input for number of bins
        fluidRow(
            column(3,
            sliderInput("salary",
                     "Minimum FanDuel Salary:",
                     min = min(dfs_df$fd_sal),
                     max = max(dfs_df$fd_sal),
                     value = c(min(dfs_df$fd_sal), max(dfs_df$fd_sal)),
            )),
            column(3,
                   sliderInput("value",
                        "Points Per $1000",
                        min = min(dfs_df$pt_1k, na.rm = T),
                        max = max(dfs_df$pt_1k, na.rm = T),
                        value = c(min(dfs_df$pt_1k, na.rm = T), max(dfs_df$pt_1k, na.rm = T))
                   )),
            column(3,
                   sliderInput("lev",
                               "Leverage",
                               min = min(dfs_df$fd_lev, na.rm = T),
                               max = max(dfs_df$fd_lev, na.rm = T),
                               value = c(min(dfs_df$fd_lev, na.rm = T), max(dfs_df$fd_lev, na.rm = T))
                   )),
            column(3,
                   sliderInput("line",
                               "Line",
                               min = min(dfs_df$line, na.rm = T),
                               max = max(dfs_df$line, na.rm = T),
                               value = c(min(dfs_df$line, na.rm = T), max(dfs_df$line, na.rm = T))
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
                               selected = "points_per_1k")),
            column(2,
                   selectInput("x_axis",
                               h3("X Axis"),
                               choices = as.list(names(dfs_df)),
                               selected = "implied_total")),
            column(6,
                   plotOutput('plot', height = 500))
        ),

        fluidRow(column(12,textOutput("publish_date")))
        ),



# QB Panel ----------------------------------------------------------------

    tabPanel("QB",

             fluidRow(column(3,
                             checkboxGroupInput("qb_select",
                                                "Plot All Returned Players in Tables",
                                                choices = list("yes"),
                                                selected = c("yes")))),
             #
             # Sliders for Defense QB
             #

             fluidRow(
               column(3,
                      sliderInput("td_g",
                                  "TD Surrendered to QB / Game",
                                  min = min(def_qb$td, na.rm = T),
                                  max = max(def_qb$td, na.rm = T),
                                  value = c(min(def_qb$td, na.rm = T),max(def_qb$td, na.rm = T))
                      )),
               column(3,
                      sliderInput("fd_pts_gm",
                                  "FD Pts / Gm to QB",
                                  min = min(def_qb$fd_pts, na.rm = T),
                                  max = max(def_qb$fd_pts, na.rm = T),
                                  value = c(min(def_qb$fd_pts, na.rm = T),max(def_qb$fd_pts, na.rm = T))
                      )),
               column(3,
                      sliderInput("qb_dvoa_diff",
                                  "DVOA Difference",
                                  min = min(def_qb$difference, na.rm = T),
                                  max = max(def_qb$difference, na.rm = T),
                                  value = c(min(def_qb$difference, na.rm = T), max(def_qb$difference, na.rm = T))
                      )),
               column(3,
                      sliderInput("adj_net_yd_att",
                                  "Adj Net Yd / Att",
                                  min = min(def_qb$net_yds_per_att, na.rm = T),
                                  max = max(def_qb$net_yds_per_att, na.rm = T),
                                  value = c(min(def_qb$net_yds_per_att, na.rm = T),max(def_qb$net_yds_per_att, na.rm = T))
                      ))),


             fluidRow(
               column(12,DT::dataTableOutput("def_qb"))
             ),

             #
             # Sliders for Offense QB
             #

             fluidRow(
               column(3,
                      sliderInput("qb_salary",
                                  "Minimum FanDuel Salary:",
                                  min = min(off_qb$fd_sal, na.rm = T),
                                  max = max(off_qb$fd_sal, na.rm = T),
                                  value = c(min(off_qb$fd_sal, na.rm = T),max(off_qb$fd_sal, na.rm = T))
                      )),
               column(3,
                      sliderInput("qb_total",
                                  "Implied Total",
                                  min = min(off_qb$implied_total, na.rm = T),
                                  max = max(off_qb$implied_total, na.rm = T),
                                  value = c(min(off_qb$implied_total, na.rm = T),max(off_qb$implied_total, na.rm = T))
                      )),
               column(3,
                      sliderInput("qb_pass_yds_diff",
                                  "Passing EYds - Yds",
                                  min = min(off_qb$eyd_diff, na.rm = T),
                                  max = max(off_qb$eyd_diff, na.rm = T),
                                  value = c(min(off_qb$eyd_diff, na.rm = T),max(off_qb$eyd_diff, na.rm = T))
                      )),
               column(3,
                      sliderInput("qb_rush_yds",
                                  "Rushing Yds/Gm",
                                  min = 0,
                                  max = max(off_qb$rush_yds_gm,  na.rm = T),
                                  value = c(0,max(off_qb$rush_yds_gm,  na.rm = T))
                      ))
               ),


             column(12,
                       div(DT::dataTableOutput("off_qb")),
                       tags$div(HTML(qb_off_legend))),

             fluidRow(
               column(2,
                      selectInput("qb_y_axis",
                                  h3("Y Axis"),
                                  choices = as.list(c(names(def_qb), names(off_qb))),
                                  selected = "fd_pts")),
               column(2,
                      selectInput("qb_x_axis",
                                  h3("X Axis"),
                                  choices = as.list(c(names(def_qb), names(off_qb))),
                                  selected = "pass_dyar")),
               column(2,
                      selectInput("qb_size",
                                  h3("Size"),
                                  choices = as.list(c(names(def_qb), names(off_qb))),
                                  selected = "implied_total")),
               column(6,
                      plotOutput('qbplot', height = 500))
             )

),
# 
# # RB Panel ----------------------------------------------------------------
# 
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
                              min = min(rb_def$rush_dvoa, na.rm = T),
                              max = max(rb_def$rush_dvoa, na.rm = T),
                              value = c(min(rb_def$rush_dvoa, na.rm = T),max(rb_def$rush_dvoa, na.rm = T)),
                              step = 0.1, round = -1
                  )),
           column(3,
                  sliderInput("net_yds_diff",
                              "Net Yards Differece",
                              min = min(rb_def$net_adj_line_yd_diff_vs_off, na.rm = T),
                              max = max(rb_def$net_adj_line_yd_diff_vs_off, na.rm = T),
                              value = c(min(rb_def$net_adj_line_yd_diff_vs_off, na.rm = T),
                                        max(rb_def$net_adj_line_yd_diff_vs_off, na.rm = T))
                  )),
           column(3,
                  sliderInput("dvoa_advantage",
                              "Rushing Advantage",
                              min = min(rb_def$dvoa_advantage,  na.rm = T),
                              max = max(rb_def$dvoa_advantage,  na.rm = T),
                              value = c(min(rb_def$dvoa_advantage,  na.rm = T),
                                        max(rb_def$dvoa_advantage,  na.rm = T)),
                              step = 0.1, round = -1
                  )),
           column(3,
                  sliderInput("total_touches",
                              "Total Touches by RB",
                              min = min(rb_def$total_touches, na.rm = T),
                              max = max(rb_def$total_touches, na.rm = T),
                              value = c(min(rb_def$total_touches, na.rm = T),max(rb_def$total_touches, na.rm = T))
                  ))),


         column(12,
                div(DT::dataTableOutput("def_rb"), style = "font-size:95%"),
                tags$div(HTML(rb_def_legend))

         ),

         #
         # Offensive RB Stats
         #

         ## Sliders
         fluidRow(
           column(2,
                  sliderInput("tot_touches",
                              "Total Touches",
                              min = min(rb_off$total_touches, na.rm = T),
                              max = max(rb_off$total_touches, na.rm = T),
                              value = c(min(rb_off$total_touches, na.rm = T),max(rb_off$total_touches, na.rm = T))
                  )),
           column(2,
                  sliderInput("rush_att",
                              "Rushing Attempts / Gm",
                              min = min(rb_off$att, na.rm = T),
                              max = max(rb_off$att, na.rm = T),
                              value = c(min(rb_off$att, na.rm = T),max(rb_off$att, na.rm = T))
                  )),
           column(2,
                  sliderInput("rush_10_per",
                              "% Rushing Att Inside 10y",
                              min = min(rb_off$inside10_perrush,  na.rm = T),
                              max = max(rb_off$inside10_perrush,  na.rm = T),
                              value = c(min(rb_off$inside10_perrush,  na.rm = T),
                                        max(rb_off$inside10_perrush,  na.rm = T))
                  )),
           column(2,
                  sliderInput("rb_off_line",
                              "Line",
                              min = min(rb_off$line, na.rm = T),
                              max = max(rb_off$line, na.rm = T),
                              value = c(min(rb_off$line, na.rm = T),max(rb_off$line, na.rm = T))
                  )),
           column(2,
                  sliderInput("rb_salary",
                              "Salary",
                              min = min(rb_off$fd_sal,  na.rm = T),
                              max = max(rb_off$fd_sal,  na.rm = T),
                              value = c(min(rb_off$fd_sal,  na.rm = T),max(rb_off$fd_sal,  na.rm = T))))
           ),

         ## RB Offense Table
         column(12,
                div(DT::dataTableOutput("off_rb"), style = "font-size:93%"),
                tags$div(HTML(rb_off_legend))
         ),

         #
         # RB Plot
         #
         fluidRow(
           column(2,
                  selectInput("rb_y_axis",
                              h3("Y Axis"),
                              choices = as.list(c(names(rb_def), names(rb_off))),
                              selected = "total_touches")),
           column(2,
                  selectInput("rb_x_axis",
                              h3("X Axis"),
                              choices = as.list(c(names(rb_def), names(rb_off))),
                              selected = "high_value_touches")),
           column(2,
                  selectInput("rb_size",
                              h3("Size"),
                              choices = as.list(c(names(rb_def), names(rb_off))),
                              selected = "fd_sal")),
           column(6,
                  plotOutput('rbplot', height = 500)))
),
 
# # WR Panel ----------------------------------------------------------------
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
                              min = min(wr_def$fantasy_per_gm_fdpt, na.rm = T),
                              max = max(wr_def$fantasy_per_gm_fdpt, na.rm = T),
                              value = c(min(wr_def$fantasy_per_gm_fdpt, na.rm = T),max(wr_def$fantasy_per_gm_fdpt, na.rm = T))
                  )),
           column(3,
                  sliderInput("wr_tg_vs",
                              "Targets/Gm to WR",
                              min = min(wr_def$tgt, na.rm = T),
                              max = max(wr_def$tgt, na.rm = T),
                              value = c(min(wr_def$tgt, na.rm = T),max(wr_def$tgt, na.rm = T))
                  )),
           column(3,
                  sliderInput("pass_yds_gm",
                              "Pass Yds/Gm",
                              min = min(wr_def$y_per_g,  na.rm = T),
                              max = max(wr_def$y_per_g,  na.rm = T),
                              value = c(min(wr_def$y_per_g,  na.rm = T),max(wr_def$y_per_g,  na.rm = T))
                  )),
           column(3,
                  sliderInput("wr_dvoa_advantage",
                              "DVOA Advantage",
                              min = min(wr_def$dvoa_advantage, na.rm = T),
                              max = max(wr_def$dvoa_advantage, na.rm = T),
                              value = c(min(wr_def$dvoa_advantage, na.rm = T),max(wr_def$dvoa_advantage, na.rm = T))
                  ))),

         fluidRow(column(12,
                  div(DT::dataTableOutput("def_wr"), style = "font-size: 90%"),
                  tags$div(HTML(receiver_def_legend)))),

         #
         #  Offense WR Sliders and Table Presentation
         #

         fluidRow(
           column(2,
                  sliderInput("wr_tgt",
                              "YTD Targets Per Game",
                              min = min(wr_off$target, na.rm = T),
                              max = max(wr_off$target, na.rm = T),
                              value = c(min(wr_off$target, na.rm = T),max(wr_off$target, na.rm = T))
                  )),
           column(2,
                  sliderInput("wr_rz_20",
                              "% Targets Receiving Inside 20yd",
                              min = min(wr_off$inside20_pertgt, na.rm = T),
                              max = max(wr_off$inside20_pertgt, na.rm = T),
                              value = c(min(wr_off$inside20_pertgt, na.rm = T),max(wr_off$inside20_pertgt, na.rm = T))
                  )),
           column(2,
                  sliderInput("cb_pts_tgt",
                              "CB Matchup FD Pts/Tgt",
                              min = min(wr_off$vs_cb_fpt,  na.rm = T),
                              max = max(wr_off$vs_cb_fpt,  na.rm = T),
                              value = c(min(wr_off$vs_cb_fpt,  na.rm = T),max(wr_off$vs_cb_fpt,  na.rm = T))
                  )),
           column(2,
                  sliderInput("wr_salary",
                              "Salary",
                              min = min(wr_off$fd_sal,  na.rm = T),
                              max = max(wr_off$fd_sal,  na.rm = T),
                              value = c(min(wr_off$fd_sal,  na.rm = T),max(wr_off$fd_sal,  na.rm = T)))),
           column(2,
                  checkboxGroupInput("cb_matchup",
                              "CB Matchup Advantage",
                              choices = list("plus", "minus", "neutral"),
                              selected = c("plus", "minus", "neutral"))),
           column(2,
                  selectInput("wr_opponent",
                              "Opponent",
                              choices = as.list(c("all", unique(wr_off$opp))),
                              selected = "all"))),

         fluidRow(column(12,
                  div(DT::dataTableOutput("off_wr"), style = "font-size: 90%"),
                  tags$div(HTML(wr_off_legend)))),

         #
         # WR Plot
         #
         fluidRow(
           column(2,
                  selectInput("wr_y_axis",
                              h3("Y Axis"),
                              choices = as.list(c(names(wr_def), names(wr_off))),
                              selected = "yds")),
           column(2,
                  selectInput("wr_x_axis",
                              h3("X Axis"),
                              choices = as.list(c(names(wr_def), names(wr_off))),
                              selected = "air_yards")),
           column(2,
                  selectInput("wr_size",
                              h3("Size"),
                              choices = as.list(c(names(wr_def), names(wr_off))),
                              selected = "fd_sal")),
           column(6,
                  plotOutput('wrplot', height = 500)))


),
 
# # TE Panel ----------------------------------------------------------------
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
                              min = min(te_def$fantasy_per_gm_fdpt, na.rm = T),
                              max = max(te_def$fantasy_per_gm_fdpt, na.rm = T),
                              value = c(min(te_def$fantasy_per_gm_fdpt, na.rm = T),
                                        max(te_def$fantasy_per_gm_fdpt, na.rm = T))
                  )),
           column(3,
                  sliderInput("te_tg_vs",
                              "Targets/Gm to TE",
                              min = min(te_def$tgt, na.rm = T),
                              max = max(te_def$tgt, na.rm = T),
                              value = c(min(te_def$tgt, na.rm = T),max(te_def$tgt, na.rm = T))
                  )),
           column(3,
                  sliderInput("pass_yds_gm_te",
                              "Pass Yds/Gm",
                              min = min(te_def$yds,  na.rm = T),
                              max = max(te_def$yds,  na.rm = T),
                              value = c(min(te_def$yds,  na.rm = T),max(te_def$yds,  na.rm = T))
                  )),
           column(3,
                  sliderInput("te_dvoa_advantage",
                              "DVOA Advantage",
                              min = min(te_def$dvoa_advantage, na.rm = T),
                              max = max(te_def$dvoa_advantage, na.rm = T),
                              value = c(min(te_def$dvoa_advantage, na.rm = T),
                                        max(te_def$dvoa_advantage, na.rm = T))
                  ))),

         fluidRow(column(12,
                        div(DT::dataTableOutput("def_te"),
                            tags$div(HTML(receiver_def_legend))))),

         #
         #  Offense TE Sliders and Table Presentation
         #

         fluidRow(
           column(2,
                  sliderInput("te_tgt",
                              "YTD Targets Per Game",
                              min = min(te_off$target, na.rm = T),
                              max = max(te_off$target, na.rm = T),
                              value = c(min(te_off$target, na.rm = T),
                                        max(te_off$target, na.rm = T))
                  )),
           column(2,
                  sliderInput("te_rec_td",
                              "Receiving TD",
                              min = min(te_off$td,  na.rm = T),
                              max = max(te_off$td,  na.rm = T),
                              value = c(min(te_off$td,  na.rm = T),max(te_off$td,  na.rm = T))
                  )),
           column(2,
                  sliderInput("te_rz_20",
                              "% Targets Receiving Inside 20yd",
                              min = min(te_off$inside20_pertgt, na.rm = T),
                              max = max(te_off$inside20_pertgt, na.rm = T),
                              value = c(min(te_off$inside20_pertgt, na.rm = T),max(te_off$inside20_pertgt, na.rm = T))
                  )),
           column(2,
                  sliderInput("te_salary",
                              "Salary",
                              min = min(te_off$fd_sal,  na.rm = T),
                              max = max(te_off$fd_sal,  na.rm = T),
                              value = c(min(te_off$fd_sal,  na.rm = T),max(te_off$fd_sal,  na.rm = T)))),
           column(2,
                  sliderInput("off_pass_dvoa_te",
                              "Pass Off DVOA",
                              min = min(te_off$pass_dvoa,  na.rm = T),
                              max = max(te_off$pass_dvoa,  na.rm = T),
                              value = c(min(te_off$pass_dvoa,  na.rm = T),max(te_off$pass_dvoa,  na.rm = T)),
                              step = 0.1, round = -1
                  ))),

         fluidRow(column(12,
                         div(DT::dataTableOutput("off_te")),
                         tags$div(HTML(te_off_legend)))),

         #
         # TE Plot
         #
         fluidRow(
           column(2,
                  selectInput("te_y_axis",
                              h3("Y Axis"),
                              choices = as.list(c(names(te_def), names(te_off))),
                              selected = "fantasy_per_gm_fdpt")),
           column(2,
                  selectInput("te_x_axis",
                              h3("X Axis"),
                              choices = as.list(c(names(te_def), names(te_off))),
                              selected = "air_yards")),
           column(2,
                  selectInput("te_size",
                              h3("Size"),
                              choices = as.list(c(names(te_def), names(te_off))),
                              selected = "fd_sal")),
           column(6,
                  plotOutput('teplot', height = 500)))


)

)


# Server Function ---------------------------------------------------------
server <- function(input, output) {

# Main Panel Server -------------------------------------------------------
    # Publish Date
    output$publish_date <- renderText(paste0("App last updated on: ", day_time))
  
    # Main Table output
    output$fanduel <- renderDataTable({

        # Editing table for rendering
        render_table <- subset(dfs_df,
                               fd_sal >= input$salary[1] & fd_sal <= input$salary[2] &
                               pt_1k >= input$value[1] & pt_1k <= input$value[2] &
                               fd_lev >= input$lev[1] & fd_lev <= input$lev[2] &
                               line >= input$line[1] & line <= input$line[2] &
                               position == input$pos | is.na(fd_sal))

                # Selecting program
        render_table <- render_table[,c("full_name", "position", "team", "opp",
                                        "proj_ffpts","pt_1k","fd_lev",
                                        "afpa","afpa_rk",
                                        "fd_sal","price_change","projected_own", "cash_odds", "gpp_odds",
                                        "line","total","implied_total")]

        # Better Output - customizing column names
        dfs_container <- htmltools::withTags(table(
                                            class = 'display',
                                            thead(
                                              tr(
                                                th(colspan = 4,''),
                                                th(class = 'dt-center', colspan = 3, 'Point Projections'),
                                                th(class = 'dt-center', colspan = 2, 'aFPA'),
                                                th(class = 'dt-center', colspan = 5, 'DFS Leverage'),
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
                                                th(colspan = 1, 'Change ($)'),
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
                                 columnDefs = list(list(className = 'dt-center', targets = 'all')))) %>%
                  formatStyle(c('proj_ffpts','pt_1k','fd_lev',
                                'fd_sal','projected_own','cash_odds','gpp_odds'),
                              backgroundColor = '#F2F3F4')

       })


    # Graph Output
    dat <- reactive({

        render_table <- subset(dfs_df,
                               fd_sal >= input$salary[1] & fd_sal <= input$salary[2] &
                                   pt_1k >= input$value[1] & pt_1k <= input$value[2] &
                                   fd_lev >= input$lev[1] & fd_lev <= input$lev[2] &
                                   line >= input$line[1] & line <= input$line[2] &
                                   position == input$pos)

        return(render_table)
    })


    output$plot = renderPlot({

        plot_data <- dat()
        ggplot(plot_data, aes(x = plot_data[[input$x_axis]], plot_data[[input$y_axis]])) +
              xlab(input$x_axis) +
              ylab(input$y_axis) +
              geom_point(size = 6, color = "#0000b7", alpha = 0.5) +
              geom_text_repel(aes(label = full_name), hjust = 0, vjust = -1) +
              theme_bw() +
              theme(
                axis.title = element_text(size = 12, face = "bold")
              )
    })

# QB Tab Data -------------------------------------------------------------


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
          th(class = 'dt-center', colspan = 3, 'Defense Line Performance'),
          th(class = 'dt-center', colspan = 2, 'Other Stats')
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
          th(colspan = 1, 'QB Rating Allowed'),
          th(colspan = 1, 'Net Yds/Att'),
          th(colspan = 1, 'Oline Adj Sack Rate'))
      )
    ))

    render_def_qb <- subset(def_qb,
                            td >= input$td_g[1] & td <= input$td_g[2] &
                            fd_pts >= input$fd_pts_gm[1] & fd_pts <= input$fd_pts_gm[2] &
                            difference >= input$qb_dvoa_diff[1] & difference <= input$qb_dvoa_diff[2] &
                            net_yds_per_att >= input$adj_net_yd_att[1] & net_yds_per_att <= input$adj_net_yd_att[2])

    datatable(render_def_qb,
              rownames = F,
              container = qb_def_container,
              options = list(pageLength = 10,
                             lengthMenu = c(10,20,30),
                             columnDefs = list(list(className = 'dt-center', targets = 'all')))) %>%
              formatStyle(c('att','yds','td', 'fd_pts','total_dvoa','pass_dvoa','difference',
                            'net_yds_per_att','oline_adjsack_rate'), backgroundColor = '#F2F3F4')

    })


    # QB Graph Output
    qb_def_reactive <- reactive({

      def_qb_s1 <- input$def_qb_rows_selected

      qb_def_render_table <- subset(def_qb,
                                    td >= input$td_g[1] & td <= input$td_g[2] &
                                    fd_pts >= input$fd_pts_gm[1] & fd_pts <= input$fd_pts_gm[2] &
                                    difference >= input$qb_dvoa_diff[1] & difference <= input$qb_dvoa_diff[2] &
                                    net_yds_per_att >= input$adj_net_yd_att[1] & net_yds_per_att <= input$adj_net_yd_att[2])

      if(length(input$qb_select) == 0) {qb_def_render_table <- qb_def_render_table[def_qb_s1,]}

      return(qb_def_render_table)
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

    #
    # Offense QB ----
    #

    # Getting Subsetted Database Based on Sliders
    output$off_qb <- renderDataTable({

      # Offense QB Container
      off_qb_container <- htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(colspan = 2,''),
            th(class = 'dt-center', colspan = 2, 'Per Game Stats'),
            th(class = 'dt-center', colspan = 6, 'Advance Passing'),
            th(class = 'dt-center', colspan = 3, 'QB Rushing'),
            th(class = 'dt-center', colspan = 2, 'RZ Stats'),
            th(class = 'dt-center', colspan = 2, 'DFS Info')
          ),
          tr(
            th(colspan = 1, 'Player'),
            th(colspan = 1, 'Opp'),
            th(colspan = 1, 'Yards'),
            th(colspan = 1, 'TD'),
            th(colspan = 1, 'IAY'),
            th(colspan = 1, 'DYAR'),
            th(colspan = 1, 'On Target %'),
            th(colspan = 1, 'Eyds - Yds'),
            th(colspan = 1, 'Bad Throw %'),
            th(colspan = 1, 'Pressure %'),
            th(colspan = 1, 'DYAR'),
            th(colspan = 1, 'Yds/Gm'),
            th(colspan = 1, 'Eyds - Yds'),
            th(colspan = 1, 'Attempts'),
            th(colspan = 1, 'TD'),
            th(colspan = 1, 'Salary ($)'),
            th(class = 'dt-center', colspan = 1, 'Implied Total'))
        )
      ))

      render_qb <-  subset(off_qb,
                           fd_sal >= input$qb_salary[1] & fd_sal <= input$qb_salary[2] &
                           implied_total >= input$qb_total[1] & implied_total <= input$qb_total[2] &
                           eyd_diff >= input$qb_pass_yds_diff[1] & eyd_diff <= input$qb_pass_yds_diff[2] &
                           is.na(rush_yds_gm) | rush_yds_gm >= input$qb_rush_yds[1] & rush_yds_gm <= input$qb_rush_yds[2])

      datatable(render_qb,
                rownames = F,
                container = off_qb_container,
                options = list(pageLength = 10,
                               lengthMenu = c(10,20,30),
                               columnDefs = list(list(className = 'dt-center', targets = 'all')))) %>%
        formatStyle(c('yds_per_gm','td', 'rush_dyar','rush_yds_gm','rush_eyd_diff',
                      'fd_sal', 'implied_total'), backgroundColor = '#F2F3F4')

    })

    # QB Graph Output
    qb_off_reactive <- reactive({

      qb_s1 <- input$off_qb_rows_selected

      qb_render_table <- subset(off_qb,
                                fd_sal >= input$qb_salary[1] & fd_sal <= input$qb_salary[2] &
                                implied_total >= input$qb_total[1] & implied_total <= input$qb_total[2] &
                                eyd_diff >= input$qb_pass_yds_diff[1] & eyd_diff <= input$qb_pass_yds_diff[2] &
                                rush_yds_gm >= input$qb_rush_yds[1] & rush_yds_gm <= input$qb_rush_yds[2] | is.na(fd_sal))

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

    # Combine for a big figure
    all_qb <- reactive({

      # Getting full dataframe
      all <- full_join(off_qb, def_qb, by = c("full_name", "opp"))

      off <- qb_def_reactive()
      def <- qb_off_reactive()
      all_qb_plot <- full_join(def, off, by = c("full_name", "opp")) %>%
                     select(full_name, opp) %>%
                     left_join(all, by = c("full_name", "opp"))

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
          geom_text_repel(aes(label = full_name), hjust = 0, vjust = -1) +
          theme_bw() +
          theme(
            axis.title = element_text(size = 12, face = "bold")
          )
      })
 
# # RB Tab Data -------------------------------------------------------------

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
                               rush_dvoa >= input$rush_dvoa[1] & rush_dvoa <= input$rush_dvoa[2] &
                               net_adj_line_yd_diff_vs_off >= input$net_yds_diff[1] & net_adj_line_yd_diff_vs_off <= input$net_yds_diff[2] &
                               dvoa_advantage >= input$dvoa_advantage[1] & dvoa_advantage <= input$dvoa_advantage[2] &
                               total_touches >= input$total_touches[1] & total_touches <= input$total_touches[2])

      datatable(render_def_rb,
                rownames = F,
                container = def_rb_container,
                options = list(pageLength = 10,
                               lengthMenu = c(5,10,15,20),
                               columnDefs = list(list(className = 'dt-center', targets = 'all')))) %>%
                formatStyle(c('total_touches','fantasy_per_gm_fdpt','rush_att',
                              'rush_yds','rush_tds','targets','receiving_yards',
                              'run_powersuccess','power_success_diff','dline_net_adj_line_yards',
                              'net_adj_line_yd_diff_vs_off'), backgroundColor = '#F2F3F4') %>%
                formatStyle(
                  'power_success_diff',
                  fontWeight = styleInterval(0, c('normal','bold')),
                  color = styleInterval(c(-30, -10, 0, 30),
                                        c('#a50026', '#f16d43', 'black', '#64bc61', '#23964f'))) %>%
                formatStyle(
                  'dvoa_advantage',
                  fontWeight = styleInterval(0, c('normal','bold')),
                  color = styleInterval(c(-30, -10, 0, 30),
                                        c('#a50026', '#f16d43', 'black', '#64bc61', '#23964f'))) %>%
                formatStyle(
                  'dvoa_difference',
                  fontWeight = styleInterval(0, c('normal','bold')),
                  color = styleInterval(c(-30, -10, 0, 30),
                                        c('#a50026', '#f16d43', 'black', '#64bc61', '#23964f'))) %>%
                formatStyle(
                  'dline_net_adj_line_yards',
                  fontWeight = styleInterval(.5, c('normal','bold')),
                  color = styleInterval(c(-1, -.5, 0, 1),
                                        c('#a50026', '#f16d43', 'black', '#64bc61', '#23964f'))) %>%
                formatStyle(
                  'net_adj_line_yd_diff_vs_off',
                  fontWeight = styleInterval(.5, c('normal','bold')),
                  color = styleInterval(c(-1, -.5, 0, 1),
                                        c('#a50026', '#f16d43', 'black', '#64bc61', '#23964f')))

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
            th(colspan = 5,''),
            th(class = 'dt-center', colspan = 3, 'Rushing / Game'),
            th(class = 'dt-center', colspan = 4, 'Advanced Rushing'),
            th(class = 'dt-center', colspan = 2, 'Receiving / Game'),
            th(class = 'dt-center', colspan = 3, 'Advanced Receiving'),
            th(class = 'dt-center', colspan = 4, 'RZ Usage inside 10y'),
            th(class = 'dt-center', colspan = 4, 'DFS')
          ),
          tr(
            th(colspan = 1, 'Player'),
            th(colspan = 1, 'Opp'),
            th(colspan = 1, 'Total Touches'),
            th(colspan = 1, 'High Val Touches'),
            th(colspan = 1, 'HV %'),
            th(colspan = 1, 'Att'),
            th(colspan = 1, 'TD'),
            th(colspan = 1, 'Yds'),
            th(colspan = 1, 'DYAR'),
            th(colspan = 1, 'DVOA'),
            th(colspan = 1, 'Diff'),
            th(colspan = 1, 'Suc. %'),
            th(colspan = 1, 'Tgt'),
            th(colspan = 1, 'Yds'),
            th(colspan = 1, 'DYAR'),
            th(colspan = 1, 'DVOA'),
            th(colspan = 1, 'Diff'),
            th(colspan = 1, 'Tgt'),
            th(colspan = 1, 'Rush'),
            th(colspan = 1, 'TD'),
            th(colspan = 1, 'Rush %'),
            th(colspan = 1, 'Line'),
            th(colspan = 1, '$'),
            th(colspan = 1, 'Touches / $1k'),
            th(colspan = 1, 'HV / $1k'))
        )
      ))


      render_off_rb <-  subset(rb_off,
                                 total_touches >= input$tot_touches[1] & total_touches <= input$tot_touches[2] &
                                 att >= input$rush_att[1] & att <= input$rush_att[2] &
                                 inside10_perrush >= input$rush_10_per[1] & inside10_perrush <= input$rush_10_per[2] &
                                 line >= input$rb_off_line[1] & line <= input$rb_off_line[2] &
                                 fd_sal >= input$rb_salary[1] & fd_sal <= input$rb_salary[2 ]| is.na(total_touches) | is.na(inside10_perrush))

      datatable(render_off_rb,
                rownames = F,
                container = off_rb_container,
                options = list(pageLength = 10,
                              lengthMenu = c(10,20,30),
                              columnDefs = list(list(className = 'dt-center', targets = 'all')))) %>%
                formatStyle(c('att','td','yds',
                              'target','rec_yds',
                              'inside10_tgt', 'inside10_att','inside10_total_td','inside10_perrush'),
                            backgroundColor = '#F2F3F4')

    })

    ###### Graphing

    # RB Offense  Graph Output
    rb_reactive <- reactive({

      ## Offense
      rb_off_s1 <- input$off_rb_rows_selected

      reactive_off_rb <-  subset(rb_off,
                          total_touches >= input$tot_touches[1] & total_touches <= input$tot_touches[2] &
                          att >= input$rush_att[1] & att <= input$rush_att[2] &
                          inside10_perrush >= input$rush_10_per[1] & inside10_perrush <= input$rush_10_per[2] &
                          line >= input$rb_off_line[1] & line <= input$rb_off_line[2] | is.na(total_touches) | is.na(inside10_perrush))

      if(length(input$rb_select) == 0) {reactive_off_rb <- reactive_off_rb[rb_off_s1,]}

      ## Defense
      rb_def_s1 <- input$def_rb_rows_selected

      reactive_def_rb <-  subset(rb_def,
                                 rush_dvoa >= input$rush_dvoa[1] & rush_dvoa <= input$rush_dvoa[2] &
                                 net_adj_line_yd_diff_vs_off >= input$net_yds_diff[1] & net_adj_line_yd_diff_vs_off <= input$net_yds_diff[2] &
                                 dvoa_advantage >= input$dvoa_advantage[1] & dvoa_advantage <= input$dvoa_advantage[2] &
                                 total_touches >= input$total_touches[1] & total_touches <= input$total_touches[2])

      if(length(input$rb_select) == 0) {reactive_def_rb <- reactive_def_rb[rb_def_s1,]}

      ## Join Togeather
      all_rb <- full_join(rb_def, rb_off, by = c("full_name", "opp"))

      all_rb_plot <- full_join(reactive_def_rb, reactive_off_rb, by = c("full_name", "opp")) %>%
                     select(full_name, opp) %>%
                     left_join(all_rb, by = c("full_name", "opp"))
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
        geom_text_repel(aes(label = full_name), hjust = 0, vjust = -1) +
        theme_bw() +
        theme(
          axis.title = element_text(size = 12, face = "bold")
        )
    })


# # WR Tab Data -------------------------------------------------------------


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
              th(class = 'dt-center', colspan = 4, 'DVOA'),
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
                              fantasy_per_gm_fdpt >= input$fd_pts_gm_wr[1] & fantasy_per_gm_fdpt <= input$fd_pts_gm_wr[2] &
                              tgt >= input$wr_tg_vs[1] & tgt <= input$wr_tg_vs[2] &
                              yds >= input$pass_yds_gm[1] & yds <= input$pass_yds_gm[2] &
                              dvoa_advantage >= input$wr_dvoa_advantage[1] & dvoa_advantage <= input$wr_dvoa_advantage[2])

      datatable(wr_def_render,
                rownames = F,
                container = def_wr_container,
                options = list(pageLength = 15, lengthMenu = c(10,15,20))) %>%
                formatStyle(c('fantasy_per_gm_fdpt','tgt','yds','td',
                              'any_per_a','y_per_g','tot_y_per_p',
                              'pass_rank','pass_sacks','pass_adjustedsack_rate','sack_rate_diff'),
                            backgroundColor = '#F2F3F4') %>%
        formatStyle(
          'dvoa_advantage',
          fontWeight = styleInterval(0, c('normal','bold')),
          color = styleInterval(c(-30, -10, 0, 30),
                                c('#a50026', '#f16d43', 'black', '#64bc61', '#23964f'))) %>%
        formatStyle(
          'dvoa_difference',
          fontWeight = styleInterval(0, c('normal','bold')),
          color = styleInterval(c(-30, -10, 0, 30),
                                c('#a50026', '#f16d43', 'black', '#64bc61', '#23964f'))) %>%
        formatStyle(
          'sack_rate_diff',
          fontWeight = styleInterval(1, c('normal','bold')),
          color = styleInterval(c(-4, 0, 2, 5),
                                c('#a50026', '#f16d43', 'black', '#64bc61', '#23964f')))

      })

    # Offense WR
    output$off_wr <- renderDataTable({

      # Offense WR Container
      off_wr_container <- htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(colspan = 2,''),
            th(class = 'dt-center', colspan = 3, 'Per Game Stats'),
            th(class = 'dt-center', colspan = 6, 'Advanced Stats'),
            th(class = 'dt-center', colspan = 3, 'Market Share'),
            th(class = 'dt-center', colspan = 2, 'Red Zone Stats'),
            th(class = 'dt-center', colspan = 4, 'CB Matchup'),
            th(class = 'dt-center', colspan = 3, 'DFS Info')),
          tr(
             th(colspan = 1, 'Player'),
             th(colspan = 1, 'Opp'),
             th(colspan = 1, 'Target'),
             th(colspan = 1, 'Yds/Gm'),
             th(colspan = 1, 'TD'),
             th(colspan = 1, 'DYAR'),
             th(colspan = 1, 'DVOA'),
             th(colspan = 1, 'EYds - Yds'),
             th(colspan = 1, 'ADOT'),
             th(colspan = 1, 'Air Yards'),
             th(colspan = 1, 'RACR'),
             th(colspan = 1, 'Target %'),
             th(colspan = 1, 'Air Yard %'),
             th(colspan = 1, 'WOPR'),
             th(colspan = 1, 'Tgt'),
             #th(colspan = 1, 'TD'),
             th(colspan = 1, 'Target (%)'),
             th(colspan = 1, 'Target (%)'),
             th(colspan = 1, 'Pt / Tgt'),
             th(colspan = 1, 'Shadow'),
             th(colspan = 1, 'Matchup'),
             th(colspan = 1, 'Salary ($)'),
             th(colspan = 1, 'Target / $1k'),
             th(colspan = 1, 'Implied Total'))
        )))

      wr_off_render <- subset(wr_off,
                              (target >= input$wr_tgt[1] & target <= input$wr_tgt[2] | is.na(target)) &
                              (inside20_pertgt >= input$wr_rz_20[1] & inside20_pertgt <= input$wr_rz_20[2] | is.na(inside20_pertgt)) &
                              (vs_cb_fpt >= input$cb_pts_tgt[1] & vs_cb_fpt <= input$cb_pts_tgt[2] | is.na(vs_cb_fpt)) &
                              fd_sal >= input$wr_salary[1] & fd_sal <= input$wr_salary[2])
      # Team Selection
      if(input$wr_opponent != 'all') {wr_off_render <- subset(wr_off_render, opp == input$wr_opponent)}


      # for +/-/neutral CB pairings
      if(length(input$cb_matchup) == 1) {wr_off_render <- filter(wr_off_render, vs_cb_matchup == input$cb_matchup)

      } else if(length(input$cb_matchup) == 2)  {wr_off_render <- filter(wr_off_render, vs_cb_matchup == input$cb_matchup[1] | vs_cb_matchup == input$cb_matchup[2])
        } else {wr_off_render <- filter(wr_off_render, vs_cb_matchup == input$cb_matchup[1] | vs_cb_matchup == input$cb_matchup[2] | vs_cb_matchup == input$cb_matchup[3] | is.na(vs_cb_matchup))}

      datatable(wr_off_render,
                rownames = F,
                container = off_wr_container,
                options = list(pageLength = 15, lengthMenu = c(10,15,20))) %>%
                formatStyle(c("dyar","dvoa","eyds_diff","adot","air_yards","racr",
                              "inside20_tgt", "inside20_pertgt",
                              "fd_sal","target_per_1k","implied_total"), backgroundColor = '#f2f3f4')
    })


    # WR Table
    output$wrtable <- renderDataTable({

      render_wr <-  subset(wr,
                           fd_sal >= input$wr_salary[1] & fd_sal <= input$wr_salary[2] &
                           total_dvoa >= input$wr_dvoa[1] & total_dvoa <= input$wr_dvoa[2] &
                           pass_dvoa >= input$wr_pass_dvoa[1] & pass_dvoa <= input$wr_pass_dvoa[2] &
                           line >= input$wr_line[1] & line <= input$wr_line[2])

      DT::datatable(render_wr[, input$wr_vars], rownames = F, options = list(pageLength = 15, lengthMenu = c(10,15,20)))

    })

    # WR Graph Output
    wr_dat <- reactive({

      wr_s1 <- input$wrtable_rows_selected

      wr_render_table <- subset(wr,
                                fd_sal >= input$wr_salary[1] & fd_sal <= input$wr_salary[2] &
                                total_dvoa >= input$wr_dvoa[1] & total_dvoa <= input$wr_dvoa[2] &
                                pass_dvoa >= input$wr_pass_dvoa[1] & pass_dvoa <= input$wr_pass_dvoa[2] &
                                line >= input$wr_line[1] & line <= input$wr_line[2]) %>%
                        .[wr_s1,]

      return(wr_render_table)
    })

    # WR Offense  Graph Output
    wr_reactive <- reactive({

      ## Offense
      wr_off_s1 <- input$off_wr_rows_selected

      reactive_off_wr <-  subset(wr_off,
                                 target >= input$wr_tgt[1] & target <= input$wr_tgt[2] &
                                 inside20_pertgt >= input$wr_rz_20[1] & inside20_pertgt <= input$wr_rz_20[2] &
                                 vs_cb_fpt >= input$cb_pts_tgt[1] & vs_cb_fpt <= input$cb_pts_tgt[2])

      if(length(input$wr_select) == 0) {reactive_off_wr <- reactive_off_wr[wr_off_s1,]}

      ## Defense
      wr_def_s1 <- input$def_wr_rows_selected

      reactive_def_wr <-  subset(wr_def,
                                 fantasy_per_gm_fdpt >= input$fd_pts_gm_wr[1] & fantasy_per_gm_fdpt <= input$fd_pts_gm_wr[2] &
                                 tgt >= input$wr_tg_vs[1] & tgt <= input$wr_tg_vs[2] &
                                 yds >= input$pass_yds_gm[1] & yds <= input$pass_yds_gm[2] &
                                 dvoa_advantage >= input$wr_dvoa_advantage[1] & dvoa_advantage <= input$wr_dvoa_advantage[2])

      if(length(input$wr_select) == 0) {reactive_def_wr <- reactive_def_wr[wr_def_s1,]}

      ## Join Togeather
      all_wr <- full_join(wr_def, wr_off, by = c("full_name", "opp"))

      all_wr_plot <- full_join(reactive_def_wr, reactive_off_wr, by = c("full_name", "opp")) %>%
                     select(full_name, opp) %>%
                     left_join(all_wr, by = c("full_name", "opp"))

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
        geom_text_repel(aes(label = full_name), hjust = 0, vjust = -1) +
        theme_bw() +
        theme(
          axis.title = element_text(size = 12, face = "bold")
        )
    })
 
# # TE Tab Data -------------------------------------------------------------

    # Def TE Table

    output$def_te <- renderDataTable({

      # Defense TE Container
      def_te_container <- htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(colspan = 2,''),
            th(class = 'dt-center', colspan = 4, 'Points Against to TE'),
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
                              fantasy_per_gm_fdpt >= input$fd_pts_gm_te[1] & fantasy_per_gm_fdpt <= input$fd_pts_gm_te[2] &
                              tgt >= input$te_tg_vs[1] & tgt <= input$te_tg_vs[2] &
                              yds >= input$pass_yds_gm_te[1] & yds <= input$pass_yds_gm_te[2] &
                              dvoa_advantage >= input$te_dvoa_advantage[1] & dvoa_advantage <= input$te_dvoa_advantage[2])

      datatable(te_def_render,
                rownames = F,
                container = def_te_container,
                options = list(pageLength = 15, lengthMenu = c(10,15,20))) %>%
        formatStyle(c('fantasy_per_gm_fdpt','tgt','yds','td',
                      'any_per_a','y_per_g','tot_y_per_p',
                      'pass_rank','pass_sacks','pass_adjustedsack_rate','sack_rate_diff'),
                    backgroundColor = '#F2F3F4') %>%
        formatStyle(
          'dvoa_advantage',
          fontWeight = styleInterval(0, c('normal','bold')),
          color = styleInterval(c(-30, -10, 0, 30),
                                c('#a50026', '#f16d43', 'black', '#64bc61', '#23964f'))) %>%
        formatStyle(
          'dvoa_difference',
          fontWeight = styleInterval(0, c('normal','bold')),
          color = styleInterval(c(-30, -10, 0, 30),
                                c('#a50026', '#f16d43', 'black', '#64bc61', '#23964f'))) %>%
        formatStyle(
          'sack_rate_diff',
          fontWeight = styleInterval(1, c('normal','bold')),
          color = styleInterval(c(-4, 0, 2, 5),
                                c('#a50026', '#f16d43', 'black', '#64bc61', '#23964f')))
    })

    # Offense TE
    output$off_te <- renderDataTable({

      # Offense TE Container
      # Offense WR Container
      off_te_container <- htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(colspan = 2,''),
            th(class = 'dt-center', colspan = 3, 'Per Game Stats'),
            th(class = 'dt-center', colspan = 10, 'Advanced Stats'),
            th(class = 'dt-center', colspan = 3, 'Red Zone Stats'),
            th(class = 'dt-center', colspan = 3, 'DFS Info')),
          tr(
            th(colspan = 1, 'Player'),
            th(colspan = 1, 'Opp'),
            th(colspan = 1, 'Target'),
            th(colspan = 1, 'Yds/Gm'),
            th(colspan = 1, 'TD'),
            th(colspan = 1, 'DYAR'),
            th(colspan = 1, 'DVOA'),
            th(colspan = 1, 'EYds - Yds'),
            th(colspan = 1, 'ADOT'),
            th(colspan = 1, 'Air Yards'),
            th(colspan = 1, 'RACR'),
            th(colspan = 1, 'Target %'),
            th(colspan = 1, 'Air Yard %'),
            th(colspan = 1, 'WOPR'),
            th(colspan = 1, 'Passing DVOA'),
            th(colspan = 1, 'Tgt'),
            th(colspan = 1, 'TD'),
            th(colspan = 1, 'Target (%)'),
            th(colspan = 1, 'Salary ($)'),
            th(colspan = 1, 'Target / $1k'),
            th(colspan = 1, 'Implied Total'))
        )))

      # Editing Table to render
      te_off_render <- subset(te_off,
                              target >= input$te_tgt[1] & target <= input$te_tgt[2] &
                              td >= input$te_rec_td[1] & td <= input$te_rec_td[2] &
                              inside20_pertgt >= input$te_rz_20[1] & inside20_pertgt <= input$te_rz_20[2] &
                              pass_dvoa >= input$off_pass_dvoa_te[1] & pass_dvoa <= input$off_pass_dvoa_te[2] &
                              fd_sal >= input$te_salary[1] & fd_sal <= input$te_salary[2])

      datatable(te_off_render,
                rownames = F,
                container = off_te_container,
                options = list(pageLength = 15, lengthMenu = c(10,15,20))) %>%
                formatStyle(c("dyar","dvoa","eyds_diff","adot","air_yards","racr",
                              "target_per","air_yard_per","wopr","pass_dvoa",
                              "fd_sal","inside20_pertgt","implied_total"),
                            backgroundColor = '#F2F3F4')

      })


    # TE Offense  Graph Output
    te_reactive <- reactive({

      ## Offense
      te_off_s1 <- input$off_te_rows_selected

      reactive_off_te <-  subset(te_off,
                                 target >= input$te_tgt[1] & target <= input$te_tgt[2] &
                                 td >= input$te_rec_td[1] & td <= input$te_rec_td[2] &
                                 inside20_pertgt >= input$te_rz_20[1] & inside20_pertgt <= input$te_rz_20[2] &
                                 dvoa >= input$off_pass_dvoa_te[1] & dvoa <= input$off_pass_dvoa_te[2])

      if(length(input$te_select) == 0) {reactive_off_te <- reactive_off_te[te_off_s1,]}

      ## Defense
      te_def_s1 <- input$def_te_rows_selected

      reactive_def_te <-  subset(te_def,
                                 fantasy_per_gm_fdpt >= input$fd_pts_gm_te[1] & fantasy_per_gm_fdpt <= input$fd_pts_gm_te[2] &
                                 tgt >= input$te_tg_vs[1] & tgt <= input$te_tg_vs[2] &
                                 y_per_g >= input$pass_yds_gm_te[1] & y_per_g <= input$pass_yds_gm_te[2] &
                                 dvoa_advantage >= input$te_dvoa_advantage[1] & dvoa_advantage <= input$te_dvoa_advantage[2])

      if(length(input$te_select) == 0) {reactive_def_te <- reactive_def_te[te_def_s1,]}

      ## Join Togeather
      all_te <- full_join(te_def, te_off, by = c("full_name", "opp"))

      all_te_plot <- full_join(reactive_def_te, reactive_off_te, by = c("full_name", "opp")) %>%
                     select(full_name, opp) %>%
                     left_join(all_te, by = c("full_name", "opp"))


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
        geom_text_repel(aes(label = full_name), hjust = 0, vjust = -1) +
        theme_bw() +
        theme(
          axis.title = element_text(size = 12, face = "bold")
        )
    })

}

# Run the application
shinyApp(ui = ui, server = server)
