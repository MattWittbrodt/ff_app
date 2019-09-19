#
# DFs Shiny
#

library(shiny)
library(tidyverse)
library(DT)

df <- readxl::read_xlsx("data/all_data_wk_3.xlsx")
    

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
           total_dvoa.x,
           pass_def_dvoa,
           dline_pass_rank,
           dline_pass_adjustedsack_rate,
           def_third_d_per,
           def_pass_comp_per,
           def_pass_qb_rating_allowed,
           def_pass_adj_net_yds_per_att,
           def_pass_yds_per_gm)

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
            

# RB Data -----------------------------------------------------------------

rb <- filter(df, proj_pos == "RB") %>%
      select(proj_player,
             proj_opp,
             ytd_rush_att,
             ytd_rush_yds_per_att,
             ytd_rush_yds_per_gm,
             ytd_rush_td,
             ytd_rec_target,
             ytd_rec_rec,
             ytd_rec_yds,
             ytd_rec_rec_per_gm,
             rushing_twenty_att,
             rushing_twenty_yds,
             rushing_twenty_td,
             rushing_twenty_per_rush,
             rushing_ten_att,
             rushing_ten_yds,
             rushing_ten_td,
             rushing_ten_per_rush,
             rushing_five_att,
             rushing_five_yds,
             rushing_five_td,
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
             rush_def_dvoa,
             dline_stuffed,
             dline_rbyards,
             dline_2nd_level_yards,
             oline_adj_lineyards,
             oline_powerrank)


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
           oline_pass_adjustedsack_rate,
           dline_2nd_level_yards,
           oline_adj_lineyards,
           oline_powerrank)


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
           oline_pass_adjustedsack_rate,
           dline_2nd_level_yards,
           oline_adj_lineyards,
           oline_powerrank)

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
    
        fluidRow(column(12,
                         DT::dataTableOutput("dat")))
        ),



# QB Panel ----------------------------------------------------------------

    tabPanel("QB",
             
             
             fluidRow(column(2,
                 
                 checkboxGroupInput("qb_vars", "QB columns to show:",
                                    names(qb), selected = names(qb))
             ),
             
 
                 column(10,
                        div(DT::dataTableOutput("qbtable"), style = "font-size:75%")
                 )),
             fluidRow(column(12,
                             p("Data Dictionary: Rz = Red Zone, ytd = Year to Date, 
                             DVOA = Defense-adjusted Value Over Average where negative is better,
                             Dline = Defensive Line Ratings,
                             def_ = Raw Defense Stats")))
),


# RB Panel ----------------------------------------------------------------

tabPanel("RB",
         
         fluidRow(column(12,p("YTD Stats are Per Game"))),
         
         fluidRow(column(2,
                         
                         checkboxGroupInput("rb_vars", "RB columns to show:",
                                            names(rb), selected = names(rb))
         ),
         
         
         column(10,
                div(DT::dataTableOutput("rbtable"), style = "font-size:75%")
         )),
         fluidRow(column(12,
                         p("Data Dictionary: Rz = Red Zone, ytd = Year to Date, 
                             DVOA = Defense-adjusted Value Over Average where negative is better,
                             Dline = Defensive Line Ratings,
                             def_ = Raw Defense Stats")))
),

# WR Panel ----------------------------------------------------------------
tabPanel("WR",
         
         fluidRow(column(12,p("YTD Stats are Per Game"))),
         
         fluidRow(column(2,
                         
                         checkboxGroupInput("wr_vars", "WR columns to show:",
                                            names(wr), selected = names(wr))
         ),
         
         
         column(10,
                div(DT::dataTableOutput("wrtable"), style = "font-size:75%")
         )),
         fluidRow(column(12,
                         p("Data Dictionary: Rz = Red Zone, ytd = Year to Date, 
                             DVOA = Defense-adjusted Value Over Average where negative is better,
                             Dline = Defensive Line Ratings,
                             def_ = Raw Defense Stats")))
),

# TE Table ----------------------------------------------------------------
tabPanel("TE",
         
         fluidRow(column(12,p("YTD Stats are Per Game"))),
         
         fluidRow(column(2,
                         
                         checkboxGroupInput("te_vars", "TE columns to show:",
                                            names(te), selected = names(te))
         ),
         
         column(10,
                div(DT::dataTableOutput("tetable"), style = "font-size:75%")
         )),
         fluidRow(column(12,
                         p("Data Dictionary: Rz = Red Zone, ytd = Year to Date, 
                             DVOA = Defense-adjusted Value Over Average where negative is better,
                             Dline = Defensive Line Ratings,
                             def_ = Raw Defense Stats")))
))


# Server Function ---------------------------------------------------------
server <- function(input, output) {
    
    
    #table <- reactive({
           #     d <- subset(d_all, FD.Sal.. >= input$salary)
    #})

    output$fanduel <- renderDataTable({
        
        # Editing table for rendering
        render_table <- subset(dfs_df,
                               fd_sal >= input$salary[1] & fd_sal <= input$salary[2] &
                               points_per_1k >= input$value[1] & points_per_1k <= input$value[2] &
                               fd_lev >= input$lev[1] & fd_lev <= input$lev[2] &
                               line >= input$line[1] & line <= input$line[2] &
                               pos == input$pos) %>%
                        select(-implied_own)
        
        datatable(render_table, rownames = F, options = list(pageLength = 20, lengthMenu = c(10,20,30)))

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

        return(render_table) #datatable(test, rownames = F)
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
    
    output$qbtable <- renderDataTable({
        
        # Editing table for rendering
        #render_qb_table <- qb
        # 
        # datatable(render_qb_table, 
        #           rownames = F, 
        #           options = list(pageLength = 20, lengthMenu = c(10,20,30)))
        
        #output$qbtable <- renderDataTable({
            DT::datatable(qb[, input$qb_vars])
        })
    
    # RB Table
    output$rbtable <- renderDataTable({
        
        datatable(rb[, input$rb_vars])
    })
    
    # WR Table
    output$wrtable <- renderDataTable({
        
        datatable(wr[, input$wr_vars])
    })
    
    # TE Table
    output$tetable <- renderDataTable({
        
        datatable(te[, input$te_vars])
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
