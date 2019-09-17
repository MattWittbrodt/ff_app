#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#


library(shiny)
library(tidyverse)
library(DT)

# Sourcing team name changes
# source("~/ff_shiny_app/ff_app/find_names.R")
# tm_names <- readxl::read_xlsx("~/ff_shiny_app/ff_app/data/team_names.xlsx")
# 
# # Getting full dataframe --------------------------------------------------
# source("~/ff_shiny_app/ff_app/shiny_df_creation.R")
# df <- shiny_df(2)
# 
# # DFS Specific Data
# dfs_df <- select(df,
#                  player, 
#                  pos, 
#                  tm, 
#                  field, 
#                  opp, 
#                  fd_sal,
#                  projected_own,
#                  cash_odds,
#                  gpp_odds,
#                  implied_own,
#                  fd_lev,
#                  proj_ffpts,
#                  proj_afpa,
#                  proj_afpa_rk,
#                  line,
#                  total,
#                  implied_total) %>%
#             filter(fd_sal > 0) %>%
#             mutate(points_per_1k = round(proj_ffpts / (fd_sal/1000),2))

df <- readxl::read_xlsx("~/ff_shiny_app/all_data_wk_2.xlsx")


#qb <- 


# Importing and Preprocessing Data ----------------------------------------
# leverage <- read.csv("data/4for4-fantasy-football-gpp-leverage-scores-table_wk2.csv") %>%
#             select(-Opp)
# projections <- read.csv("data/4for4_W2_projections.csv") %>% 
#                select(-Team,-FG,-XP,-Pos,-PID,-Week,-Season,-Fum,
#                       -Comp, -Pass.Att, -Pass.Yds, -Pass.TD, -INT, -Rush.Att, -Rush.Yds, 
#                       -Rush.TD, -Rec, -Rec.Yds, -Rec.TD)

# Vegas lines
# source("vegas_lines.R")
# vegas <- vegas_lines() %>%
#          select(team, line, total, implied_total)

# Removing some of the irrelevant columns
# d_all <- merge(leverage, projections, by = "Player") %>%
#          select(-Pa1D, -Ru1D, -Rec1D, -Grade) %>%
#          mutate(Cash.Odds = as.numeric(gsub("\\%", "", Cash.Odds)),
#                 GPP.Odds = as.numeric(gsub("\\%", "", GPP.Odds)),
#                 Implied.Own. = as.numeric(gsub("\\%", "", Implied.Own.)),
#                 Projected.Own. = as.numeric(gsub("\\%", "", Projected.Own.)),
#                 Points_Per_1k = round(FFPts / (FD.Sal../1000),2),
#                 Tm = as.character(Tm))
#          
# d_names <- colnames(d_all) %>%
#            str_remove_all("[.]")

# colnames(d_all) <- d_names

# Merging total dataset and vegas lines
# d_all <- inner_join(d_all, vegas, by = c("Tm" = "team"))


# Define UI for application that draws a histogram
# Fluidpage adjusts to window size
ui <- fluidPage(
    
    tabsetPanel(type = "tabs",
                
                tabPanel("DFS FanDuel",

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
                           min = min(dfs_df[["line"]]),
                           max = max(dfs_df[["line"]]),
                           value = c(min,max)
               ))),

        # Show a plot of the generated distribution
        fluidRow(
         column(12,
           DT::dataTableOutput("fanduel")
        )
    )#,
    
    # fluidRow(
    #     column(3,
    #            selectInput("y_axis",
    #                        h3("Y Axis"),
    #                        choices = list("FDSal",
    #                                       "ProjectedOwn",
    #                                       "CashOdds",
    #                                       "GPPOdds",
    #                                       "FDLEV",
    #                                       "FFPts",
    #                                       "Points_Per_1k",
    #                                       "line",
    #                                       "total",
    #                                       "implied_total"),
    #                        selected = "FDSal")),
    #     column(3,
    #            selectInput("x_axis",
    #                        h3("X Axis"),
    #                        choices = list("FDSal",
    #                                       "ProjectedOwn",
    #                                       "CashOdds",
    #                                       "GPPOdds",
    #                                       "FDLEV",
    #                                       "FFPts",
    #                                       "Points_Per_1k",
    #                                       "line",
    #                                       "total",
    #                                       "implied_total"),
    #                        selected = "GPPOdds")),
    #     column(6,
    #            plotOutput('plot', height = 500))
    # ),
    # 
    # fluidRow(column(12,
    #                  DT::dataTableOutput("dat")))
)))

#tabPanel("Data")))

# Define server logic required to draw a histogram
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
    # dat <- reactive({
    #     
    #     s1 <- input$fanduel_rows_selected
    #     
    #     render_table <- subset(d_all, 
    #                            FDSal >= input$salary[1] & FDSal <= input$salary[2] &
    #                                Points_Per_1k >= input$value[1] & Points_Per_1k <= input$value[2] &
    #                                FDLEV >= input$lev[1] & FDLEV <= input$lev[2] & 
    #                                line >= input$line[1] & line <= input$line[2] &
    #                                Pos == input$Position) %>% select(-ImpliedOwn) %>%
    #                     .[s1,]
    #     
    #     # test <- filter(d_all, Pos == input$Position) %>%
    #     #         .[s1,] 
    #     
    #     #        select(Player, input$x_axis, input$y_axis)
    #     
    #     return(render_table) #datatable(test, rownames = F)
    # })
    # 
    # output$plot = renderPlot({
    #     
    #     plot_data <- dat()
    #     ggplot(plot_data, aes(x = plot_data[[input$x_axis]], plot_data[[input$y_axis]])) +
    #           xlab(input$x_axis) + 
    #           ylab(input$y_axis) +
    #           geom_point(size = 6, color = "#0000b7", alpha = 0.5) +
    #           geom_text(aes(label = Player), hjust = 0, vjust = -1) +
    #           theme_bw() +
    #           theme(
    #             axis.title = element_text(size = 12, face = "bold")
    #           )
    #})
}

# Run the application 
shinyApp(ui = ui, server = server)
