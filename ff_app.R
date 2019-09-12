#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)

#Importing Data
leverage <- read.csv("/data/4for4-fantasy-football-gpp-leverage-scores-table_wk2.csv") %>%
            select(-Opp)
projections <- read.csv("/data/4for4_W2_projections.csv") %>% 
               select(-Team,-FG,-XP,-Pos,-PID,-Week,-Season,-Fum,
                      -Comp, -Pass.Att, -Pass.Yds, -Pass.TD, -INT, -Rush.Att, -Rush.Yds, 
                      -Rush.TD, -Rec, -Rec.Yds, -Rec.TD)

# Removing some of the irrelevant columns
d_all <- merge(leverage, projections, by = "Player") %>%
         select(-Pa1D, -Ru1D, -Rec1D, -Grade) %>%
         mutate(Cash.Odds = as.numeric(gsub("\\%", "", Cash.Odds)),
                GPP.Odds = as.numeric(gsub("\\%", "", GPP.Odds)),
                Implied.Own. = as.numeric(gsub("\\%", "", Implied.Own.)),
                Projected.Own. = as.numeric(gsub("\\%", "", Projected.Own.)),
                Points_Per_1k = round(FFPts / (FD.Sal../1000),2))
         
d_names <- colnames(d_all) %>%
           str_remove_all("[.]")

colnames(d_all) <- d_names

# Define UI for application that draws a histogram
# Fluidpage adjusts to window size
ui <- fluidPage(

    # Application title
    titlePanel("DFS FanDuel Predictions"),
    
    fluidRow(
        column(3, 
               selectInput("Position",
                                  h3("Position"),
                                  choices = list("QB","RB","WR","TE","DEF"),
                                  selected = "QB"))),
        #),
    #     column(3, 
    #            selectInput("SortBy",
    #                               h3("Sort By"),
    #                               choices = list("FDSal",
    #                                              "ProjectedOwn",
    #                                              "CashOdds",
    #                                              "GPPOdds",
    #                                              "ImpliedOwn",
    #                                              "FDLEV")))
    # ),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(3,
              sliderInput("salary",
                        "Minimum FanDuel Salary:",
                        min = min(d_all$FDSal),
                        max = max(d_all$FDSal),
                        value = c(min,max)
        )),
        column(3,
               sliderInput("value",
                    "Points Per $1000",
                    min = min(d_all$Points_Per_1k),
                    max = max(d_all$Points_Per_1k),
                    value = c(min,max)
               )),
        column(3,
               sliderInput("lev",
                           "Leverage",
                           min = min(d_all$FDLEV),
                           max = max(d_all$FDLEV),
                           value = c(min,max)
               ))),

        # Show a plot of the generated distribution
        fluidRow(
         column(12,
           DT::dataTableOutput("fanduel")
        )
    ),
    
    fluidRow(
        column(3,
               selectInput("y_axis",
                           h3("Y Axis"),
                           choices = list("FDSal","ProjectedOwn","CashOdds","GPPOdds","FDLEV","FFPts","Points_Per_1k"),
                           selected = "FDSal")),
        column(3,
               selectInput("x_axis",
                           h3("X Axis"),
                           choices = list("FDSal","ProjectedOwn","CashOdds","GPPOdds","FDLEV","FFPts","Points_Per_1k"),
                           selected = "FDSal")),
        column(6,
               plotOutput('plot', height = 500))
    ),
    
    fluidRow(column(12,
                    DT::dataTableOutput("dat")))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #table <- reactive({
           #     d <- subset(d_all, FD.Sal.. >= input$salary)
    #})

    output$fanduel <- renderDataTable({
        
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'orange')
        
        render_table <- subset(d_all, 
                               FDSal >= input$salary[1] & FDSal <= input$salary[2] &
                               Points_Per_1k >= input$value[1] & Points_Per_1k <= input$value[2] &
                               FDLEV >= input$lev[1] & FDLEV <= input$lev[2] &    
                               Pos == input$Position)
        datatable(render_table, rownames = F)

       })
    
    
    # Graph Output
    dat <- reactive({
        
        s1 <- input$fanduel_rows_selected
        
        test <- filter(d_all, Pos == input$Position) %>%
                .[s1,] 
        
        #        select(Player, input$x_axis, input$y_axis)
        
        return(test) #datatable(test, rownames = F)
    })
    
    output$plot = renderPlot({
        
        plot_data <- dat()
        ggplot(plot_data, aes(x = plot_data[[input$x_axis]], plot_data[[input$y_axis]])) +
              xlab(input$x_axis) + 
              ylab(input$y_axis) +
              geom_point(size = 6, color = "#0000b7", alpha = 0.5) +
              geom_text(aes(label = Player), hjust = 0, vjust = -1) +
              theme_bw() +
              theme(
                axis.title = element_text(size = 12, face = "bold")
              )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
