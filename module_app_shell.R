#
# App Shell
#

library(shiny)
library(tidyverse)
library(DT)
library(ggrepel)
library(reactable)

source("C:/Users/mattw/Documents/ff_shiny_app/ff_app/global.R")
source("C:/Users/mattw/Documents/ff_shiny_app/ff_app/slider.R")

sliderUI <- function(id, min, max, name) {

  ns <- NS(id)

  tagList(
    sliderInput(ns("version"), name, min = min, max = max, val = c(min, max)),
    textOutput(ns("low")),
    textOutput(ns("high"))
  )

}

sliderServer <- function(input, output, session) {

                 return(
                   list(
                     low = reactive({input$version[1]}),
                     high = reactive({input$version[2]})
                   )
                 )}


table_UI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 6,
        DT::dataTableOutput(ns("table"))
      )
    )
  )

}


table_server <- function(input, output, session, vars) {


                tbl <- reactive({t <- filter(dfs_df, pricing_current > vars$low() & pricing_current < vars$high()); return(t)})

                output$table <- renderDataTable({datatable(tbl())})
               }


ui <- fluidPage(
  fluidRow(sliderUI("one", 3000, 10000, name = "sLiDe Me")),
  fluidRow(table_UI("tbl_sal"))
)

server <- function(input, output, session) {
  sal <- callModule(sliderServer, "one")

  tb <- callModule(table_server,"tbl_sal", vars = sal)
}

shinyApp(ui, server)
