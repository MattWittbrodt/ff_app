###
### This is for the slider module
###

library(shiny)

# histogramUI <- function(id) {
#   tagList(
#     selectInput(NS(id, "var"), "Variable", names(mtcars)),
#     numericInput(NS(id, "bins"), "bins", 10, min = 1),
#     plotOutput(NS(id, "hist"))
#   )
# }https://stackoverflow.com/questions/46555355/passing-data-within-shiny-modules-from-module-1-to-module-2

sliderUI <- function(id, min, max, name) {
  ns <- NS(id)
  tagList(
    sliderInput(ns("version"), name, min = min, max = max, val = c(min, max)),
    textOutput(ns("low")),
    textOutput(ns("high"))
  )
}

sliderServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {

              return(
                list(
                  low = reactive({input$version[1]}),
                  high = reactive({input$version[2]})
                )
              )})}


                 #vals <- reactiveValues()
               #observe({vals$low <- input$version[1]})
               #observe({vals$high <- input$versoin[2]})
               # output$low <- renderText({input$version[1]})
               # output$high <- renderText({input$version[2]})
               #
               # #return(vals)
               #
               # })}


# ui <- fluidPage(
#   sliderUI("one", 3000, 10000, name = "sLiDe Me")
# )
#
# server <- function(input, output, session) {
#         sliderServer("one")
# }
#
# shinyApp(ui, server)
