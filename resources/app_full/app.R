library(dplyr)
library(ggraph)
library(shiny)
library(tidygraph)

source("helpers.R")

load("actor_data_course3.rdata")

# get list of actors
actors <- actor_network %>%
  activate(nodes) %>%
  arrange(name) %>%
  pull(name)

# get distance matrix
distance_mat <- igraph::distances(actor_network)

ui <- fluidPage(
  theme = shinythemes::shinytheme("yeti"),
  # Application title
  titlePanel("How many handshakes does it take from one actor to another?"),

  fluidRow(
    column(
      9,
      plotOutput("graph", height = "400px"),
    ),

    column(
      3,
      selectInput(
        inputId = "from_name",
        label = "Select Starting Actor",
        choices = actors,
        multiple = FALSE,
        selected = "Leonardo DiCaprio" # Default values for select
      ),
      checkboxInput(
        inputId = "draw_shortest_path",
        label = "Draw Shortest Path to Another Actor?"
      ),
      uiOutput(
        outputId = "to_name_wrapper"
      )
    )
  ),
  fluidRow(
    column(
      9,
      plotOutput("graph2", height = "350px", width = "100%")
    )
  )
)

server <- function(input, output) {
  # define second input
  output$to_name_wrapper <- renderUI({
    if (input$draw_shortest_path) {
      selectInput(
        inputId = "to_name",
        label = "Select End Point Actor",
        choices = actors[!stringr::str_detect(actors, input$from_name)],
        multiple = FALSE,
        selected = "Naomi Scott" # Default values for select
      )
    }
  })

  deg_sep_data <-
    reactive({
      calculate_deg_sep(
        actor_network,
        distance_mat,
        input$from_name
      )
    })

  shortest_path_data <-
    reactive({
      if (input$draw_shortest_path) {
        req(input$to_name)
        calculate_shortest_path(
          deg_sep_data(),
          input$from_name,
          input$to_name
        )
      }
    })

  output$graph <- renderPlot({
    if (input$draw_shortest_path) {
      draw_shortest_path(shortest_path_data())
    } else {
      draw_deg_sep(deg_sep_data())
    }
  })

  output$graph2 <- renderPlot({
    if (input$draw_shortest_path) {
      draw_sub_graph(
        shortest_path_data(),
        input$from_name,
        input$to_name
      )
    }
  })
}

# Run the application
options(shiny.autoreload = TRUE)
shinyApp(ui = ui, server = server)
