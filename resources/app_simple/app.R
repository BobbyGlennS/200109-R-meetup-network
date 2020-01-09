library(dplyr)
library(ggraph)
library(shiny)
library(tidygraph)

source("helpers.R")

load("../actor_data_course3.rdata")

# get list of actors
actors <- actor_network %>%
  activate(nodes) %>%
  arrange(name) %>%
  pull(name)

# get distance matrix
distance_mat <- igraph::distances(actor_network)

ui <- fluidPage(

  # Application title
  titlePanel("How many handshakes does it take from one actor to another?"),

  sidebarLayout(

    mainPanel(
      plotOutput("graph")
    ),

    sidebarPanel(
      selectInput(
        inputId = "from_name",
        label = "Select Starting Actor",
        choices = actors,
        multiple = FALSE,
        selected = "Bruce Willis" # Default values for select
      )
    )
  )
)

server <- function(input, output) {
  deg_sep_data <-
    reactive({
      calculate_deg_sep(
        actor_network,
        distance_mat,
        input$from_name
      )
    })

  output$graph <- renderPlot({
    draw_deg_sep(deg_sep_data())
  })

}

# Run the application
options(shiny.autoreload = TRUE)
shinyApp(ui = ui, server = server)
