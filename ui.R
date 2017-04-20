
library(shiny)


dataset_list <- function() {
  datasets <- c(as.character(lapply(strsplit(dir("..", ".*\\.txt"), "\\."), "[", 1)))
  select_list <- seq(length(datasets))
  names(select_list) <- datasets 
  select_list  
}

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Graph Centrality"),

  fluidRow(
    column(1),
    column(3,
           fluidRow(
              fileInput("dataset", "Dataset", multiple = FALSE, accept = NULL, width = NULL,
                       buttonLabel = "Browse...", placeholder = "No file selected")
           ),
           fluidRow(
              checkboxInput("simplifyGraph", "Remove duplicate Paths", TRUE)
           ),
           fluidRow(
             selectInput("centr_meas",
                         "Centrality",
                         list("Betweenness"=1,
                              "Degree"=2,
                              "Closeness"=3,
                              "Eigenvalue"=4),
                         multiple=FALSE,
                         selectize=FALSE)
           ),
           fluidRow(
             DT::dataTableOutput("table")
           ),
           fluidRow(div(style="height: 40px;")),
           fluidRow(
             div(id="APL", "Average Path Length:", textOutput("avgPathLength", inline=TRUE), 
              style="font-weight: bold; font-size: 16pt; display: inline;")
           )
    ),
  
    column(8,
           tags$head(tags$style("#graphPlot {height: 80vh !important;}")),
           plotOutput("graphPlot", width="100%")
    )
  )
))