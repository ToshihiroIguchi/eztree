library(shiny)
library(tibble)
library(ggplot2)

shinyUI(
  fluidPage(
    titlePanel("Easy decision tree"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Choose csv file",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        tags$hr(),
        htmlOutput("ydata"),
        htmlOutput("xdata"),
        htmlOutput("method"),
        
        actionButton("submit", "Analyze")

      ),
      
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Table", tableOutput('table')),
                    tabPanel("Result", 
                             plotOutput("plot"),
                             verbatimTextOutput("sum"))

        )
      )
    )
  )
)