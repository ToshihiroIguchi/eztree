#ライブラリ読み込み
#library(rpart) #CART
library(partykit) #ctree
library(evtree) #evtree
library(RWeka) #J48
library(C50) #C5.0
source("eztree.R")


server <- function(input, output, session) {
  observeEvent(input$file, {
    #テーブルにて表示
    csv_file <- reactive({read.csv(input$file$datapath)})
    output$table <- renderTable({head(csv_file(), n = 30)})
    
    #説明変数を選択
    output$ydata <- renderUI({ 
      selectInput("ydata", "Purpose variable", choices = colnames(csv_file()))
    })
  })
  
  observeEvent(input$ydata, {
    csv_file <- reactive({read.csv(input$file$datapath)})
    #目的変数を選択
    output$xdata <- renderUI({
      checkboxGroupInput("xdata", 
                         label = "Explanatory variable",
                         choices = get.explanatory(csv_file(), input$ydata),
                         selected = get.explanatory(csv_file(), input$ydata))
      })
    #手法を選択
    output$method <- renderUI({
      selectInput("method", "Method", choices = get.method(csv_file()[input$ydata]))
    })
  })

  observeEvent(input$submit, {

    csv_file <- reactive({read.csv(input$file$datapath)})
    
    result <- reactive({eztree(chr2formula(y = input$ydata, x= input$xdata), 
                               data = csv_file(), 
                               method = get.short(input$method)
                               )
      })
    
    output$plot <- renderPlot({plot(result())})
    
    output$sum <- renderPrint({(result())})
    


  })
    
}





