#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {
    
    data <- reactive({
        req(input$dataset)
        read.csv(file   = input$dataset$datapath, 
                 header = input$header,
                 sep    = input$sep)
    })
    
    filtereddata <- eventReactive({
        input$update
        data()
    },  {
        req(data())
        if(is.null(input$select) || input$select == "")
            data() else 
                data()[data()$Id == input$patient, colnames(data()) %in% input$select]
    })
    
    observeEvent(data(), {
        updateSelectInput(session, "select", choices = colnames(data()))
    })
    
    observeEvent(data(), {
        updateSelectInput(session, "patient", choices = unique(data()$Id))
    })
    
    output$mytable  <- renderDataTable(filtereddata())
    
})
