#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(readxl)
library(nlme)
library(knitr)
library(DT)
library(ggplot2)
library(ggthemes)
library(plotly)


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
        if(is.null(input$outcome) || input$outcome == "")
            data()[data()$Id == input$patient, c("Id", "Date", "Assigned.Treatment")] else 
                data()[data()$Id == input$patient, colnames(data()) %in% c(input$date, input$treatment, input$outcome)]
    })
    
    observeEvent(data(), {
        updateSelectInput(session, "date", choices = colnames(data()), selected = c("Date"))
    })
    
    
    observeEvent(data(), {
        updateSelectInput(session, "treatment", choices = colnames(data()), selected = c("Assigned.Treatment"))
    })
    
    observeEvent(data(), {
        updateSelectInput(session, "outcome", choices = colnames(data()), selected = c("emap", "emaf", "emas"))
    })
    
    observeEvent(data(), {
        updateSelectInput(session, "patient", choices = unique(data()$Id))
    })
    
    
    sliderValues <- reactive({
        df <- filtereddata() %>%
            mutate(Treatment = factor(Assigned.Treatment, levels = c("Baseline", "Usual Care", "Yoga", "Massage")))
        
        df <- df %>%
            group_by(Date, Treatment) %>% 
            filter(Treatment != "Baseline")
        
        
        fit <- gls(model       = as.numeric(emap) ~ Treatment, 
                   correlation = corAR1(form = ~1),
                   subset      = which(Treatment != "Baseline"),
                   control     = list(singular.ok = TRUE),
                   na.action   = na.omit, 
                   data        = df)
        
        data.frame(
            Output        = c("Mean EMA pain averaged during usual care",
                              "Mean EMA pain averaged increased during yoga",
                              "Mean EMA pain averaged increased during massage"),
            Estimate      = c(round(summary(fit)$tTable[, 1], 2)),
            `p-value`     = c("-", round(summary(fit)$tTable[-1, 4], 2)))
    })
    
    
    sliderValues1 <- reactive({
        df <- filtereddata() %>%
            mutate(Treatment = factor(Assigned.Treatment, levels = c("Baseline", "Usual Care", "Yoga", "Massage")))
        
        df <- df %>%
            group_by(Date, Treatment) %>% 
            filter(Treatment != "Baseline")
        
        
        fit <- gls(model       = as.numeric(emaf) ~ Treatment, 
                   correlation = corAR1(form = ~1),
                   subset      = which(Treatment != "Baseline"),
                   control     = list(singular.ok = TRUE),
                   na.action   = na.omit, 
                   data        = df)
        
        data.frame(
            Output        = c("Mean EMA fatigue averaged during usual care",
                              "Mean EMA fatigue averaged increased during yoga",
                              "Mean EMA fatigue averaged increased during massage"),
            Estimate      = c(round(summary(fit)$tTable[, 1], 2)),
            `p-value`     = c("-", round(summary(fit)$tTable[-1, 4], 2)))
    })
    
    sliderValues2 <- reactive({
        df <- filtereddata() %>%
            mutate(Treatment = factor(Assigned.Treatment, levels = c("Baseline", "Usual Care", "Yoga", "Massage")))
        
        df <- df %>%
            group_by(Date, Treatment) %>% 
            filter(Treatment != "Baseline")
        
        
        fit <- gls(model       = as.numeric(emas) ~ Treatment, 
                   correlation = corAR1(form = ~1),
                   subset      = which(Treatment != "Baseline"),
                   control     = list(singular.ok = TRUE),
                   na.action   = na.omit, 
                   data        = df)
        
        data.frame(
            Output        = c("Mean EMA stress averaged during usual care",
                              "Mean EMA stress averaged increased during yoga",
                              "Mean EMA stress averaged increased during massage"),
            Estimate      = c(round(summary(fit)$tTable[, 1], 2)),
            `p-value`     = c("-", round(summary(fit)$tTable[-1, 4], 2)))
    })
    
    # Insert the right number of plot output objects into the web page
    output$plots <- renderUI({
        plot_output_list <- lapply(1:length(input$outcome), function(i) {
            plotname <- paste("plot", i, sep="")
            plotOutput(plotname, height = 500, width = 750)
        })
        
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, plot_output_list)
    })
    
    ##wrapping the, for me strange(but nice) for-local-loop, seems to restore normal shiny reactive behavior
    observeEvent({c(input$outcome, input$dataset$datapath, input$treatment, input$header, 
                    input$sep, input$patient, input$date)},{
        mp = length(input$outcome)
        
        req(input$dataset)
        data <- read.csv(file   = input$dataset$datapath, 
                         header = input$header,
                         sep    = input$sep)
        
        
        data <- data[data$Id == input$patient, colnames(data) %in% c(input$date, input$treatment, input$outcome)]
        
        data <- data %>% 
            mutate_at(input$outcome, as.numeric) %>%
            mutate(Treatment = factor(input$treatment, levels = c("Baseline", "Usual Care", "Yoga", "Massage")))
            #group_by(input$date, Treatment) %>% 
        
        df <- data[, colnames(data) %in% c(input$outcome)]
        
        for (i in colnames(df)) {
            # Need local so that each item gets its own number. Without it, the value
            # of i in the renderPlot() will be the same across all instances, because
            # of when the expression is evaluated.
            local({
                plotname <- paste("plot", i, sep="")
                
                output[[plotname]] <- renderPlot({
                    ggplot(data = data, aes_string(x = "Date", y = i)) + 
                        geom_point() +
                        geom_line(aes(group = 1)) 
                    
                    
                    
                    # renderPlot({
                    # plot(df[, i], df[, i],
                    #      xlim = c(0, 20),
                    #      ylim = c(0, 20),
                    #      main = paste(names(df)[i], sep = "")
                    # )
                })
            })
        }
        
    })
    
    
    
    output$mytable  <- renderDataTable(filtereddata())
    
    # Show the values in an HTML table ----
    output$values <- renderDataTable({
        sliderValues() 
    }, extensions = 'Buttons', 
    options = list(
        initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')), rownames= FALSE)
    
    output$values1 <- renderDataTable({
        sliderValues1() 
    }, extensions = 'Buttons', 
    options = list(
        initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')), rownames= FALSE)
    
    output$values2 <- renderDataTable({
        sliderValues2() 
    }, extensions = 'Buttons', 
    options = list(
        initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')), rownames= FALSE)
    
    
})
