#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(nlme)
library(DT)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("CLBP Patient Report"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("dataset", "Choose CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            # Include clarifying text ----
            #helpText(em("Note: This app requires file in csv format only!!")),
            helpText(em("Note:Select all the inputs and click on button as given below to exectute the app")),
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            selectInput("date", "Select date column", "Nothing"),
            selectInput("treatment", "Select treatment column", "Nothing"),
            selectInput("outcome", "Select outcomes (can be multiple)", "Nothing", multiple = TRUE),
            selectInput("patient", "Select patient", "Upload data!"),
            actionButton("update", "Update Data set", class = "btn-primary",style='padding:4px; font-size:120%')
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            h2('Data'),
            #tableOutput("mytable")
            DT::dataTableOutput("mytable"), 
            
            h5("AR(1) model analysis with treatment effects"),
            dataTableOutput("values"),
            
            h5("AR(1) model analysis with treatment effects"),
            dataTableOutput("values1"), 
            
            h5("AR(1) model analysis with treatment effects"),
            dataTableOutput("values2"), 
            
            uiOutput("plots")
            
        )
        )
    )
)

