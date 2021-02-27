#install.packages("shiny")
# install.packages("DT")
source("test.R")
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(xlsx)
library(rvest)
library(stringr)
library(tidytext)
library(tidyverse)
library(tm)
library(zoo)
library(xlsx)
library(scales)
library(forecast)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("Economic Policy Uncertainty Index"),
  sidebarLayout(
    sidebarPanel(
      helpText("Choix du pays"),
      selectInput("var", 
                  label = "Choisir une variable à afficher",
                  choices = list("France", 
                                 "Royaume-Uni",
                                 "Global"),
                  selected = "France"),
       dateRangeInput("dateRange",
                      label = "Intervalle de temps : ",
                      format = "mm/yyyy",
                      language="fr",
                      start  = "2018-01-01",
                      end    = Sys.Date(),
                      startview = "year",
                      separator = " - "),
    ),
    mainPanel(
      h4(textOutput("date"),style="color:red"),
      #plotlyOutput("uk"),
      plotlyOutput("p"),
      textOutput("selected_var"),
      textOutput("selected_dateRange2"),
      plotOutput("forecast")
      )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
   output$p <- renderPlotly({
     date1 <- input$dateRange[1]
     date2 <- input$dateRange[2]
     pays <- input$var
     if(pays == "Royaume-Uni" ){
       p <- graph_uk(date1, date2)
     }else if(pays == "France"){
       p <- graph_fr(date1, date2)
     }else{
       p <- graph_global(date1, date2)
     }
   })

   output$date <- renderText({ 
     if(input$dateRange[1] < "2018-01-01"){
       paste("L'intervalle choisi n'existe pas : ", input$dateRange[1], " - ", input$dateRange[2],". La date minimum est : 2018-01")
     }
   })
   
   output$forecast <- renderPlot({
     date1 <- input$dateRange[1]
     date2 <- input$dateRange[2]
     pays <- input$var
     forecast <- accurancy(pays, date1, date2)
   })

}

shinyApp(ui = ui, server = server)


