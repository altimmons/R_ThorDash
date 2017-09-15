
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
source("Sidebar.R")
source("Body.R")

dataset <- diamonds


#dHeader <- dashboardHeader(
#  title = "Thoracic Dashboard"
#)

#Sidebar <- funSidebar()

#dBody <- funBody()

ui <- dashboardPage(funHeader(), funSidebar(), funBody())

server <-shinyServer(function(input, output) {
  
      output$distPlot <- renderPlot({
        x    <- faithful[, 2] 
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        hist(x, breaks = bins, col = 'darkgray', border = 'white', ylim = c(0, input$slider2) )
      })
      
      output$progressBox <- renderInfoBox({
        infoBox("Selected Tab", input$selected_panel, icon = icon("list"), fill = input$blnSolid)
      })
      
      output$values <- renderInfoBox({
          infoBox("Parameters",
          h4(paste0("Obs = ", input$bins)),
          h4(paste0("Ylim = ", input$slider2))
          )
        })
          
      dataset <- reactive({diamonds[sample(nrow(diamonds), input$sampleSize),]  })
          
          output$plot <- renderPlot({
            p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()
            
            if (input$color != 'None')
              p <- p + aes_string(color=input$color)

            facets <- paste(input$facet_row, '~', input$facet_col)
            
            if (facets != '. ~ .')
              p <- p + facet_grid(facets)
            
            if (input$jitter)
              p <- p + geom_jitter()
            
            if (input$smooth)
              p <- p + geom_smooth()
            
            print(p)
            
            
            
          }, height=700)
          
          
          
        }
        

      
      # calls from search button input$searchText input$searchButton
      
)

shinyApp(ui, server) 