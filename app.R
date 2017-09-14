
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

dataset <- diamonds

t1s <-menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
t2s <-  menuItem("Widgets", tabName = "widgets", icon = icon("plus-square")) #menuSubItem("testone", tabName = "sub1", icon = icon("plus-square-o")))
t3s <- menuItem("Charts", tabName = "charts", icon = icon("graph"))
           
tab1 <-tabItem(tabName = "dashboard",
          title = ("Old Faithful Geyser Data"),
          h2= ("Settings 123"),
          fluidRow(
            tabBox(
              #status = "warning",
              title = "controls",
              id = "selected_panel",
              width = 12,
              tabPanel("input", "Selector 1", sliderInput("bins", "Number of observations", 1,100,50)),
              tabPanel("tab2", "Selector 2", sliderInput("slider2", "No. of Somethings", 1,100,23)),
              tabPanel("tab3", h2("Empty"), checkboxInput("blnSolid", "make info box solid", FALSE)))
              #box(drop)
            ),
          fluidRow(
            box(
              title = "This Sample Chart",  
              h3 = "Histogram",
              status = "primary", 
              plotOutput("distPlot"), 
              width = 12,
              solidHeader = TRUE,
              collapsible = TRUE
              )
          ),
          fluidRow(
              # A static infoBox
              infoBoxOutput("values"),
              # Dynamic infoBoxes
              infoBoxOutput("progressBox")
              #infoBoxOutput("approvalBox")
          )
          )

tab2 <- tabItem(tabName = "widgets",
           h2("Widgets tab content"),
           shinyUI(navbarPage("My Application",
                              tabPanel("Component 1"),
                              tabPanel("Component 2"),
                              tabPanel("Component 3")
           ))
        )

tab3 <- tabItem(tabName = "charts",
                h1("ggPlot Charts"),
                
                shinyUI(fluidPage(
                  
                  title = "Diamonds Explorer",
                  
                  fluidRow(box(plotOutput('plot'), height = 725, width = 12)),
                  
                  hr(),
                  
                  fluidRow(
                    column(3,
                           h4("Diamonds Explorer"),
                           sliderInput('sampleSize', 'Sample Size', 
                                       min=1, max=nrow(dataset), value=min(1000, nrow(dataset)), 
                                       step=500, round=0),
                           br(),
                           checkboxInput('jitter', 'Jitter'),
                           checkboxInput('smooth', 'Smooth')
                    ),
                    column(4, offset = 1,
                           selectInput('x', 'X', names(dataset)),
                           selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
                           selectInput('color', 'Color', c('None', names(dataset)))
                    ),
                    column(4,
                           selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
                           selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
                    )
                  )
                ))
                
                
                )


dHeader <- dashboardHeader(
  title = "Thoracic Dashboard"
)

dSidebar <- dashboardSidebar(
  #id = "sidebarMenu" , #For Restoring the sidebar? Not working
  sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                    label = "Search..."),
  sidebarMenu(t1s, t2s, t3s)
  
)


dBody <- dashboardBody(
  tabItems(tab1, tab2, tab3))

ui <- dashboardPage(dHeader, dSidebar, dBody)

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