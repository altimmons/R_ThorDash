# app.R

# This is an initial look at the shiny/shiny dashboard environments
# mostly a testing and learning environment.
#
# Change Log
# 9/14 - Initial build
# 9/15 - working on separating out functions into other files, initally doesnt work, return
# to build a rich graphing environment, adding shinyjs
#

library(shiny) 
library(shinydashboard) 
library(ggplot2) 
#library("ggplot2", lib.loc="~/ThorDashboard/packrat/lib/x86_64-w64-mingw32/3.4.1")
library(shinyjs, lib.loc="~/ThorDashboard/packrat/lib/x86_64-w64-mingw32/3.4.1")
library(jsonlite)


dataset <- as.data.frame("","")
datasets <- c("", "diamonds", "iris", "mtcars", "faithful", "ldeaths", "mdeaths", "fdeaths", "freeny", "sleep")

# tabs
{
t1s <-menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")) 
t2s <-  menuItem("Widgets", tabName = "widgets", icon = icon("plus-square")) #menuSubItem("testone", tabName = "sub1", icon = icon("plus-square-o"))) 
t3s <- menuItem("Charts", tabName = "charts", icon = icon("graph"))
t4s <- menuItem("New Charts", tabName = "flexchart", icon = icon("microchip"))
}

tab1 <- {tabItem(tabName = "dashboard", 
   title = ("Old Faithful Geyser Data"), 
   
   fluidRow(box(h2= ("Settings 123"),title = "testing")),
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
)} 
tab2 <- {tabItem(tabName = "widgets", 
          h2("Widgets tab content"), 
          shinyUI(
            navbarPage("My Application", 
            tabPanel("Component 1"), 
            tabPanel("Component 2"), 
            tabPanel("Component 3") 
            ) 
            ) 
)} 
tab3 <- {tabItem(tabName = "charts", 
                
                shinyUI(fluidPage( 
                  
                  title = "Diamonds Explorer", 
                  
                  fluidRow(box(plotOutput('plot'), height = 725, width = 12)), 
                  
                  hr(), 
                  
                  fluidRow( 
                    column(3, 
                           h4("Diamonds Explorer"), 
                           sliderInput('sampleSize', 'Sample Size',  
                                       min=1, max=nrow(diamonds), value=min(1000, nrow(diamonds)),  
                                       step=500, round=0), 
                           br(), 
                           checkboxInput('jitter', 'Jitter'), 
                           checkboxInput('smooth', 'Smooth') 
                    ), 
                    column(4, offset = 1, 
                           selectInput('x', 'X', names(diamonds)), 
                           selectInput('y', 'Y', names(diamonds), names(diamonds)[[2]]), 
                           selectInput('color', 'Color', c('None', names(diamonds))) 
                    ), 
                    column(4, 
                           selectInput('facet_row', 'Facet Row', c(None='.', names(diamonds))), 
                           selectInput('facet_col', 'Facet Column', c(None='.', names(diamonds))) 
                    ) 
                  ) 
                )) 
                
)}
tab4 <- {tabItem(tabName = "flexchart",
  title = "FlexChart",
  fluidRow(
    width = 12,
    tabBox(
      title = "Advanced Controls",
      id = "advcontrols",
      width = 12,
      tabPanel(
        id = "sourceTab",
        title = "Data Select",
        #fluidRow(
            selectizeInput('seldataset', 'Select a Dataset to Work With', datasets, selected = ""),
            #uiOutput('varname_x', "X Field"),
            #uiOutput('varname_y', "Y Field" )
            #width = 12,
            #offset = 0.5
            #),
        fluidRow(
          column(
            width = 3,
            offset = 1,
            selectInput('t4_x', 'X', character(0), width = 200)
          ),
          column(
            width = 3,
            offset = 0,
            selectInput('t4_y', 'Y (optional)', character(0), width = 200)
          ),
          column(
            width = 3,
            offset = 0,
            selectInput('t4_z', 'Z (optional)', character(0), width = 200)
          )
        )
          # 
          # div(style="display: inline-block;vertical-align:top; width: 150px;",
          #     selectInput('t4_x', 'X', character(0), width = 200)),
          # div(style="display: inline-block;vertical-align:top; width: 150px;",
          #     selectInput('t4_y', 'Y', character(0), width = 200)), 
          # div(style="display: inline-block;vertical-align:top; width: 150px;", 
          #     selectInput('t4_z', 'Z', character(0), width = 200)
          # )
        
      ),
      tabPanel(
        id = "geomstab",
        title =  "Select Geometry",
        fluidRow()
      ),
      tabPanel(
        id = "stats",
        title = "Select a Stat Function",
        fluidRow()
      ),
      tabPanel(
        id = "othertab",
        title = "Coords, Scales, Faceting"
      )
    )
  )
)}

dHeader <- {dashboardHeader( 
  title = "Thoracic Dashboard" 
) }

dSidebar <- dashboardSidebar( 
  #id = "sidebarMenu" , #For Restoring the sidebar? Not working 
  #sidebarSearchForm(textId = "searchText", buttonId = "searchButton", 
   #                 label = "Search..."),                              #search function
  sidebarMenu(t1s, t2s, t3s, t4s) 
  
) 


dBody <- dashboardBody( 
  tabItems(tab1, tab2, tab3, tab4), shinyjs::useShinyjs()) 

ui <- dashboardPage(dHeader, dSidebar, dBody) 


#######################################################################################################
server <-shinyServer(function(input, output, session) { 
  
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
  
  ##ggplot page
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
 
  observeEvent( input$seldataset, {
    #toggleState("t4_x", condition = (input$seldataset != "") )
    #toggleState("t4_y", condition = (input$seldataset != "") )
    
    if(input$seldataset != ""){
      dataset <- get(input$seldataset)
      shinyjs::enable("t4_x")
      shinyjs::enable("t4_y")
      shinyjs::enable("t4_z")
      updateSelectInput(session, "t4_x", choices = names(dataset))
      updateSelectInput(session, "t4_y", choices = c("[none]", names(dataset)))
      updateSelectInput(session, "t4_z", choices = c("[none]", names(dataset)))
    } else {
      dataset <- ""
      updateSelectInput(session, "t4_x", choices = character(0))
      updateSelectInput(session, "t4_y", choices = character(0))
      updateSelectInput(session, "t4_z", choices = character(0))
      shinyjs::disable("t4_x")
      shinyjs::disable("t4_y")
      shinyjs::disable("t4_z")
    }
})
# calls from search button input$searchText input$searchButton 
}) 

shinyApp(ui, server)

# 
# if (interactive()) {
# 
# ui <- fluidPage(
#   p("The checkbox group controls the select input"),
#   checkboxGroupInput("inCheckboxGroup", "Input checkbox",
#     c("Item A", "Item B", "Item C")),
#   selectInput("inSelect", "Select input",
#     c("Item A", "Item B", "Item C"))
# )
# 
# server <- function(input, output, session) {
#   observe({
#     x <- input$inCheckboxGroup
# 
#     # Can use character(0) to remove all choices
#     if (is.null(x))
#       x <- character(0)
# 
#     # Can also set the label and select items
#     updateSelectInput(session, "inSelect",
#       label = paste("Select input label", length(x)),
#       choices = x,
#       selected = tail(x, 1)
#     )
#   })
# }
# 
# shinyApp(ui, server)
# }