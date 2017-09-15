#funBody
funBody({
  
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
  return (dashboardBody(
    tabItems(tab1, tab2, tab3),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"))
    )
  )  
})