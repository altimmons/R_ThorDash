
library(shiny)
library(shinydashboard)

funSidebar({
  t1s <-menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
  t2s <-  menuItem("Widgets", tabName = "widgets", icon = icon("plus-square"), menuSubItem("testone", tabName = "sub1", icon = icon("plus-square-o")))
  t3s <- menuItem("Charts", tabName = "charts", icon = icon("graph"))
  
  
      return( dashboardSidebar(
        #id = "sidebarMenu" , #For Restoring the sidebar? Not working
        sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                          label = "Search..."),
        sidebarMenu(t1s, t2s, t3s))
      )
})
