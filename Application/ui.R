library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(pageWithSidebar(
    
    headerPanel("City Analytics"),
    sidebarPanel(
        # First Sidebar
        conditionalPanel(condition="input.tabselected==1",
                         h4("City"),
                         selectInput("MultiCitySelector",label = NULL, multiple=TRUE, choices=c("None"), selected=NULL),
                         h4("Feature"),
                         selectInput("FeatureSelector",label = NULL, choices=c("Price", "Availability/Month", "Revenue/Month"), selected=NULL),
                         h4("Date Range"),
                         dateRangeInput('DateRangeComparator', label = NULL, start = Sys.Date() - 3, end = Sys.Date() + 3, format = "dd/mm/yyyy"),
                         h4("Comparator"),
                         selectInput("Comparator",label = NULL, choices=c("None","Num/Bedrooms", "Room Type"), selected=NULL),
                         h4("Plot Type"),
                         selectInput("PlotSelector",label = NULL, choices=c("Box Distibution", "Bar Mean", "Bar Median"), selected=NULL)),
        
        
        # Second Sidebar
        conditionalPanel(condition="input.tabselected==2",
                         h4("City"),
                         selectInput("CitySelector",label = NULL, choices=c("None"), selected=NULL),
                         h4("Plot Type"),
                         selectInput("PlotCondition",label = NULL, choices=c("Proportion", "Distribution", "Average"), selected=NULL),
                         h4("Date Range"),
                         dateRangeInput("DateRangeSelector", label = NULL, start = Sys.Date() - 3, end = Sys.Date() + 3, format = "dd/mm/yyyy"),
                         conditionalPanel(condition="input.PlotCondition!='Proportion'", h4("Feature"),
                         selectInput("FeatureCondition",label = NULL, choices=c("Price", "Availability/Month", "Revenue/Month"), selected=NULL)),
                         h4("Comparator"),
                         selectInput("ComparatorCondition",label = NULL, choices=c("None","Num/Bedrooms", "Room Type", "Neighbourhood"), selected=NULL),
                         
        )),
    
    mainPanel(

        
        tabsetPanel(
            #First Tab
            tabPanel("City Comparator", value=1, plotOutput("comparator")),
            # Second Tab
            tabPanel("City Insights", value=2, plotOutput("insight"), leafletOutput("map")),
            # Id of the Tabset Panel
            id = "tabselected"
        )
    )
))