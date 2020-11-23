# Importing Libraries
library(shiny)
library(leaflet)

# Import Utils Scripts & Functions
source('Script/Utils.R')

# Retieve available data
database_city_date <- create_city_date_dataframe()
# retrieve cities
cities <- unique(database_city_date$city)

# Retrieve the dara cleaned previously with Utils script
database <- read.csv("Data/clean_data.csv")

mindate <- min(database$date)
maxdate <- max(database$date)

# Create a Comparison plot
make_comparator_plot <- function(dataset, feature, comparator, plot_type) {
  # Init Plot
  p <- ggplot(dataset, aes_string(x="city", y=feature, fill=comparator,color=comparator))
  # Init Title
  title <- ""
  
  # Plot
  if(plot_type == "box"){
    p<-p+geom_boxplot()
    title <- "Distribution of"
    }
  else if(plot_type == "barmean"){
    p<-p+geom_bar( stat = "summary", fun="mean", position = position_dodge(preserve = 'single'))
    title <- "Average"
    }
  else if(plot_type == "barmed"){
    p<-p+geom_bar( stat = "summary", fun="median", position = position_dodge(preserve = 'single'))
    title <- "Median"
    }
  
  # Feature special ploting parameters
  if(feature == "revenue_30"){ p <- p }
  else if(feature == "price"){ p <- p + coord_cartesian(ylim=c(0,500))}
  else if(feature == "availability_30"){ p <- p + coord_cartesian(ylim=c(0,31))}
  
  # Set Title  
  title <- paste(title, feature)
  if(!is.null(comparator)){title<-paste(title, "for each", comparator)}
  p<-p + ggtitle(title)
  
  return(p)
}

# Create a Pie Proportion Plot
make_proportion_plot <- function(dataset, comparator){
    # Init Plot
    p <- ggplot(dataset, aes_string(x=factor(1), fill=comparator))+
      geom_bar(width = 1)+
      coord_polar("y") + theme_void() 
    if(comparator == "neighbourhood_cleansed"){p <- p + ggtitle("Proportion of each Neighbourhood")}
    else if(comparator == "room_type"){p <- p + ggtitle("Proportion of each Room Types")}
    else if(comparator == "bedrooms"){p <- p + ggtitle("Proportion of each Bedrooms amount")}
    
    return(p)
}


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # Update the ui in function of the available cities
  updateSelectizeInput(session, 'MultiCitySelector', choices = cities, server = TRUE)
  updateSelectizeInput(session, 'CitySelector', choices = cities, server = TRUE)
  updateDateRangeInput(session, 'DateRangeComparator', start=mindate, end=maxdate, min=mindate, max=maxdate)
  updateDateRangeInput(session, 'DateRangeSelector', start=mindate, end=maxdate, min=mindate, max=maxdate)
  
  # The Comparator Plot
  output$comparator <- renderPlot({

    # Retrieve the cities
    cities <- input$MultiCitySelector
    # Retrieve the correct feature
    feature <- switch(input$FeatureSelector, "Price" = "price", "Availability/Month" = "availability_30", "Revenue/Month" = "revenue_30")
    # Retrieve the correct comparator
    comparator <- switch(input$Comparator, "None" = NULL,"Num/Bedrooms"="bedrooms", "Room Type"= "room_type")
    # Retrive the desired plot
    plot_type <- switch(input$PlotSelector, "Box Distibution" = "box", "Bar Mean"="barmean", "Bar Median"="barmed")
    # Retrieve the date
    dateMin <- input$DateRangeComparator[1]
    dateMax <- input$DateRangeComparator[2]
    
    # Retrieve the subdataset for selected cities
    subdataset <- subset(database, database$city %in% cities)
    subdataset <- subset(subdataset, subdataset$date >= dateMin & subdataset$date <= dateMax)
    
    # Generate a box plot
    make_comparator_plot(subdataset,feature, comparator, plot_type)
  })
  
  # The Insight Plot
  output$insight <- renderPlot({
    
    #Retrieve city
    city <- c(input$CitySelector)
    # Retrieve the correct feature
    feature <- switch(input$FeatureCondition, "Price" = "price", "Availability/Month" = "availability_30", "Revenue/Month" = "revenue_30")
    # Retrieve the correct comparator
    comparator <- switch(input$ComparatorCondition, "None" = NULL,"Num/Bedrooms"="bedrooms", "Room Type"= "room_type", "Neighbourhood"="neighbourhood_cleansed")
    # Retreive the desired plot
    plot_type <- switch(input$PlotCondition, "Proportion" = "pie", "Distribution"="box", "Average"="barmean")
    # Retrieve the date
    dateMin <- input$DateRangeSelector[1]
    dateMax <- input$DateRangeSelector[2]
    
    # Retrieve the subdataset for selected cities
    if(city != "None"){
        subdataset <- database[database$city == city,]
        subdataset <- subset(subdataset, subdataset$date >= dateMin & subdataset$date <= dateMax)
        print(head(subdataset))
    }
    
    # Make adequate plot
    if(plot_type == "pie" && !is.null(comparator)) {
        make_proportion_plot(subdataset, comparator)
    } else if(plot_type != "pie"){
        # Generate a box plot
        make_comparator_plot(subdataset,feature, comparator, plot_type)
    }
  })
  
  # The map Plot
  output$map <- renderLeaflet({
    #Retrieve city
    city <- c(input$CitySelector)
    if(city != "None"){
        df <- subset(database, city == input$CitySelector)
        leaflet() %>% addTiles() %>%
        #addPolylines(df, lng = df$long, lat = df$lat, col = "grey", opacity = 1)%>%
        addMarkers(data = df, lng = ~longitude, lat = ~latitude, clusterOptions = markerClusterOptions())
    }
  })
  
})
