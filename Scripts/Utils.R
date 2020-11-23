library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
library(stringr)


# A function used to create a dataframe containing the country / region / City / date / listing_url
create_city_date_dataframe <- function()
{
    # Retrieve the dataframe containing the urls of the differents databases
    database_urls <- read.csv(file.path("Data/urls.csv"))
    features <- str_split_fixed(database_urls$listings_data_url, "/", 8)
    #Getting data from URL
    database_urls$country = subset(features, select = c(4))
    database_urls$region = subset(features, select = c(5))
    database_urls$city = subset(features, select = c(6))
    database_urls$date = as.Date(subset(features, select = c(7)))
    database_urls = subset(database_urls, select = c("country", "region", "city", "date"))
    
    # Keep only our countries
    database_urls<-database_urls %>% subset((country =="france" & city != "paris")  | country =="germany" | country =="spain" | country =="ireland")
    # keep only the required dates
    database_urls <- database_urls %>% group_by(city) %>% slice_max(order_by = date, n = 3)
    # Fix col names
    colnames(database_urls) <- c("country", "region", "city", "date")
    
    print("Database Loaded Succesfully")
    return(database_urls)
}

#Load a listing of a given city and date and return the append it to the dataframe
load_listing_into <- function(x, dataframe)
{
    # Getting appropriate web url
    web_url <- paste("http://data.insideairbnb.com/",x[1],"/",x[2],"/",x[3],"/",x[4],"/data/listings.csv.gz",sep="")
    print(paste("Downloading:",web_url))
    con <- gzcon(url(web_url))
    txt <- readLines(con)
    # Read the dataframe
    listing = read.csv(textConnection(txt))
    
    ## Add Keys: columns city and day date
    listing$country <- x[1]
    listing$region <- x[2]
    listing$city <- x[3]
    listing$date <- x[4]
    
    
    listing <- listing %>%  select(colnames(dataframe)) %>%  arrange(id)
    #???Finally Merge the dataset
    return(listing)
    
}

# a generic function to prepare data for a specific city, data_date
create_clean_dataset <- function(country, region, city, date)
{
    ## Defining the columns of the dataset
    columns_listings <- c( "id","country", "region","city", "date", "neighbourhood_cleansed", 
                           "latitude", "longitude", "room_type", "accommodates", "bedrooms",
                           "availability_30", "price")
    
    # Create empty dataframe 
    dataframe <- data.frame(matrix(ncol = 13, nrow = 0))
    colnames(dataframe) <- columns_listings
    
    # Retrieve the urls
    database_url = create_city_date_dataframe()

    # For each city and date
    dataframe <- do.call("rbind", apply(database_url, 1,load_listing_into, dataframe))
    
    
    # Retrieve the revenue
    dataframe$price <- as.numeric(str_remove(dataframe$price, "[$]"))
    dataframe$availability_30 <- as.numeric(dataframe$availability_30)
    dataframe$revenue_30 <- ((30-dataframe$availability_30) * dataframe$price)
    dataframe$revenue_30[is.na(dataframe$revenue_30)] <- 0
    dataframe$availability_30[is.na(dataframe$availability_30)] <- 30
    # Fix Bedrooms 
    dataframe$bedrooms <- as.numeric(dataframe$bedrooms)
    dataframe$bedrooms <- ifelse(dataframe$bedrooms >= 4, "4+", dataframe$bedrooms)
    dataframe <- na.omit(dataframe)
    # Fix Shared Room
    available_room_types = c("Entire home/apt", "Hotel room", "Private room", "Shared room")
    dataframe <- subset(dataframe, dataframe$room_type %in% available_room_types)
    
    return(dataframe)
}

# Set Working Directory
#setwd("D:/Documents/GitHub/R-Project/Application")
# Let's load the data
#dataframe <- create_clean_dataset()

# Put it in the data frame
#print("Starting to Write")
#write.csv(dataframe, "Data/clean_data.csv")
