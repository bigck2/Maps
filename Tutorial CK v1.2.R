



# Set Up Libraries --------------------------------------------------------

library(purrr)

x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap")
# install.packages(x) # warning: uncommenting this may take a number of minutes
lapply(x, library, character.only = TRUE) # load the required packages

rm(x)

library(leaflet)





# Part 1 ------------------------------------------------------------------

# Read in the shape file
lnd <- readOGR(dsn = "data/london_sport.shp")

# Change the class of Pop_2001 from dactor to numeric
lnd$Pop_2001 <- as.numeric(lnd$Pop_2001)

# Look at the CRS
lnd@proj4string


EPSG <- make_EPSG() # create data frame of available EPSG codes
EPSG[grepl("WGS 84$", EPSG$note), ] # search for WGS 84 code 


# Reproject data into correct CRS ()
lnd <- spTransform(lnd, CRS("+init=epsg:4326")) # reproject


rm(EPSG)






# Attribute joins ---------------------------------------------------------


# Create and look at new crime_data object
crime_data <- read.csv("data/mps-recordedcrime-borough.csv",
                       stringsAsFactors = FALSE)

# Extract "Theft & Handling" crimes and save
crime_theft <- crime_data[crime_data$CrimeType == "Theft & Handling", ]

# Calculate the sum of the crime count for each district, save result
crime_ag <- aggregate(CrimeCount ~ Borough, FUN = sum, data = crime_theft)


# complete the join
lnd@data <- left_join(lnd@data, crime_ag, by = c('name' = 'Borough'))

rm(crime_data, crime_theft)




# ggmap -------------------------------------------------------------------



# To plot in ggplot we need a df/tibble which we can create with broom::tidy()
lnd_f <- broom::tidy(lnd)



# This has resulted in us losing the additional data we had so we will need to re-join

# add a common column with which to join
lnd$id <- row.names(lnd) # allocate an id variable to the sp data


# join the data
lnd_f <- left_join(lnd_f, lnd@data) 



map <- ggplot(lnd_f, aes(long, lat, group = group, fill = Partic_Per)) +
              geom_polygon() + 
              coord_map() + 
              labs(x = "Easting (m)", 
                   y = "Northing (m)",
                   fill = "% Sports\nParticipation") +
              ggtitle("London Sports Participation")

map



#download london map
london_map <- get_map(location = "London", zoom = 11, maptype = "road" )

ggmap(london_map)

ggmap(london_map) +
  coord_map() + 
  geom_polygon(data = lnd_f, 
               aes(x = long, y = lat, group = group, fill = Partic_Per)) +
  scale_fill_gradient()




map + scale_fill_gradient(low = "white", high = "black")






















# Leaflet -----------------------------------------------------------------


leaflet() %>%
  addTiles() %>%
  addPolygons(data = lnd84)




leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addPolygons(data = lnd84, color = "red", fillColor = "pink", opacity = 0.8, fillOpacity = 0.5)
















