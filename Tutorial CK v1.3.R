
# Set Up Libraries --------------------------------------------------------

x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap")
# install.packages(x) # warning: uncommenting this may take a number of minutes
lapply(x, library, character.only = TRUE) # load the required packages

rm(x)


library(OpenStreetMap)
library(leaflet)


# Part 1 ------------------------------------------------------------------

# Read in the shape file
lnd <- readOGR(dsn = "data/london_sport.shp")

# Look at classes of the data slot
sapply(lnd@data, class)


# Change the class of Pop_2001 from dactor to numeric
lnd$Pop_2001 <- as.numeric(lnd$Pop_2001)




# Basic Plotting ----------------------------------------------------------

# Basic plot
plot(lnd)

# We can subset data for regions with participation higher than 25%
sel <- lnd$Partic_Per > 25

# plotting this subset will only display these regions
plot(lnd[sel,])

# In order to display these with the other regions we need to do this:
plot(lnd, col = "lightgrey") 
plot(lnd[ sel, ], col = "turquoise", add = TRUE) 

rm(sel)


# setting and transforming CRS --------------------------------------------

proj4string(lnd)
proj4string(lnd) <- NA_character_ # remove CRS information from lnd
proj4string(lnd) <- CRS("+init=epsg:27700") # assign a new CRS



# EPSG <- make_EPSG() # create data frame of available EPSG codes
# EPSG[grepl("WGS 84$", EPSG$note), ] # search for WGS 84 code 
# 
# rm(EPSG)


# # Reproject data into correct CRS ()
# lnd84 <- spTransform(lnd, CRS("+init=epsg:4326")) # reproject
# 
# coordinates(gCentroid(lnd84))
# 
# # Save lnd84 object (we will use it in Part IV)
# saveRDS(object = lnd84, file = "data/lnd84.Rds")
# 
# rm(lnd84)









# Attribute joins ---------------------------------------------------------


# Create and look at new crime_data object
crime_data <- read.csv("data/mps-recordedcrime-borough.csv",
                       stringsAsFactors = FALSE)

head(crime_data$CrimeType) # information about crime type

# Extract "Theft & Handling" crimes and save
crime_theft <- crime_data[crime_data$CrimeType == "Theft & Handling", ]
head(crime_theft, 2) # take a look at the result (replace 2 with 10 to see more rows)



# Calculate the sum of the crime count for each district, save result
crime_ag <- aggregate(CrimeCount ~ Borough, FUN = sum, data = crime_theft)
# Show the first two rows of the aggregated crime data
head(crime_ag, 2)


# Compare the name column in lnd to Borough column in crime_ag to see which rows match.
lnd$name %in% crime_ag$Borough


# Return rows which do not match
lnd$name[!lnd$name %in% crime_ag$Borough]

crime_ag$Borough[!crime_ag$Borough %in% lnd$name]





# Find common columns
head(lnd$name) # dataset to add to (results not shown)
head(crime_ag$Borough) # the variables to join


# head(left_join(lnd@data, crime_ag)) # test it works
lnd@data <- left_join(lnd@data, crime_ag, by = c('name' = 'Borough'))



head(lnd@data)



qtm(lnd, "CrimeCount") # plot the basic map






# Clipping and spatial joins ----------------------------------------------




# create new stations object using the "lnd-stns" shapefile.
stations <- readOGR(dsn = "data/lnd-stns.shp")
# stations = read_shape("data/lnd-stns.shp") # from tmap

proj4string(stations) # this is the full geographical detail.
proj4string(lnd) # what's the coordinate reference system (CRS)

bbox(stations) # the extent, 'bounding box' of stations
bbox(lnd) # return the bounding box of the lnd object



# Create reprojected stations object
stations <- spTransform(stations, CRSobj = CRS(proj4string(lnd)))
plot(lnd) # plot London 
points(stations) # overlay the station points



stations <- stations[lnd, ] 
plot(stations) # test the clip succeeded




# Part IV: Making maps with tmap, ggplot2 and leaflet ---------------------



# tmap --------------------------------------------------------------------



vignette("tmap-nutshell")



qtm(shp = lnd, fill = "Partic_Per", fill.palette = "-Blues") # not shown

qtm(shp = lnd, fill = c("Partic_Per", "Pop_2001"), fill.palette = "Blues", ncol = 2) 




tm_shape(lnd) +
  tm_fill("Pop_2001", thres.poly = 0) +
  tm_facets("name", free.coords = TRUE, drop.units = TRUE)



# Transform the coordinate reference system
lnd_wgs = spTransform(lnd, CRS("+init=epsg:4326"))

if(curl::has_internet()) {
  
  osm_tiles = tmaptools::read_osm(bbox(lnd_wgs)) # download images from OSM
  
  tm_shape(osm_tiles) + tm_raster() +
    tm_shape(lnd_wgs) +
    tm_fill("Pop_2001", fill.title = "Population, 2001", scale = 0.8, alpha = 0.5) +
    tm_layout(legend.position = c(0.89, 0.02)) 
  
} else {
  tm_shape(lnd_wgs) +
    tm_fill("Pop_2001", fill.title = "Population, 2001", scale = 0.8, alpha = 0.5) +
    tm_layout(legend.position = c(0.89, 0.02))
}


# The code above did not work to download a backgroung from OSM
# This is an alternative with leaflet

tmap_mode("view")


tm_shape(lnd_wgs) +
  tm_fill("Pop_2001", fill.title = "Population, 2001", scale = 0.8, alpha = 0.5) +
  tm_layout(legend.position = c(0.89, 0.02))






# ggmap -------------------------------------------------------------------



lnd@data$Pop_2001 <- as.numeric(lnd@data$Pop_2001)

p <- ggplot(lnd@data, aes(Partic_Per, Pop_2001))

p + geom_point(aes(colour = Partic_Per, size = Pop_2001)) +
    geom_text(size = 2, aes(label = name))



# To plot in ggplot we need a df/tibble which we can create with broom::tidy()

lnd_f <- broom::tidy(lnd)


head(lnd_f)

# This has resulted in us losing the additional data we had so we will need to re-join

head(lnd_f, n = 2) # peak at the fortified data

lnd$id <- row.names(lnd) # allocate an id variable to the sp data

head(lnd@data, n = 2) # final check before join (requires shared variable name)

lnd_f <- left_join(lnd_f, lnd@data) # join the data



map <- ggplot(lnd_f, aes(long, lat, group = group, fill = Partic_Per)) +
              geom_polygon() + 
              coord_equal() +
              labs(x = "Easting (m)", 
                   y = "Northing (m)",
                   fill = "% Sports\nParticipation") +
              ggtitle("London Sports Participation")

map



map + scale_fill_gradient(low = "white", high = "black")






# Leaflet -----------------------------------------------------------------




lnd84 <- readRDS('data/lnd84.Rds')

leaflet() %>%
  addTiles() %>%
  addPolygons(data = lnd84)



leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addPolygons(data = lnd84, color = "red", fillColor = "pink", opacity = 0.8, fillOpacity = 0.5)





# Advanced Task: Faceting for Maps ----------------------------------------

london_data <- read.csv("data/census-historic-population-borough.csv")

ltidy <- gather(london_data, date, pop, -Area.Code, -Area.Name)

head(ltidy)



lnd_f <- broom::tidy(lnd)

head(lnd_f, 2)



ltidy <- rename(ltidy, ons_label = Area.Code) # rename Area.code variable


lnd_f <- left_join(lnd_f, ltidy)


lnd_f$date <- gsub(pattern = "Pop_", replacement = "", lnd_f$date)




ggplot(data = lnd_f, # the input data
       aes(x = long, y = lat, fill = pop/1000, group = group)) + # define variables
  geom_polygon() + # plot the boroughs
  geom_path(colour="black", lwd=0.05) + # borough borders
  coord_equal() + # fixed x and y scales
  facet_wrap(~ date) + # one plot per time slice
  scale_fill_gradient2(low = "blue", mid = "grey", high = "red", # colors
                       midpoint = 150, name = "Population\n(thousands)") + # legend options
  theme(axis.text = element_blank(), # change the theme options
        axis.title = element_blank(), # remove axis titles
        axis.ticks = element_blank()) # remove axis ticks









