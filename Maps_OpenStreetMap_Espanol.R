library(tidyverse)
library(OpenStreetMap)
library(readr)
library(stringr)

# Map ---------------------------------------------------------------------


#Set the limites. You can do it in a simple way, or with a matrix

#Simple way
#lat1 <- 39.2884; lat2 <- 39.3338; long1 <- -0.5786; long2 <- -0.5025
lat1 <- 39.2914; lat2 <- 39.3271; long1 <- -0.5847; long2 <- -0.5097

#Now open the map. Notice that Lat1 goes with Long1, and viceversa.
TelloMap <- openmap(c(lat1, long1), c(lat2, long2), zoom = 15,
                    type = "bing", mergeTiles = TRUE)

#Reproject the map on WGS84
#TelloMap <- openproj(TelloMap)

#This autoplot.OpenStreetMap() is equal to ggplot2()
TelloMapPlot <- autoplot.OpenStreetMap(TelloMap)

#Now we have a decent map in WGS84 of our area of study. But if we want
#to draw on it, we need to use "bialowieza", no "BialowiezaPlot"
TelloMapPlot

# Data --------------------------------------------------------------------

#There are waypoints from diferent days. We downloaded this data from a My Maps data, but
#you can create this type of dataframe with any kind of data. You only need
#3 columns: name, latitude and longitude

#Importing data
waypointscamtrap <- read_csv("C:/Users/josem/Documents/R/Github/Mapas/Mapping-Camtraps-with-OpenStreenMap/Data/CamtrapWaypoints.csv")

#We need to separate first column in 4, and then select only latitud and long
waypointscamtrap[c('wk', 'long', 'lat', 'z')] <- str_split_fixed(waypointscamtrap$WKT, ' ', 4)

head(waypointscamtrap)

waypointscamtrap <- select(waypointscamtrap, nombre, descripción, long, lat)

#Deleting every ( of the long column
waypointscamtrap <- waypointscamtrap %>% 
  mutate(long = str_replace_all(long, "\\(", "")) %>%
  mutate(lat = str_replace_all(lat, "\\)", ""))

#Transforming latitude and longitude in numeric data, to use it on the map
waypointscamtrap <- waypointscamtrap %>%
  mutate_at('long', as.numeric) %>%
  mutate_at('lat', as.numeric)

# Converting coordinates --------------------------------------------------
library(sf)

coordinates <- waypointscamtrap

sf_obj <- st_as_sf(coordinates, coords = c("long", "lat"), crs = 4326)

sf_obj_mercator <- st_transform(sf_obj, crs = st_crs("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"))
sf_obj_mercator <- st_transform(sf_obj, crs = st_crs("EPSG:3857"))
transformed_coords <- st_coordinates(sf_obj_mercator)

coordinates <- bind_cols(coordinates, transformed_coords)


# Map ---------------------------------------------------------------------

TelloPlotWaypoint <- autoplot.OpenStreetMap(TelloMap) +
  geom_point(data = coordinates,
             aes(x = X, y = Y),
             colour = "red", size = 2.5) +
  xlab("Longitude") + ylab("Latitude")

TelloPlotWaypoint

TelloPlotWaypointNames <- TelloPlotWaypoint +
  geom_text(data = coordinates, #Choose dataframe
            aes(X + 0.002, Y + 0.002, label = nombre), #Which label, and where
            hjust = 1.15, vjust = 0.5, #horizontal and vertical
            size = 5, colour = "white") + #size and colour
  ggtitle("Camera traps in El Tello (Spain)") #title


library(ggsn)

TelloScaleNorth <- TelloPlotWaypointNames +
  scalebar(x.min = -61000, x.max = -57000, y.min = 4764000, y.max = 4764300,
           dist= 1, dist_unit = "km", height = 0.3, st.dist = 0.8, st.size = 4,
           st.color = "white", transform = FALSE, border.size = 0.2, model = "WGS84") +
  north(x.min = -58000, x.max = -57000, y.min = 4767500, y.max = 4768100,
        scale = 1, symbol = 2)


TelloScaleNorth
