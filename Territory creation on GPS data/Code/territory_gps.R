#**************************************************************************************************************************************#
#*                     R CODE TO PERFORM CLUSTERING ANALYSIS ON THE GPS DATA AND OPTIMIZE THEM                                        *#
#* OWNER: ANUBHAV DIKSHIT                                                                                                             *#
#**************************************************************************************************************************************#

cat("\014") # Clears console
rm(list=ls()) # removes everything in workspace
stringsAsFactors = FALSE # does not read strings as factors
getOption("width")
tidy = TRUE # Makes the code Tidy

start_time <- Sys.time() # Start time of code execution

#**************************************************************************************************************************************#
#*                                            LIBRARIES NEEDED IN THE CODE                                                            *#
#**************************************************************************************************************************************#

if (!require("pacman")) install.packages("pacman") # Paqckage manager for R, installs a package if not found

pacman::p_load(dplyr, dbscan, ggplot2, ggmap) # Specify the packages that the code needs to work

# loading the required packages
library("dplyr")
library("dbscan")
library("ggplot2")
library("ggmap")
#**************************************************************************************************************************************#
#*                                            IMPORTING THE DATASET TO WORK ON                                                        *#
#**************************************************************************************************************************************#

# Dataset that is worked on, link: http://www.census.gov/geo/maps-data/data/gazetteer2015.html
school_with_gps <- read.delim("~/Data Experiments/Territory creation on GPS data/Input/2015_Gaz_ua_national.txt")

# Subsetting the gps coordinates as matrix
school_with_gps_matric <- as.matrix(school_with_gps[, c("INTPTLAT", "INTPTLONG")])
                                                    

#**************************************************************************************************************************************#
#*                                            PERFORMING CLUSTERING ON THE GPS COORDINATES                                            *#
#**************************************************************************************************************************************#

clustered_school <- dbscan(school_with_gps_matric, 0.045, minPts = 10, splitRule = "MIDPT", search = "kdtree", bucketSize = 10)

school_with_gps_clustered <- cbind(school_with_gps, clustered_school$cluster)

#Ploting the clusterd school

#**************************************************************************************************************************************#
#*                                           PLOTTING THE CLUSTERED GPS COORDINATES                                                   *#
#**************************************************************************************************************************************#

# getting the map
mapgilbert <- get_map(location = c(lon = mean(school_with_gps_clustered$INTPTLONG), lat = mean(school_with_gps_clustered$INTPTLAT)), zoom = 4,
                      maptype = "satellite", scale = 2)

# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = school_with_gps_clustered, aes(x = INTPTLONG, y = INTPTLAT, fill = clustered_school$cluster , alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)
                                                    
