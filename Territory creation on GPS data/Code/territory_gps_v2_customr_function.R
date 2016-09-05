#**************************************************************************************************************************************#
#*                     R CODE TO PERFORM CLUSTERING ANALYSIS ON THE GPS DATA AND OPTIMIZE THEM                                        *#
#* OWNER: ANUBHAV DIKSHIT                                                                                                             *#
#**************************************************************************************************************************************#

cat("\014") # Clears console
rm(list=ls()) # removes everything in workspace
stringsAsFactors = FALSE # does not read strings as factors

start_time <- Sys.time() # Start time of code execution

#**************************************************************************************************************************************#
#*                                            LIBRARIES NEEDED IN THE CODE                                                            *#
#**************************************************************************************************************************************#

if (!require("pacman")) install.packages("pacman") # Paqckage manager for R, installs a package if not found

pacman::p_load(dplyr, dbscan, ggplot2, ggmap, fields) # Specify the packages that the code needs to work

# loading the required packages
library("dplyr")
library("dbscan")
library("ggplot2")
library("ggmap")
library("fields")

#**************************************************************************************************************************************#
#*                                            IMPORTING THE DATASET TO WORK ON                                                        *#
#**************************************************************************************************************************************#

# Dataset that is worked on, link: http://www.census.gov/geo/maps-data/data/gazetteer2015.html
school_with_gps <- read.delim("~/Data Experiments/Territory creation on GPS data/Input/2015_Gaz_ua_national.txt")

# Subsetting the gps coordinates as matrix
school_with_gps_matric <- as.matrix(school_with_gps[, c("INTPTLAT", "INTPTLONG")])

#distance matrix
school_with_gps_matric_distance <- rdist.earth(school_with_gps_matric, miles = F,R=6371)


#**************************************************************************************************************************************#
#*                                            PERFORMING CLUSTERING ON THE GPS COORDINATES                                            *#
#**************************************************************************************************************************************#

threshold.in.km <- 50

#clustering
clustered_school <- hclust(as.dist(school_with_gps_matric_distance), method = "single")

clusters <- cutree(clustered_school, h = threshold.in.km)

school_with_gps_clustered <- cbind(school_with_gps, clusters)


#**************************************************************************************************************************************#
#*                                           PLOTTING THE CLUSTERED GPS COORDINATES                                                   *#
#**************************************************************************************************************************************#

# getting the map
mapgilbert <- get_map(location = c(lon = mean(school_with_gps_clustered$INTPTLONG), lat = mean(school_with_gps_clustered$INTPTLAT)), zoom = 4,
                      maptype = "satellite", scale = 2)

# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = school_with_gps_clustered, size = 5, shape = 21,  aes(x = INTPTLONG, y = INTPTLAT, colour = factor(clusters), fill = factor(clusters))) 
  + guides(fill=FALSE, alpha=FALSE, size=FALSE)

#**************************************************************************************************************************************#
#*                                           EXPORTING THE FILE TO LOCAL                                                              *#
#**************************************************************************************************************************************#

end_time <- as.numeric(Sys.time() - start_time)

cat("The code execution time was: ", end_time, " Secs")

write.csv(school_with_gps_clustered, file = "~/Data Experiments/Territory creation on GPS data/Output/clustered_school.csv",
          row.names = FALSE)
                                                    
