rm(list = ls(all=TRUE))  
library(dplyr)
library(rgeos)
library(sp)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read in the full .csv file
full.data <- read.csv("/Volumes/HDD internal/Google Drive/Work/NHM/Mapping/General/Data sheets/MAIN_Paragonimus_data.csv")

# Prepare the data set requiring location matching
identify <- full.data %>% dplyr::filter(citation_full == "IDENTIFY", !is.na(Latitude), !is.na(Longitude)) %>% 
      dplyr::select(citation_full, country, County, Site, Latitude, Longitude) %>% 
      dplyr::mutate(Latitude = as.numeric(as.character(Latitude)),
                    Longitude = as.numeric(as.character(Longitude)))

identify.sp <- SpatialPoints(identify[-c(1:4)])
      
# Prepare the data set used for location matching
short.data <- full.data %>% dplyr::filter(citation_full != "IDENTIFY", !is.na(Latitude), !is.na(Longitude)) %>% 
      dplyr::select(citation_full, country, County, Site, Latitude, Longitude) %>% 
      dplyr::mutate(Latitude = as.numeric(as.character(Latitude)),
                    Longitude = as.numeric(as.character(Longitude))) 

short.data.sp <- SpatialPoints(short.data[-c(1:4)])

# Find records with coordinates closest to the values that need to be identified
identify.sp$nearest_in_set2 <- apply(gDistance(short.data.sp, identify.sp, byid=TRUE), 1, which.min)
identify.df <- as.data.frame(identify.sp) %>% unique()
identify.df$matched_to <- short.data$citation_full[identify.df$nearest_in_set2]
identify.df$matched_site <- short.data$Site[identify.df$nearest_in_set2]
data.merged <- merge(identify.df, identify) %>% 
      dplyr::select(-c(nearest_in_set2, citation_full)) %>% 
      dplyr::select(country, County, Site, Latitude, Longitude, matched_to, matched_site)

write.csv(data.merged, "matched_locations.csv")
