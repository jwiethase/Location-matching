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
short.data.sp$nearest_in_set2 <- apply(gDistance(identify.sp, short.data.sp, byid=TRUE), 1, which.min)
short.data.sp <- as.data.frame(short.data.sp) %>% unique()
short.data.sp$matched_to <- identify$Site[short.data.sp$nearest_in_set2]
short.data.sp <- merge(short.data.sp, short.data) %>% dplyr::select(-nearest_in_set2) %>% 
      dplyr::select(matched_to, everything())

write.csv(short.data.sp, "matched_locations.csv")
