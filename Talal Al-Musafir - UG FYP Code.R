# R Script for UG FYP Analysis
# Author: Talal Al-Musafir
# Email: ta2123@ic.ac.uk
# In association with Imperial College London & CaLE Labs

# If you have questions about the code, just email me, I'll try my best to answer them!
# Good luck with your analysis (you're going to need it)

setwd("C:/Users/<user>/OneDrive - Imperial College London/")
library(tidyverse)
library(terra)
library(tiff)
library(sf)
library(landscapemetrics)
library(vegan)
library(ggplot2)
library(osmdata)
library(dplyr)
library(soundecology)
library(tuneR)
library(glue)
library(GGally)
library(AER)
library(MASS)
library(visreg)
library(leaflet)

###

#### LOAD ID SUMMARY FILES FROM RDS ####

# Here, I copied the RDS files into my college OneDrive for ease of access
# Just copy this directory into your file browser and pin / favourite it
# //rds.imperial.ac.uk/rds/project/silwood_acoustics/live/Copped Hall - Epping/Copped Hall-Month Year/Kaleidoscope Outputs/SiteX
# Weirdly, you have to remove the top row from each of the csv files manually

apr25_site1 <- read.csv("April 2025/Site 1/idsummary.csv",
                  header = T,
                  stringsAsFactors = T)
apr25_site2 <- read.csv("April 2025/Site 2/idsummary.csv",
                  header = T,
                  stringsAsFactors = T)
apr25_site3 <- read.csv("April 2025/Site 3/idsummary.csv",
                  header = T,
                  stringsAsFactors = T)
apr25_site4 <- read.csv("April 2025/Site 4/idsummary.csv",
                  header = T,
                  stringsAsFactors = T)
apr25_site5 <- read.csv("April 2025/Site 5/idsummary.csv",
                  header = T,
                  stringsAsFactors = T)
apr25_site6 <- read.csv("April 2025/Site 6/idsummary.csv",
                  header = T,
                  stringsAsFactors = T)
apr25_site7 <- read.csv("April 2025/Site 7/idsummary.csv",
                  header = T,
                  stringsAsFactors = T)
apr25_site8 <- read.csv("April 2025/Site 8/idsummary.csv",
                  header = T,
                  stringsAsFactors = T)
apr25_siteA <- read.csv("April 2025/Site A/idsummary.csv",
                  header = T,
                  stringsAsFactors = T)
apr25_siteB <- read.csv("April 2025/Site B/idsummary.csv",
                  header = T,
                  stringsAsFactors = T)
apr25_siteC <- read.csv("April 2025/Site C/idsummary.csv",
                  header = T,
                  stringsAsFactors = T)

apr25_all_sites <- bind_rows(apr25_site1,
                                   apr25_site2,
                                   apr25_site3,
                                   apr25_site4,
                                   apr25_site5,
                                   apr25_site6,
                                   apr25_site7,
                                   apr25_site8,
                                   apr25_siteA,
                                   apr25_siteB,
                                   apr25_siteC)

colnames(apr25_all_sites)[1] <- "site"
apr25_all_sites$site <- c("Site 1","Site 2","Site 3","Site 4","Site 5","Site 6","Site 7","Site 8","Site A","Site B","Site C")
# Select the columns we don't want
apr25_all_sites <- subset(apr25_all_sites,select = -c(X,X.1,Presence.P.Values.))
# Replace NAs with 0
apr25_all_sites[is.na(apr25_all_sites)] <- 0

##

jul25_site1 <- read.csv("July 2025/Site 1/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)
jul25_site2 <- read.csv("July 2025/Site 2/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)
jul25_site3 <- read.csv("July 2025/Site 3/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)
jul25_site4 <- read.csv("July 2025/Site 4/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)
jul25_site5 <- read.csv("July 2025/Site 5/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)
jul25_site6 <- read.csv("July 2025/Site 6/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)
jul25_site7 <- read.csv("July 2025/Site 7/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)
jul25_site8 <- read.csv("July 2025/Site 8/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)
jul25_siteA <- read.csv("July 2025/Site A/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)
jul25_siteB <- read.csv("July 2025/Site B/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)
jul25_siteC <- read.csv("July 2025/Site C/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)

jul25_all_sites <- bind_rows(jul25_site1,
                             jul25_site2,
                             jul25_site3,
                             jul25_site4,
                             jul25_site5,
                             jul25_site6,
                             jul25_site7,
                             jul25_site8,
                             jul25_siteA,
                             jul25_siteB,
                             jul25_siteC)

colnames(jul25_all_sites)[1] <- "site"
jul25_all_sites$site <- c("Site 1","Site 2","Site 3","Site 4","Site 5","Site 6","Site 7","Site 8","Site A","Site B","Site C")
# Select the columns we don't want
jul25_all_sites <- subset(jul25_all_sites,select = -c(X,X.1,Presence.P.Values.))
# Replace NAs with 0
jul25_all_sites[is.na(jul25_all_sites)] <- 0

##

mar26_site1 <- read.csv("March 2026/Site 1/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)
mar26_site2 <- read.csv("March 2026/Site 2/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)
mar26_site3 <- read.csv("March 2026/Site 3/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)
mar26_site4 <- read.csv("March 2026/Site 4/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)
mar26_site5 <- read.csv("March 2026/Site 5/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)
mar26_site6 <- read.csv("March 2026/Site 6/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)
mar26_site8 <- read.csv("March 2026/Site 8/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)
mar26_siteA <- read.csv("March 2026/Site A/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)
mar26_siteB <- read.csv("March 2026/Site B/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)
mar26_siteC <- read.csv("March 2026/Site C/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)
mar26_siteP2 <- read.csv("March 2026/Site P2/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)
mar26_siteP3 <- read.csv("March 2026/Site P3/idsummary.csv",
                        header = T,
                        stringsAsFactors = T)

mar26_all_sites <- bind_rows(mar26_site1,
                             mar26_site2,
                             mar26_site3,
                             mar26_site4,
                             mar26_site5,
                             mar26_site6,
                             mar26_site8,
                             mar26_siteA,
                             mar26_siteB,
                             mar26_siteC,
                             mar26_siteP2,
                             mar26_siteP3)

colnames(mar26_all_sites)[1] <- "site"
mar26_all_sites$site <- c("Site 1","Site 2","Site 3","Site 4","Site 5","Site 6","Site 8","Site A","Site B","Site C", "Site P2", "Site P3")
# Select the columns we don't want
mar26_all_sites <- subset(mar26_all_sites,select = -c(X,X.1,Presence.P.Values.))
# Replace NAs with 0
mar26_all_sites[is.na(mar26_all_sites)] <- 0

###

#### COORDINATES DATA ####

# Created this manually, there should be a file with this info in Cris' shared OneDrive folder
coords_data <- read.csv("coords_data.csv")

# Subset & merge
apr25_all_sites <- merge(apr25_all_sites, 
                         coords_data[coords_data$start_month=="Apr-25",],
                         all.x = T,
                         by = "site")
jul25_all_sites <- merge(jul25_all_sites, 
                         coords_data[coords_data$start_month=="Jul-25",],
                         all.x = T,
                         by = "site")
mar26_all_sites <- merge(mar26_all_sites, 
                         coords_data[coords_data$start_month=="Mar-26",],
                         all.x = T,
                         by = "site")

###

#### MAP ####

# Create colour code separated by site name, if you want (doesn't look great)
site_palette <- leaflet::colorFactor(c("#ff0000",
                                       "#ff7700",
                                       "#ffbb00",
                                       "#ffff00",
                                       "#bbff00",
                                       "#77ff00",
                                       "#00ff00",
                                       "#00ff77",
                                       "#00ffbb",
                                       "#00ffff",
                                       "#00bbff",
                                       "#0077ff",
                                       "#0000ff"), domain = unique(coords_data$site))

# Or separate them by sampling period
month_palette <- leaflet::colorFactor(c("#f0f",
                                        "#ff0",
                                        "#0ff"), domain = unique(coords_data$start_month))

# Or by Copped Hall vs Patmore's Field
locale_palette <- leaflet::colorFactor(c("#00ffff",
                                         "#ffaa00"), domain = unique(coords_data$location))


map_site <- leaflet() %>%
  addTiles() %>% # Add default OpenStreetMap map tiles
  addCircleMarkers(lng = coords_data$long,
                   lat = coords_data$lat,
                   popup = coords_data$site,
                   fillColor = site_palette(coords_data$site),
                   fillOpacity = 0.33,
                   color = "black",
                   stroke = TRUE,
                   weight = 2.5,
                   radius = 5)

map_month <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng = coords_data$long,
                   lat = coords_data$lat,
                   popup = coords_data$site,
                   fillColor = month_palette(coords_data$start_month),
                   fillOpacity = 0.65,
                   color = "#000",
                   stroke = TRUE,
                   weight = 2.5,
                   radius = 5)

map_locale <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng = coords_data$long,
                   lat = coords_data$lat,
                   popup = coords_data$site,
                   #fillColor = site_palette(coords_data$site),
                   #fillColor = month_palette(coords_data$start_month),
                   fillColor = locale_palette(coords_data$location),
                   fillOpacity = 1,
                   color = "black",
                   stroke = TRUE,
                   weight = 2.5,
                   radius = 5)

# Just pick what you want, I used map_locale
map_site
map_month
map_locale

###

#### SPECIES RICHNESS ####

# Do this manually because I couldn't find how to do it in R
# Export dataframe to csv, then open and modify in excel
write.csv(apr25_all_sites, file = "April 2025 data.csv")
write.csv(jul25_all_sites, file = "July 2025 data.csv")
write.csv(mar26_all_sites, file = "March 2026 data.csv")

# In excel, I used the COUNTIF() function to calculate spp richness for each site
# We did this by counting how many presence p values are below 0.05
# This means there is less than a 5% probability this species was detected here due purely to chance
# You can also use the method below (spp is present if it's detected even once)

# Then re-import the csv with the new spp richness column
apr25_richness <- read.csv("April 2025 data spp richness.csv",
                           header = T,
                           stringsAsFactors = T)
jul25_richness <- read.csv("July 2025 data spp richness.csv",
                           header = T,
                           stringsAsFactors = T)
mar26_richness <- read.csv("March 2026 data spp richness.csv",
                           header = T,
                           stringsAsFactors = T)

###

#### FOREST COVER ####

# This raster file is from the UKCEH, you have to request this manually, then it will be sent by email to you
imported_raster <- rast('FME_64656034_1777727651377_3186/data/LCM.tif')
par(mfrow=c(1,1))
plot(imported_raster,
     axes=T,
     legend=T)

# Copy and subset, remove monitor ID & distance from road columns because they contain NAs which mess things up
coords_data_subset <- coords_data
coords_data_subset <- coords_data_subset %>%
  select(start_month, site, location, lat, long)

# Convert to an sf format, compatible with terra functions
coords_sf <- st_as_sf(coords_data_subset,
                      coords = c("long","lat"),
                      crs = 4326)

coords_bng <- coords_sf %>% st_transform(crs(imported_raster))

# Create buffer zones, 100m radius here gives the greatest scale of effect (ignore the name containing 200)
buffers_200 <- st_buffer(coords_bng, dist = 100)

# Honestly not sure what 1 & 2 mean here, probably are meant to correspond to forest & non-forest values in the raster?
woodland_cover_200 <- terra::extract(imported_raster, vect(buffers_200),
                                     fun = function(x) {
                                       mean(x %in% c(1, 2), na.rm = TRUE)
                                     })

# Add the forest cover data back to the coords data
coords_data_subset$forest_cover <- woodland_cover_200$LCM_1

# These "multivar" variables will be the nice big data frame we will use at the end
apr25_multivar <- merge(coords_data_subset[coords_data_subset$start_month=="Apr-25",], 
                        apr25_richness,
                        all.x = T,
                        by = "site")
jul25_multivar <- merge(coords_data_subset[coords_data_subset$start_month=="Jul-25",], 
                        jul25_richness,
                        all.x = T,
                        by = "site")
mar26_multivar <- merge(coords_data_subset[coords_data_subset$start_month=="Mar-26",], 
                        mar26_richness,
                        all.x = T,
                        by = "site")

###

#### NDSI ####

# Used the HPC system, that worked great, load .RData file from there

# load("//rds.imperial.ac.uk/rds/user/ta2123/home/.RData")

# This .RData file contains all the NDSI files, made from the BIRD SOUND FILES, NOT BAT
# Code that I ran on the HPC looked like this, just repeated for every site, and every sampling period:

#bird_folder_jul25_site1 <- "projects/silwood_acoustics/live/Copped Hall - Epping/Birds/Birds Site 1"
#bird_files_jul25_site1 <- list.files(bird_folder_jul25_site1,
#                                     pattern = "\\.wav$",
#                                     full.names = T)
#
#index <- 1
#bird_ndsi_jul25_site1 <- data.frame(
#  ndsi_left = double(),
#  ndsi_right = logical(),
#  biophony_left = double(),
#  anthrophony_left = double(),
#  biophony_right = logical(),
#  anthrophony_right = logical(),
#  file = character())

#index <- 1
#while(index < length(bird_files_jul25_site1)){
#  print(index)
#  print(bird_files_jul25_site1[index])
#  
#  bird_wave_file <- readWave(bird_files_jul25_site1[index])
#  bird_ndsi_jul25_site1[index,] <- ndsi(soundfile = bird_wave_file,
#                                        anthro_min = 1000,
#                                        anthro_max = 2000,
#                                        bio_min = 2000,
#                                        bio_max = 8000)
#  bird_ndsi_jul25_site1$file[index] <- bird_files_jul25_site1[index]

#  index <- index + 1
#}

#bird_ndsi_means <- data.frame(
#  file = character(),
#  mean_ndsi = double(),
#  sd_ndsi = double())

##

# Store all ndsi data frame names into a new means data frame
bird_ndsi_means <- data.frame(
  file = ls(pattern = ("^bird_ndsi_(apr25|jul25|mar26)_site[0-8a-zA-Z]+$"))
)

# Extract date and site ID
bird_ndsi_means$date <- sub("^bird_ndsi_([a-z0-9]+)_site.*$", "\\1", bird_ndsi_means$file)
bird_ndsi_means$site <- sub("^bird_ndsi_[a-z0-9]+_site([0-9a-zA-Z]+)$", "\\1", bird_ndsi_means$file)

# Calc mean ndsi values
bird_ndsi_means$mean_ndsi <- sapply(
  mget(bird_ndsi_means$file),
  function(x) mean(x$ndsi_left, na.rm = TRUE)
)

# Calc standard deviations
bird_ndsi_means$sd_ndsi <- sapply(
  mget(bird_ndsi_means$file),
  function(x) sd(x$ndsi_left, na.rm = TRUE)
)

# This stretch of code makes formatting consistent with the multivar dataframes
bird_ndsi_means$site <- paste("Site", toupper(bird_ndsi_means$site))

# Extract month abbreviation and year digits
bird_ndsi_means$month_abbrev <- sub("([a-z]+)([0-9]+)", "\\1", bird_ndsi_means$date)
bird_ndsi_means$year_two     <- sub("([a-z]+)([0-9]+)", "\\2", bird_ndsi_means$date)

# Convert month abbreviation to capitalised month name
month_lookup <- c(
  jan="Jan", feb="Feb", mar="Mar", apr="Apr", may="May", jun="Jun",
  jul="Jul", aug="Aug", sep="Sep", oct="Oct", nov="Nov", dec="Decr"
)
bird_ndsi_means$month_full <- month_lookup[bird_ndsi_means$month_abbrev]
# Add dash
bird_ndsi_means$year_full <- paste0("-", bird_ndsi_means$year_two)
# Combine into final date format
bird_ndsi_means$date_formatted <- paste0(bird_ndsi_means$month_full, bird_ndsi_means$year_full)

bird_ndsi_means <- subset(
  bird_ndsi_means,
  select = c(file, site, date_formatted, mean_ndsi,sd_ndsi)
)
names(bird_ndsi_means)[names(bird_ndsi_means) == "date_formatted"] <- "start_month.x"

# Merge with multivar
apr25_multivar <- merge(
  apr25_multivar,
  bird_ndsi_means,
  by = c("site", "start_month.x"),
  all = FALSE
)
jul25_multivar <- merge(
  jul25_multivar,
  bird_ndsi_means,
  by = c("site", "start_month.x"),
  all = FALSE
)
mar26_multivar <- merge(
  mar26_multivar,
  bird_ndsi_means,
  by = c("site", "start_month.x"),
  all = FALSE
)

# Sites B & C for July 25, and site 7 for March 26 had no data, something went wrong with the monitors
# Remove these, they will cause errors later

jul25_multivar <- jul25_multivar[-c(10,11),]
mar26_multivar <- mar26_multivar[-c(7),]

###

#### PRESENCE-ABSENCE MATRIX ####

# Need a P/A matrix to make a Dice-Sorensen similarity index
apr25_pa <- apr25_multivar
jul25_pa <- jul25_multivar
mar26_pa <- mar26_multivar

# First column of apr25 multivar that contain species presence data (ends at col 24)
column <- 8
row <- 1

while(row <= 11){
  print("row")
  print(row)
  
  while(column <= 24){
    print("column")
    print(column)
    
    print("value")
    print(apr25_multivar[row, column])
    
    if(apr25_multivar[row, column] == 0){
      apr25_pa[row, column] <- 0
      print("ABSENT")
    }
    else if(apr25_multivar[row, column] >= 1){
      apr25_pa[row, column] <- 1
      print("PRESENT")
    }
    else{
      apr25_pa[row, column] <- NULL
      print("NULL")
    }
    column <- column + 1
  }
  column <- 8
  
  row <- row + 1
  print("---------------")
}

# Reset for jul25
column <- 8
row <- 1

while(row <= 9){
  print("row")
  print(row)
  
  while(column <= 24){
    print("column")
    print(column)
    
    print("value")
    print(jul25_multivar[row, column])
    
    if(jul25_multivar[row, column] == 0){
      jul25_pa[row, column] <- 0
      print("ABSENT")
    }
    else if(jul25_multivar[row, column] >= 1){
      jul25_pa[row, column] <- 1
      print("PRESENT")
    }
    else{
      jul25_pa[row, column] <- NULL
      print("NULL")
    }
    column <- column + 1
  }
  column <- 8
  
  row <- row + 1
  print("---------------")
}

# Reset again
column <- 8
row <- 1

while(row <= 12){
  print("row")
  print(row)
  
  while(column <= 24){
    print("column")
    print(column)
    
    print("value")
    print(mar26_multivar[row, column])
    
    if(mar26_multivar[row, column] == 0){
      mar26_pa[row, column] <- 0
      print("ABSENT")
    }
    else if(mar26_multivar[row, column] >= 1){
      mar26_pa[row, column] <- 1
      print("PRESENT")
    }
    else{
      mar26_pa[row, column] <- NULL
      print("NULL")
    }
    column <- column + 1
  }
  column <- 8
  
  row <- row + 1
  print("---------------")
}

# Remove unneeded columns from P/A matrix
apr25_pa <- apr25_pa[, -c(1:7,25:53)]
jul25_pa <- jul25_pa[, -c(1:7,25:53)]
mar26_pa <- mar26_pa[, -c(1:7,25:53)]
all_sites_pa <- rbind(apr25_pa,
                      jul25_pa,
                      mar26_pa)

###

#### SIMILARITY INDEX & PRINCIPAL COORDINATE ANALYSIS ####

# cmdscale() is a PCoA
# Within this, method "bray" + binary TRUE = Dice-Sorensen similarity index
apr25_pcoa <- cmdscale(
  vegdist(apr25_pa,
          method = "bray",
          binary = TRUE),
  eig = TRUE,
  k = 2,
  add = T)

jul25_pcoa <- cmdscale(
  vegdist(jul25_pa,
          method = "bray",
          binary = TRUE),
  eig = TRUE,
  k = 2,
  add = T)

mar26_pcoa <- cmdscale(
  vegdist(mar26_pa,
          method = "bray",
          binary = TRUE),
  eig = TRUE,
  k = 2,
  add = T)

all_sites_pcoa <- cmdscale(
  vegdist(all_sites_pa,
          method = "bray",
          binary = TRUE),
  eig = TRUE,
  k = 2,
  add = T)

# Plot with ggplot2
# Tutorial video I used: https://youtu.be/G5Qckqq5Erw
colnames(apr25_pcoa$points) <- c("PCoA1", "PCoA2")

apr25_pcoa$percent_explained <- 100 * (apr25_pcoa$eig / sum(apr25_pcoa$eig))
apr25_pcoa$percent_explained_rounded <- round(apr25_pcoa$percent_explained[1:2],
                                              digits = 1)

labels <- c(glue("PCoA 1 ({apr25_pcoa$percent_explained_rounded[1]}%)"),
            glue("PCoA 2 ({apr25_pcoa$percent_explained_rounded[2]}%)"))

apr25_pcoa$points %>%
  ggplot(aes(x = PCoA1,
             y = PCoA2)) + 
  geom_point() + 
  labs(x = labels[1],
       y = labels[2])

tibble(percent_explained = apr25_pcoa$percent_explained,
       axis = 1:length(apr25_pcoa$percent_explained)) %>%
  ggplot(aes(x = axis,
             y = percent_explained)) +
  labs(x = "PCoA axis",
       y = "% of variation explained by axis") +
  geom_line() +
  geom_point()

# We can see that the first axis explains 61% of var, and the second explains 13%

##

colnames(jul25_pcoa$points) <- c("PCoA1", "PCoA2")

jul25_pcoa$percent_explained <- 100 * (jul25_pcoa$eig / sum(jul25_pcoa$eig))
jul25_pcoa$percent_explained_rounded <- round(jul25_pcoa$percent_explained[1:2],
                                              digits = 1)

labels <- c(glue("PCoA 1 ({jul25_pcoa$percent_explained_rounded[1]}%)"),
            glue("PCoA 2 ({jul25_pcoa$percent_explained_rounded[2]}%)"))

jul25_pcoa$points %>%
  ggplot(aes(x = PCoA1,
             y = PCoA2)) + 
  geom_point() + 
  labs(x = labels[1],
       y = labels[2])

tibble(percent_explained = jul25_pcoa$percent_explained,
       axis = 1:length(jul25_pcoa$percent_explained)) %>%
  ggplot(aes(x = axis,
             y = percent_explained)) +
  labs(x = "PCoA axis",
       y = "% of variation explained by axis") +
  geom_line() + 
  geom_point()

# Here, the first axis only explains 43% of var, and it decreases less steeply

##

colnames(mar26_pcoa$points) <- c("PCoA1", "PCoA2")

mar26_pcoa$percent_explained <- 100 * (mar26_pcoa$eig / sum(mar26_pcoa$eig))
mar26_pcoa$percent_explained_rounded <- round(mar26_pcoa$percent_explained[1:2],
                                              digits = 1)

labels <- c(glue("PCoA 1 ({mar26_pcoa$percent_explained_rounded[1]}%)"),
            glue("PCoA 2 ({mar26_pcoa$percent_explained_rounded[2]}%)"))

mar26_pcoa$points %>%
  ggplot(aes(x = PCoA1,
             y = PCoA2)) + 
  geom_point() + 
  labs(x = labels[1],
       y = labels[2])

tibble(percent_explained = mar26_pcoa$percent_explained,
       axis = 1:length(mar26_pcoa$percent_explained)) %>%
  ggplot(aes(x = axis,
             y = percent_explained)) + 
  labs(x = "PCoA axis",
       y = "% of variation explained by axis") +
  geom_line() + 
  geom_point()

# And here, axis 1 explains 37.1% of var

##

colnames(all_sites_pcoa$points) <- c("PCoA1", "PCoA2")

all_sites_pcoa$percent_explained <- 100 * (all_sites_pcoa$eig / sum(all_sites_pcoa$eig))
all_sites_pcoa$percent_explained_rounded <- round(all_sites_pcoa$percent_explained[1:2],
                                              digits = 1)

labels <- c(glue("PCoA 1 ({all_sites_pcoa$percent_explained_rounded[1]}%)"),
            glue("PCoA 2 ({all_sites_pcoa$percent_explained_rounded[2]}%)"))

all_sites_pcoa$points %>%
  ggplot(aes(x = PCoA1,
             y = PCoA2)) + 
  geom_point() + 
  labs(x = labels[1],
       y = labels[2])

tibble(percent_explained = all_sites_pcoa$percent_explained,
       axis = 1:length(all_sites_pcoa$percent_explained)) %>%
  ggplot(aes(x = axis,
             y = percent_explained)) + 
  labs(x = "PCoA axis",
       y = "% of variation explained by axis") +
  geom_line() + 
  geom_point()

###

#### GLMs ####

# Some questions to ask, and their answers for MY investigation:
  # Do you have enough sites to run an interaction model? -> We did not, but the code is there if you do
  # Is there any reason NOT to combine all the sampling periods? Is it part of your investigation? -> Not here
  # Check as many combinations of covariates as possible
  # Check distribution of covariates, do we need to try log-transforming anything?

hist(all_sites_multivar$forest_cover) # Yes
all_sites_multivar$log_forest_cover <- log(all_sites_multivar$forest_cover + 0.001) # + 0.001 removes -Inf errors
hist(all_sites_multivar$log_forest_cover)

hist(all_sites_multivar$dist_from_road_metres) # No
hist(all_sites_multivar$mean_ndsi) # No
hist(all_sites_multivar$sd_ndsi) # No

###

# Now we run a GLM including all our covariates, with the response being the PCoA axis 1 with the most explanatory power
apr25_multivar$pcoa <- apr25_pcoa$points[,1]
apr25_multivar_glm <- glm(pcoa ~ dist_from_road_metres * forest_cover * sd_ndsi,
                          family = "gaussian", data = apr25_multivar)
summary(apr25_multivar_glm)  # Significance here, but we don't have enough data for interactions to be reliable
RsquareAdj(apr25_multivar_glm)

# Simplify model  by dropping insignificant interaction
drop.scope(apr25_multivar_glm) # This says to drop the 3-way interaction first
apr25_multivar_glm2 <- update(apr25_multivar_glm, . ~ . - dist_from_road_metres:forest_cover:sd_ndsi)
summary(apr25_multivar_glm2)
# Is the new model significantly worse? -> p < 0.05, yes, so go back
anova(apr25_multivar_glm2, apr25_multivar_glm, test = "Chisq")

# Try using logged forest cover
apr25_multivar_log_glm <- glm(pcoa ~ dist_from_road_metres * log_forest_cover * sd_ndsi,
                          family = "gaussian", data = apr25_multivar)
summary(apr25_multivar_log_glm) # Significance here, but we don't have enough data for interactions to be reliable
# Can't drop any terms bc 3 way interaction is significant

##

jul25_multivar$pcoa <- jul25_pcoa$points[,1]
jul25_multivar_glm <- glm(pcoa ~ dist_from_road_metres * forest_cover  * sd_ndsi,
                          family = "gaussian", data = jul25_multivar)
summary(jul25_multivar_glm) # Significance here, but we don't have enough data for interactions to be reliable
RsquareAdj(jul25_multivar_glm)

##

mar26_multivar$pcoa <- mar26_pcoa$points[,1]
mar26_multivar_glm <- glm(pcoa ~ dist_from_road_metres * forest_cover * sd_ndsi,
                          family = "gaussian", data = mar26_multivar)
summary(mar26_multivar_glm)

##

# Try same but with spp richness as response
apr25_multivar_richness_glm <- glm(spp_richness ~ dist_from_road_metres * forest_cover * mean_ndsi,
                                   data = apr25_multivar,
                                   family = "quasipoisson")
jul25_multivar_richness_glm <- glm(spp_richness ~ dist_from_road_metres * forest_cover * mean_ndsi,
                                   data = jul25_multivar,
                                   family = "quasipoisson")
mar26_multivar_richness_glm <- glm(spp_richness ~ dist_from_road_metres * forest_cover * mean_ndsi,
                                   data = mar26_multivar,
                                   family = "quasipoisson")

summary(apr25_multivar_richness_glm)
summary(jul25_multivar_richness_glm) # Significance here, but we don't have enough data for interactions to be reliable
summary(mar26_multivar_richness_glm)

##

# Simplify
jul25_multivar_richness_glm2 <- update(jul25_multivar_richness_glm, . ~ . - mean_ndsi)
summary(jul25_multivar_richness_glm2)
# Is the new model significantly worse? -> p < 0.05, so go back to model 1
anova(jul25_multivar_richness_glm2, jul25_multivar_richness_glm, test = "Chisq")

###

# Include all sampling periods in 1 data frame, because sampling period doesn't matter for my study
all_sites_multivar <- rbind(apr25_multivar[,-c(55,56,57,58)],
                            jul25_multivar[,-c(55,56,57,58)],
                            mar26_multivar[,-c(55,56,57,58)])

# Possible combinations:
  # Responses:
    # PCoA Axis 1
    # Spp richness
  # Explanatory 1:
    # Forest cover
    # Log(forest cover)
  # Explanatory 2:
    # Dist from road
    # Mean NDSI
    # SD of NDSI

# 12 total models to try, just switch out terms below

# PCoA ~ Dist from road
all_sites_multivar_glm <- glm(pcoa ~ dist_from_road_metres + forest_cover,
                              family = "gaussian", data = all_sites_multivar)
summary(all_sites_multivar_glm)
RsquareAdj(all_sites_multivar_glm)

par(mfrow = c(2,2))
plot(all_sites_multivar_glm)

# PCoA ~ NDSI
all_sites_multivar_glm2 <- glm(pcoa ~ mean_ndsi + log_forest_cover,
                              family = "gaussian", data = all_sites_multivar)
summary(all_sites_multivar_glm2)
RsquareAdj(all_sites_multivar_glm2)

par(mfrow = c(2,2))
plot(all_sites_multivar_glm2)

# Richness ~ Dist from road
all_sites_multivar_glm3 <- glm(spp_richness ~ dist_from_road_metres + log_forest_cover,
                               family = "gaussian", data = all_sites_multivar)
summary(all_sites_multivar_glm3)
RsquareAdj(all_sites_multivar_glm3) 

par(mfrow = c(2,2))
plot(all_sites_multivar_glm3)

# Richness ~ NDSI
all_sites_multivar_glm4 <- glm(spp_richness ~ mean_ndsi + log_forest_cover,
                               family = "gaussian", data = all_sites_multivar)
summary(all_sites_multivar_glm4)
RsquareAdj(all_sites_multivar_glm4)

par(mfrow = c(2,2))
plot(all_sites_multivar_glm4)

# No significance anywhere 

###

#### PLOTS ####

# Descriptive stats

# Species richness
median(apr25_multivar$spp_richness)
median(jul25_multivar$spp_richness)
median(mar26_multivar$spp_richness)
median(all_sites_multivar$spp_richness)

IQR(apr25_multivar$spp_richness)
IQR(jul25_multivar$spp_richness)
IQR(mar26_multivar$spp_richness)
IQR(all_sites_multivar$spp_richness)

# Distance from road
mean(apr25_multivar$dist_from_road_metres)
mean(jul25_multivar$dist_from_road_metres)
mean(mar26_multivar$dist_from_road_metres)
mean(all_sites_multivar$dist_from_road_metres)

sd(apr25_multivar$dist_from_road_metres)
sd(jul25_multivar$dist_from_road_metres)
sd(mar26_multivar$dist_from_road_metres)
sd(all_sites_multivar$dist_from_road_metres)

# Forest cover
mean(apr25_multivar$forest_cover)
mean(jul25_multivar$forest_cover)
mean(mar26_multivar$forest_cover)
mean(all_sites_multivar$forest_cover)

sd(apr25_multivar$forest_cover)
sd(jul25_multivar$forest_cover)
sd(mar26_multivar$forest_cover)
sd(all_sites_multivar$forest_cover)

# NDSI
mean(apr25_multivar$mean_ndsi)
mean(jul25_multivar$mean_ndsi)
mean(mar26_multivar$mean_ndsi)
mean(all_sites_multivar$mean_ndsi)

sd(apr25_multivar$mean_ndsi)
sd(jul25_multivar$mean_ndsi)
sd(mar26_multivar$mean_ndsi)
sd(all_sites_multivar$mean_ndsi)

###

# Pairwise plots
ggpairs(data = apr25_multivar,
        columns = c(54,53,49,6),
        columnLabels = c("PCoA Axis 1", "SD of NDSI", "Dist. from M25", "Forest cover"))

ggpairs(data = jul25_multivar,
        columns = c(54,53,49,6),
        columnLabels = c("PCoA Axis 1", "SD of NDSI", "Dist. from M25", "Forest cover"))

ggpairs(data = jul25_multivar,
        columns = c(50,52,49,6),
        columnLabels = c("Spp. richness", "Mean NDSI", "Dist. from M25", "Forest cover"))

ggpairs(data = all_sites_multivar,
        columns = c(54,52,49,6),
        columnLabels = c("PCoA Axis 1", "Mean NDSI", "Dist. from M25", "Forest cover"))

ggpairs(data = all_sites_multivar,
        columns = c(54,53,49,6),
        columnLabels = c("PCoA Axis 1", "SD of NDSI", "Dist. from M25", "Forest cover"))

###

# Faceted interaction plot (ONLY if you're doing interaction model)
# Put sd_ndsi and forest_cover into bins for easier interpretation on graph
apr25_multivar$forest_bin <- cut(apr25_multivar$forest_cover, 3,
                                 labels = c("Low forest cover", "Medium forest cover", "High forest cover"))
apr25_multivar$ndsi_bin   <- cut(apr25_multivar$sd_ndsi, 3,
                                 labels = c("Low SD in NDSI", "Medium SD in NDSI", "High SD in NDSI"))

ggplot(apr25_multivar,
       aes(x = dist_from_road_metres,
           y = pcoa,
           colour = forest_bin,
           group = forest_bin)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ ndsi_bin) +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Distance from road (m)",
       y = "PCoA axis 1",
       colour = "Forest cover (bins)") +
  theme_bw(base_size = 14)

# Try same but forest cover logged ver.
apr25_multivar$log_forest_bin <- cut(apr25_multivar$log_forest_cover, 3,
                                 labels = c("Low", "Medium", "High"))
apr25_multivar$ndsi_bin   <- cut(apr25_multivar$sd_ndsi, 3,
                                 labels = c("Low SD NDSI", "Medium SD NDSI", "High SD NDSI"))

ggplot(apr25_multivar,
       aes(x = dist_from_road_metres,
           y = pcoa,
           colour = log_forest_bin,
           group = log_forest_bin)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ ndsi_bin) +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Distance from road (m)",
       y = "PCoA axis 1",
       colour = "Log of forest cover (bins)") +
  theme_bw(base_size = 14)

##

jul25_multivar$forest_bin <- cut(jul25_multivar$forest_cover, 3,
                                 labels = c("Low forest cover", "Medium forest cover", "High forest cover"))
jul25_multivar$sd_ndsi_bin   <- cut(jul25_multivar$sd_ndsi, 3,
                                 labels = c("Low SD in NDSI", "Medium SD in NDSI", "High SD in NDSI"))

ggplot(jul25_multivar,
       aes(x = dist_from_road_metres,
           y = pcoa,
           colour = forest_bin,
           group = forest_bin)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ sd_ndsi_bin) +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Distance from road (m)",
       y = "PCoA axis 1",
       colour = "Forest cover (bins)") +
  theme_bw(base_size = 14)

##

jul25_multivar$forest_bin <- cut(jul25_multivar$forest_cover, 3,
                                 labels = c("Low forest cover", "Medium forest cover", "High forest cover"))
jul25_multivar$mean_ndsi_bin   <- cut(jul25_multivar$mean_ndsi, 3,
                                 labels = c("Low mean NDSI", "Medium mean NDSI", "High mean NDSI"))

ggplot(jul25_multivar,
       aes(x = dist_from_road_metres,
           y = spp_richness,
           colour = forest_bin,
           group = forest_bin)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ mean_ndsi_bin) +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Distance from road (m)",
       y = "Species richness",
       colour = "Forest cover (bins)") +
  theme_bw(base_size = 14)

##

all_sites_multivar$forest_bin <- cut(all_sites_multivar$forest_cover, 3,
                                     labels = c("Low forest cover", "Medium forest cover", "High forest cover"))
all_sites_multivar$mean_ndsi_bin   <- cut(all_sites_multivar$mean_ndsi, 3,
                                     labels = c("Low mean NDSI", "Medium mean NDSI", "High mean NDSI"))

ggplot(all_sites_multivar,
       aes(x = dist_from_road_metres,
           y = pcoa,
           colour = forest_bin,
           group = forest_bin)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ mean_ndsi_bin) +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Distance from road (m)",
       y = "PCoA axis 1",
       colour = "Forest cover (bins)") +
  theme_bw(base_size = 14)


##

all_sites_multivar$forest_bin <- cut(all_sites_multivar$forest_cover, 2,
                                 labels = c("0.0 - 0.5", "0.5 - 1.0"))
all_sites_multivar$ndsi_bin   <- cut(all_sites_multivar$sd_ndsi, 3,
                                 labels = c("Low SD in NDSI", "Medium SD in NDSI", "High SD in NDSI"))

ggplot(all_sites_multivar,
       aes(x = dist_from_road_metres,
           y = pcoa,
           colour = forest_bin,
           group = forest_bin)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ ndsi_bin) +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Distance from road (m)",
       y = "PCoA axis 1",
       colour = "Forest cover (bins)") +
  theme_bw(base_size = 14)

###

# Scatterplots

# Again, switch out terms and labels here as needed

# PCoA ~ Dist from road
ggplot(all_sites_multivar,
       aes(x = dist_from_road_metres,
           y = pcoa,
           group = forest_bin,
           colour = forest_bin)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = T) +
  labs(x = "Distance from M25 (m)",
       y = "PCoA Axis 1",
       colour = "Forest cover")

all_sites_multivar$log_forest_bin <- cut(all_sites_multivar$log_forest_cover, 2,
                                     labels = c("-6.91 ~ -3.45", "-3.45 ~ 0.00791"))

# Spp richness ~ Mean NDSI
ggplot(all_sites_multivar,
       aes(x = mean_ndsi,
           y = spp_richness,
           group = log_forest_bin,
           colour = log_forest_bin)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = T) +
  labs(x = "Mean NDSI",
       y = "Species Richness",
       colour = "Log(forest cover)")

###