# ----------------------------------
# Authors: Ryan Hull
# Date: October 2025
# Purpose: process understand and break down neartic anuran observations
# F25 McGill - BIOL 432 Data Project
# ----------------------------------




# 1. Libraries
rm(list=ls())
library("readr")
library("dplyr")




# 2. Data processing
# Amphibian observations with location in Can/Usa/mexico from 2000-2025
# GBIF.org (02 October 2025) GBIF Occurrence Download https://doi.org/10.15468/dl.tzn8at
gbif_amphibians <- read_tsv("data/raw/gbif_data_amphibians_2000_2025.csv")

# Keep only species level observations
gbif_amphibians <- gbif_amphibians[which(gbif_amphibians$taxonRank=="SPECIES"), ]

# Keep only observations whose coordinate uncertainty doesn't exceed our gridsize
gbif_amphibians <- gbif_amphibians[which(gbif_amphibians$coordinateUncertaintyInMeters < 1000), ]

# Remove unhelpful columns
gbif_amphibians <- gbif_amphibians[,c("gbifID","species", "order", "countryCode",
                                      "stateProvince", "decimalLatitude",
                                      "decimalLongitude","day","month", "year")]

# Exclude cold season observations? Maybe needed if we use climate parameters specific to warm season
#?  months_to_keep <- c(5,6,7,8,9)
#?  gbif_amphibians <- gbif_amphibians[gbif_amphibians$month %in% months_to_keep,]

# Filter out caudates
gbif_anurans <- gbif_amphibians[gbif_amphibians$order == "Anura",]
gbif_anurans$order <- NULL

# Filter out non-neartic states
gbif_anurans <- gbif_anurans[gbif_anurans$stateProvince!="Hawaii",]



# 3. filter by number of observations per species:
species_counts <- gbif_anurans %>%
  group_by(species) %>%           # group by species
  summarise(observations = n())   # count rows per species

species_with_100obs <- species_counts[species_counts$observations > 99,]

gbif_anurans <-
  gbif_anurans[gbif_anurans$species %in% species_with_100obs$species,]




# 4. Write out data
write.csv(gbif_anurans, "data/processed/clean_neartic_anuran_observations.csv")