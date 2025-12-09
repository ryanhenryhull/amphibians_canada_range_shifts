# ------------------------------------------------------------------------------
# Authors: Ryan Hull
# Date: October 2025
# Purpose: Create curated list of neartic amphibian observations - without 
#          artificial cutoff at mexico border.
# F25 McGill - BIOL 432 Data Project
# ------------------------------------------------------------------------------

# 1. Packages
rm(list=ls())
library("readr")
library("dplyr")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggplot2")





# 2. Process gbif data 

# US/Mexico/Canada
# GBIF.org (02 October 2025) GBIF Occurrence Download https://doi.org/10.15468/dl.tzn8at
usmca_obs <- read_tsv("data/raw/gbif_data_amphibians_2000_2025.csv")

# Other non-neartic countries that neartic species could have obs in:
# GBIF.org (8 November 2025) GBIF Occurrence Download https://doi.org/10.15468/dl.96xayz
other_obs <- read_tsv("data/raw/central_america_amphibian_obs.csv")

all_obs <- rbind(usmca_obs, other_obs)

# Keep only species level observations
all_obs <- all_obs[which(all_obs$taxonRank=="SPECIES"), ]

# Keep only observations whose coordinate uncertainty doesn't exceed our gridsize
all_obs <- all_obs[which(all_obs$coordinateUncertaintyInMeters < 1000), ]

# Filter out caudates
all_obs <- all_obs[all_obs$order == "Anura",]
all_obs$order <- NULL

# remove observations from 2021-2025 per Ava's call since worldclim data is from 2000-2020
all_obs <- all_obs[all_obs$year<=2020,]

# Remove unhelpful columns
all_obs <- all_obs[,c("gbifID","species", "countryCode",
                                      "stateProvince", "decimalLatitude",
                                      "decimalLongitude","day","month", "year")]

# convert to shapefile
all_obs_sf <- st_as_sf(
  all_obs,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326 # the standard coord system
)

rm(other_obs, usmca_obs)




# 3. Process neartic shapefile
neartic <- read_sf("data/raw/tnc_terr_eco_realms.shp")

# visualize it
world <- ne_countries(scale="medium", returnclass="sf")

ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray60") +
  geom_sf(data = neartic, fill = "red", color = "black", alpha = 0.6) +
  theme_minimal() +
  coord_sf()




# 4. Find truly neartic species based on threshold of 90% of their obs being
# within the neartic realm

# Do they use the same crs?
st_crs(neartic)
st_crs(all_obs_sf) #yes

all_obs_sf$in_neartic <- lengths(st_intersects(all_obs_sf, neartic)) > 0

purely_neartic <- all_obs_sf[all_obs_sf$in_neartic==TRUE,]

# including all observations
all_species_counts <- all_obs_sf %>%
  group_by(species) %>%           # group by species
  summarise(observations = n())   # count rows per species
all_species_counts$geometry <- NULL
names(all_species_counts)[names(all_species_counts)=="observations"] <-
  "total_species_observations"

# just neartic observations
neartic_species_counts <- purely_neartic %>%
  group_by(species) %>%
  summarise(observations = n())
neartic_species_counts$geometry <- NULL
names(neartic_species_counts)[names(neartic_species_counts)=="observations"] <-
  "species_observations_within_neartic"

# join the dataframes
neartic_anuran_observation_comparison <-
  left_join(all_species_counts, neartic_species_counts, by="species")

neartic_anuran_observation_comparison <-
  neartic_anuran_observation_comparison[
    !is.na(neartic_anuran_observation_comparison$species_observations_within_neartic),
  ]

# only keep species where 90% of their observations are within the neartic
neartic_anuran_observation_comparison <-
  neartic_anuran_observation_comparison[
    (neartic_anuran_observation_comparison$species_observations_within_neartic/
    neartic_anuran_observation_comparison$total_species_observations) >=0.9,
  ]

# only keep species that have >100 obs total
neartic_anurans_obs <-
  neartic_anuran_observation_comparison[
    neartic_anuran_observation_comparison$total_species_observations >=100,
  ]

total_num_obs <- sum(neartic_anurans_obs$total_species_observations)
# total_num_obs = 150233


# 5. write out gbif data for remaining species
# remaining species have >100 total observations, 90% of which must be within
# the actual neartic realm

gbif_neartic_anuran_observations <-
  all_obs[all_obs$species %in% neartic_anurans_obs$species,]
number <- unique(gbif_neartic_anuran_observations$species) # 51 now

write.csv(gbif_neartic_anuran_observations, "data/processed/gbif_neartic_anuran_observations.csv")
