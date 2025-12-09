# ----------------------------------
# Authors: Ryan Hull
# Date: October 2025
# Purpose: process understand and break down canadian amphibian observations
# F25 McGill - BIOL 432 Data Project
# ----------------------------------




# 1. Libraries
library("readr")
library("dplyr")




# 2. Initial data processing
# Amphibian observations with location in Can/Usa/mexico from 2000-2025
# Citation for the download, with link giving info
# GBIF.org (02 October 2025) GBIF Occurrence Download https://doi.org/10.15468/dl.tzn8at
rm(list=ls())
gbif_amphibians <- read_tsv("data/gbif_data_amphibians_2000_2025.csv")

# Keep only species level observations
gbif_amphibians <- gbif_amphibians[which(gbif_amphibians$taxonRank=="SPECIES"), ]

# Keep only observations whose coordinate uncertainty doesn't exceed our gridsize
gbif_amphibians <- gbif_amphibians[which(gbif_amphibians$coordinateUncertaintyInMeters < 1000), ]

# Remove unhelpful columns
gbif_amphibians <- gbif_amphibians[,c("gbifID","species","countryCode","stateProvince",
                                      "decimalLatitude","decimalLongitude","day","month",
                                      "year")]

# Exclude cold season observations? Maybe needed if we use climate parameters specific to warm season
#?  months_to_keep <- c(5,6,7,8,9)
#?  gbif_amphibians <- gbif_amphibians[gbif_amphibians$month %in% months_to_keep,]




# 3. subset by species

# Caudata
ambystoma_jeffersonianum  <- gbif_amphibians[gbif_amphibians$species=="Ambystoma jeffersonianum",]
ambystoma_gracile <- gbif_amphibians[gbif_amphibians$species=="Ambystoma gracile",]
ambystoma_laterale <- gbif_amphibians[gbif_amphibians$species=="Ambystoma laterale",]
ambystoma_maculatum <- gbif_amphibians[gbif_amphibians$species=="Ambystoma maculatum",]
ambystoma_macrodactylum <- gbif_amphibians[gbif_amphibians$species=="Ambystoma macrodactylum",]
ambystoma_mavortium <- gbif_amphibians[gbif_amphibians$species=="Ambystoma mavortium",]
ambystoma_texanum <- gbif_amphibians[gbif_amphibians$species=="Ambystoma texanum",]
ambystoma_tigrinum <- gbif_amphibians[gbif_amphibians$species=="Ambystoma tigrinum",]
desmognathus_fuscus <- gbif_amphibians[gbif_amphibians$species=="Desmognathus fuscus",]
desmognathus_ochrophaeus <- gbif_amphibians[gbif_amphibians$species=="Desmognathus ochrophaeus",]
dicamptodon_tenebrosus <- gbif_amphibians[gbif_amphibians$species=="Dicamptodon tenebrosus",]
ensatina_eschscholtzii <- gbif_amphibians[gbif_amphibians$species=="Ensatina eschscholtzii",]
eurycea_bislineata <- gbif_amphibians[gbif_amphibians$species=="Eurycea bislineata",]
gyrinophilus_porphyriticus <- gbif_amphibians[gbif_amphibians$species=="Gyrinophilus porphyriticus",]
hemidactylium_scutatum <- gbif_amphibians[gbif_amphibians$species=="Hemidactylium scutatum",]
necturus_maculosus <- gbif_amphibians[gbif_amphibians$species=="Necturus maculosus",]
notophthalmus_viridescens <- gbif_amphibians[gbif_amphibians$species=="Notophthalmus viridescens",]
plethodon_cinereus <- gbif_amphibians[gbif_amphibians$species=="Plethodon cinereus",]
plethodon_idahoensis <- gbif_amphibians[gbif_amphibians$species=="Plethodon idahoensis",]
plethodon_vehiculum <- gbif_amphibians[gbif_amphibians$species=="Plethodon vehiculum",]
taricha_granulosa <- gbif_amphibians[gbif_amphibians$species=="Taricha granulosa",]

# Anura
acris_crepitans <- gbif_amphibians[gbif_amphibians$species=="Acris crepitans",]
ascaphus_truei <- gbif_amphibians[gbif_amphibians$species=="Ascaphus truei",]
ascaphus_montanus <- gbif_amphibians[gbif_amphibians$species=="Ascaphus montanus",]
anaxyrus_americanus <- gbif_amphibians[gbif_amphibians$species=="Anaxyrus americanus",]
anaxyrus_boreas <- gbif_amphibians[gbif_amphibians$species=="Anaxyrus boreas",]
anaxyrus_cognatus <- gbif_amphibians[gbif_amphibians$species=="Anaxyrus cognatus",]
anaxyrus_fowleri <- gbif_amphibians[gbif_amphibians$species=="Anaxyrus fowleri",]
anaxyrus_hemiophrys <- gbif_amphibians[gbif_amphibians$species=="Anaxyrus hemiophrys",]
hyla_chrysoscelis <- gbif_amphibians[gbif_amphibians$species=="Hyla chrysoscelis",]
hyla_versicolor <- gbif_amphibians[gbif_amphibians$species=="Hyla versicolor",]
rana_aurora <- gbif_amphibians[gbif_amphibians$species=="Rana aurora",]
rana_cascadae <- gbif_amphibians[gbif_amphibians$species=="Rana cascadae",]
rana_luteiventris <- gbif_amphibians[gbif_amphibians$species=="Rana luteiventris",]
rana_pretiosa <- gbif_amphibians[gbif_amphibians$species=="Rana pretiosa",]
lithobates_catesbeianus <- gbif_amphibians[gbif_amphibians$species=="Lithobates catesbeianus",]
lithobates_clamitans <- gbif_amphibians[gbif_amphibians$species=="Lithobates clamitans",]
lithobates_palustris <- gbif_amphibians[gbif_amphibians$species=="Lithobates palustris",]
lithobates_pipiens <- gbif_amphibians[gbif_amphibians$species=="Lithobates pipiens",]
lithobates_septentrionalis <- gbif_amphibians[gbif_amphibians$species=="Lithobates septentrionalis",]
lithobates_sylvaticus <- gbif_amphibians[gbif_amphibians$species=="Lithobates sylvaticus",]
pseudacris_crucifer <- gbif_amphibians[gbif_amphibians$species=="Pseudacris crucifer",]
pseudacris_maculata <- gbif_amphibians[gbif_amphibians$species=="Pseudacris maculata",]
pseudacris_regilla <- gbif_amphibians[gbif_amphibians$species=="Pseudacris regilla",]
pseudacris_triseriata <- gbif_amphibians[gbif_amphibians$species=="Pseudacris triseriata",]
spea_bombifrons <- gbif_amphibians[gbif_amphibians$species=="Spea bombifrons",]
spea_intermontana <- gbif_amphibians[gbif_amphibians$species=="Spea intermontana",]




# 4. analysis of number observations per species:
species_counts <- gbif_amphibians %>%
  group_by(species) %>%           # group by species
  summarise(observations = n())   # count rows per species
# note the above includes non canadian species. remove them:

canadian_species <- c("Acris crepitans","Ascaphus truei","Ascaphus montanus",
                      "Anaxyrus americanus","Anaxyrus boreas","Anaxyrus cognatus",
                      "Anaxyrus fowleri","Anaxyrus hemiophrys","Hyla chrysoscelis",
                      "Hyla versicolor","Rana aurora","Rana cascadae","Rana luteiventris",
                      "Rana pretiosa","Lithobates catesbeianus","Lithobates clamitans",
                      "Lithobates palustris","Lithobates pipiens","Lithobates septentrionalis",
                      "Lithobates sylvaticus","Pseudacris crucifer","Pseudacris maculata",
                      "Pseudacris regilla","Pseudacris triseriata","Spea bombifrons",
                      "Spea intermontana","Ambystoma jeffersonianum","Ambystoma gracile",
                      "Ambystoma laterale","Ambystoma maculatum","Ambystoma macrodactylum",
                      "Ambystoma mavortium","Ambystoma texanum","Ambystoma tigrinum",
                      "Desmognathus fuscus","Desmognathus ochrophaeus","Dicamptodon tenebrosus",
                      "Ensatina eschscholtzii","Eurycea bislineata","Gyrinophilus porphyriticus",
                      "Hemidactylium scutatum","Necturus maculosus","Notophthalmus viridescens",
                      "Plethodon cinereus","Plethodon idahoensis","Plethodon vehiculum",
                      "Taricha granulosa")
canadian_caudata <- c("Ambystoma jeffersonianum","Ambystoma gracile",
                      "Ambystoma laterale","Ambystoma maculatum","Ambystoma macrodactylum",
                      "Ambystoma mavortium","Ambystoma texanum","Ambystoma tigrinum",
                      "Desmognathus fuscus","Desmognathus ochrophaeus","Dicamptodon tenebrosus",
                      "Ensatina eschscholtzii","Eurycea bislineata","Gyrinophilus porphyriticus",
                      "Hemidactylium scutatum","Necturus maculosus","Notophthalmus viridescens",
                      "Plethodon cinereus","Plethodon idahoensis","Plethodon vehiculum",
                      "Taricha granulosa")
canadian_anura <- c("Acris crepitans","Ascaphus truei","Ascaphus montanus",
                    "Anaxyrus americanus","Anaxyrus boreas","Anaxyrus cognatus",
                    "Anaxyrus fowleri","Anaxyrus hemiophrys","Hyla chrysoscelis",
                    "Hyla versicolor","Rana aurora","Rana cascadae","Rana luteiventris",
                    "Rana pretiosa","Lithobates catesbeianus","Lithobates clamitans",
                    "Lithobates palustris","Lithobates pipiens","Lithobates septentrionalis",
                    "Lithobates sylvaticus","Pseudacris crucifer","Pseudacris maculata",
                    "Pseudacris regilla","Pseudacris triseriata","Spea bombifrons",
                    "Spea intermontana")

canadian_species_counts <- species_counts[species_counts$species %in% canadian_species, ]
canadian_anuran_counts <- species_counts[species_counts$species %in% canadian_anura,]
canadian_caudate_counts <- species_counts[species_counts$species %in% canadian_caudata,]



# 5. Basic analysis of the current range of canadian anurans
canadian_anuran_approx_latitudes = 
  c("Acris crepitans" = "south",
    "Ascaphus truei" = "mid",
    "Ascaphus montanus" = "mid",
    "Anaxyrus americanus" = "mid",
    "Anaxyrus boreas" = "mid",
    "Anaxyrus cognatus" = "south",
    "Anaxyrus fowleri" = "south",
    "Anaxyrus hemiophrys" = "mid",
    "Hyla chrysoscelis" = "south",
    "Hyla versicolor" = "south",
    "Rana aurora" = "mid",
    "Rana cascadae" = "mid",
    "Rana luteiventris" = "mid",
    "Rana pretiosa" = "mid",
    "Lithobates catesbeianus" = "south",
    "Lithobates clamitans" = "mid",
    "Lithobates palustris" = "mid",
    "Lithobates pipiens" = "north",
    "Lithobates septentrionalis" = "north",
    "Lithobates sylvaticus" = "north",
    "Pseudacris crucifer" = "mid",
    "Pseudacris maculata" = "mid",
    "Pseudacris regilla" = "mid",
    "Pseudacris triseriata" = "mid",
    "Spea bombifrons" = "south",
    "Spea intermontana" = "mid")

canadian_anuran_approx_longitudes = 
  c("Acris crepitans" = "east",
    "Ascaphus truei" = "west",
    "Ascaphus montanus" = "west",
    "Anaxyrus americanus" = "east",
    "Anaxyrus boreas" = "west",
    "Anaxyrus cognatus" = "mid",
    "Anaxyrus fowleri" = "east",
    "Anaxyrus hemiophrys" = "mid",
    "Hyla chrysoscelis" = "east",
    "Hyla versicolor" = "east",
    "Rana aurora" = "west",
    "Rana cascadae" = "west",
    "Rana luteiventris" = "west",
    "Rana pretiosa" = "west",
    "Lithobates catesbeianus" = "east",
    "Lithobates clamitans" = "east",
    "Lithobates palustris" = "east",
    "Lithobates pipiens" = "east",
    "Lithobates septentrionalis" = "east",
    "Lithobates sylvaticus" = "east",
    "Pseudacris crucifer" = "east",
    "Pseudacris maculata" = "mid",
    "Pseudacris regilla" = "west",
    "Pseudacris triseriata" = "east",
    "Spea bombifrons" = "mid",
    "Spea intermontana" = "west")

canadian_anuran_locations <- canadian_anuran_counts

canadian_anuran_locations$approx_latitude <- 
  canadian_anuran_approx_latitudes[canadian_anuran_locations$species]

canadian_anuran_locations$approx_longitude <- 
  canadian_anuran_approx_longitudes[canadian_anuran_locations$species]




# 6. Basic analysis of current locations of canadian caudates:

canadian_caudate_approx_latitudes =
  c("Ambystoma jeffersonianum" = "mid",
    "Ambystoma gracile" = "mid",
    "Ambystoma laterale" = "north",
    "Ambystoma maculatum" = "mid",
    "Ambystoma macrodactylum" = "mid",
    "Ambystoma mavortium" = "mid",
    "Ambystoma texanum" = "south",
    "Ambystoma tigrinum" = "south",
    "Desmognathus fuscus" = "south",
    "Desmognathus ochrophaeus" = "mid",
    "Dicamptodon tenebrosus" = "mid",
    "Ensatina eschscholtzii" = "south",
    "Eurycea bislineata" = "north",
    "Gyrinophilus porphyriticus" = "south",
    "Hemidactylium scutatum" = "south",
    "Necturus maculosus" = "south",
    "Notophthalmus viridescens" = "mid",
    "Plethodon cinereus" = "mid",
    "Plethodon idahoensis" = "mid",
    "Plethodon vehiculum" = "mid",
    "Taricha granulosa" = "mid")

canadian_caudate_approx_longitudes =
  c("Ambystoma jeffersonianum" = "east",
    "Ambystoma gracile" = "west",
    "Ambystoma laterale" = "east",
    "Ambystoma maculatum" = "east",
    "Ambystoma macrodactylum" = "west",
    "Ambystoma mavortium" = "mid",
    "Ambystoma texanum" = "east",
    "Ambystoma tigrinum" = "east",
    "Desmognathus fuscus" = "east",
    "Desmognathus ochrophaeus" = "east",
    "Dicamptodon tenebrosus" = "west",
    "Ensatina eschscholtzii" = "west",
    "Eurycea bislineata" = "east",
    "Gyrinophilus porphyriticus" = "east",
    "Hemidactylium scutatum" = "east",
    "Necturus maculosus" = "east",
    "Notophthalmus viridescens" = "east",
    "Plethodon cinereus" = "east",
    "Plethodon idahoensis" = "west",
    "Plethodon vehiculum" = "west",
    "Taricha granulosa" = "west")
  
canadian_caudate_locations <- canadian_caudate_counts

canadian_caudate_locations$approx_latitude <- 
  canadian_caudate_approx_latitudes[canadian_caudate_locations$species]

canadian_caudate_locations$approx_longitude <- 
  canadian_caudate_approx_longitudes[canadian_caudate_locations$species]



# 7. Writing to new files if wanted
# Use any of the following to Write out species of interest
# caudata
write.csv(ambystoma_jeffersonianum, "data/ambystoma_jeffersonianum.csv", row.names = FALSE)
write.csv(ambystoma_gracile, "data/ambystoma_gracile.csv", row.names = FALSE)
write.csv(ambystoma_laterale, "data/ambystoma_laterale.csv", row.names = FALSE)
write.csv(ambystoma_maculatum, "data/ambystoma_maculatum.csv", row.names = FALSE)
write.csv(ambystoma_macrodactylum, "data/ambystoma_macrodactylum.csv", row.names = FALSE)
write.csv(ambystoma_mavortium, "data/ambystoma_mavortium.csv", row.names = FALSE)
write.csv(ambystoma_texanum, "data/ambystoma_texanum.csv", row.names = FALSE)
write.csv(ambystoma_tigrinum, "data/ambystoma_tigrinum.csv", row.names = FALSE)
write.csv(desmognathus_fuscus, "data/desmognathus_fuscus.csv", row.names = FALSE)
write.csv(desmognathus_ochrophaeus, "data/desmognathus_ochrophaeus.csv", row.names = FALSE)
write.csv(dicamptodon_tenebrosus, "data/dicamptodon_tenebrosus.csv", row.names = FALSE)
write.csv(ensatina_eschscholtzii, "data/ensatina_eschscholtzii.csv", row.names = FALSE)
write.csv(eurycea_bislineata, "data/eurycea_bislineata.csv", row.names = FALSE)
write.csv(gyrinophilus_porphyriticus, "data/gyrinophilus_porphyriticus.csv", row.names = FALSE)
write.csv(hemidactylium_scutatum, "data/hemidactylium_scutatum.csv", row.names = FALSE)
write.csv(necturus_maculosus, "data/necturus_maculosus.csv", row.names = FALSE)
write.csv(notophthalmus_viridescens, "data/notophthalmus_viridescens.csv", row.names = FALSE)
write.csv(plethodon_cinereus, "data/plethodon_cinereus.csv", row.names = FALSE)
write.csv(plethodon_idahoensis, "data/plethodon_idahoensis.csv", row.names = FALSE)
write.csv(plethodon_vehiculum, "data/plethodon_vehiculum.csv", row.names = FALSE)
write.csv(taricha_granulosa, "data/taricha_granulosa.csv", row.names = FALSE)

# anura
write.csv(acris_crepitans, "data/acris_crepitans.csv", row.names = FALSE)
write.csv(ascaphus_truei, "data/ascaphus_truei.csv", row.names = FALSE)
write.csv(ascaphus_montanus, "data/ascaphus_montanus.csv", row.names = FALSE)
write.csv(anaxyrus_americanus, "data/anaxyrus_americanus.csv", row.names = FALSE)
write.csv(anaxyrus_boreas, "data/anaxyrus_boreas.csv", row.names = FALSE)
write.csv(anaxyrus_cognatus, "data/anaxyrus_cognatus.csv", row.names = FALSE)
write.csv(anaxyrus_fowleri, "data/anaxyrus_fowleri.csv", row.names = FALSE)
write.csv(anaxyrus_hemiophrys, "data/anaxyrus_hemiophrys.csv", row.names = FALSE)
write.csv(hyla_chrysoscelis, "data/hyla_chrysoscelis.csv", row.names = FALSE)
write.csv(hyla_versicolor, "data/hyla_versicolor.csv", row.names = FALSE)
write.csv(rana_aurora, "data/rana_aurora.csv", row.names = FALSE)
write.csv(rana_cascadae, "data/rana_cascadae.csv", row.names = FALSE)
write.csv(rana_luteiventris, "data/rana_luteiventris.csv", row.names = FALSE)
write.csv(rana_pretiosa, "data/rana_pretiosa.csv", row.names = FALSE)
write.csv(lithobates_catesbeianus, "data/lithobates_catesbeianus.csv", row.names = FALSE)
write.csv(lithobates_clamitans, "data/lithobates_clamitans.csv", row.names = FALSE)
write.csv(lithobates_palustris, "data/lithobates_palustris.csv", row.names = FALSE)
write.csv(lithobates_pipiens, "data/lithobates_pipiens.csv", row.names = FALSE)
write.csv(lithobates_septentrionalis, "data/lithobates_septentrionalis.csv", row.names = FALSE)
write.csv(lithobates_sylvaticus, "data/lithobates_sylvaticus.csv", row.names = FALSE)
write.csv(pseudacris_crucifer, "data/pseudacris_crucifer.csv", row.names = FALSE)
write.csv(pseudacris_maculata, "data/pseudacris_maculata.csv", row.names = FALSE)
write.csv(pseudacris_regilla, "data/pseudacris_regilla.csv", row.names = FALSE)
write.csv(pseudacris_triseriata, "data/pseudacris_triseriata.csv", row.names = FALSE)
write.csv(spea_bombifrons, "data/spea_bombifrons.csv", row.names = FALSE)
write.csv(spea_intermontana, "data/spea_intermontanawrite.csv", row.names = FALSE)