################################################################################
# Snowmaking reservoir revegetation with macrophytes
# Vegetation colonization & inventories in reservoir and regional species pool
# Author: Benjamin Gerfand
# Version : September 2025
################################################################################

#### Packages ####
library(tidyverse)

#########################
#### Import datasets ####
#########################

data_path <- "data_path"   # adapt to project structure (file = "Data_for_R")

veg_res <- read.csv2(paste(data_path,"bg_List_vegetation_species_reservoir.csv",sep="/"),sep=';',dec=",")
veg_reg <- read.csv2(paste(data_path,"bg_List_regional_vegetation_species.csv",sep="/"),sep=';',dec=",")

############################################
#### Remove voluntarily planted species ####
############################################

# Only keep species that colonised the plots
vegres_clean <- veg_res %>%
  filter(is.na(espece.plantee) | espece != espece.plantee)

###############################################
#### Species richness by location and type ####
###############################################

# Location = riprap or vegetated plots
# Type = terrestrial, macrophytes, undetermined

species_summary <- vegres_clean %>%
  group_by(localisation, type) %>%
  summarise(
    n_species = n_distinct(espece),
    .groups="drop"
  )

species_summary

####################################
#### List of colonising species ####
####################################

colonising_species <- vegres_clean %>%
  distinct(espece, localisation, type) %>%
  arrange(type, espece)

colonising_species

#############################################
#### Total species richness on reservoir ####
#############################################

total_species_reservoir <- vegres_clean %>%
  summarise(n_species = n_distinct(espece))

total_species_reservoir

###############################################
#### Total species richness per plant type ####
###############################################

species_by_type <- vegres_clean %>%
  group_by(type) %>%
  summarise(
    n_species = n_distinct(espece),
    .groups="drop"
  )

species_by_type

##########################################
#### Regional macrophyte species pool ####
##########################################

regional_species_total <- veg_reg %>%
  summarise(n_species = n_distinct(espece))

regional_species_total

#############################################
#### Frequency of species in waterbodies ####
#############################################

# Number of waterbodies where each species occurs

regional_frequency <- veg_reg %>%
  group_by(espece) %>%
  summarise(
    n_waterbodies = n_distinct(numero.PE),
    .groups="drop"
  ) %>%
  arrange(desc(n_waterbodies))

regional_frequency

######################################################################
#### Species shared between reservoir and surrounding waterbodies ####
######################################################################

shared_species <- intersect(
  unique(vegres_clean$espece),
  unique(veg_reg$espece)
)

shared_species
length(shared_species)
# Potential colonisers

##########################################################
#### Check cross-colonisation between planted species ####
##########################################################

# Carex nigra observed on plots planted with Eriophorum
carex_in_EA <- vegres_clean %>%
  filter(espece == "Carex nigra",
         espece.plantee == "Eriophorum angustifolium") %>%
  distinct(zone)

carex_in_EA # show the ID number of the vegetated plots
nrow(carex_in_EA) # number of vegetated plots

# Eriophorum angustifolium observed on plots planted with Carex
EA_in_carex <- vegres_clean %>%
  filter(espece == "Eriophorum angustifolium",
         espece.plantee == "Carex nigra") %>%
  distinct(zone)

EA_in_carex # show the ID number of the vegetated plots
nrow(EA_in_carex) # number of vegetated plots


