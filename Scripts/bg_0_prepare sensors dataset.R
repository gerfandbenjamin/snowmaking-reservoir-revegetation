################################################################################
# Snowmaking reservoir revegetation with macrophytes
# This script prepares the datasets used for luminosity/oxygen sensors analyses
# Author: Benjamin Gerfand
# Version : September 2025
################################################################################

#### Packages ####
library(readxl)
library(tidyverse)

#########################
#### Import datasets ####
#########################

# Import and export path
data_path   <- "D:/Thèse Gerfand/3 - Végétalisation Adret des Tuffes/Article végétalisation berges/Soumission/Ecological engineering/Supplementary material/Data/Raw data/Sensors"
export_path <- "D:/Thèse Gerfand/3 - Végétalisation Adret des Tuffes/Article végétalisation berges/Soumission/Ecological engineering/Supplementary material/Data/Data for R/Sensors"

# Create export directory
dir.create(export_path, showWarnings = FALSE, recursive = TRUE)

# List XLSX raw files
fichiers_xlsx <- list.files(data_path, pattern = "\\.xlsx$", full.names = TRUE)

# Function to import and prepare each file
for (fichier in fichiers_xlsx) {
  
  dataframe <- as.data.frame(read_xlsx(fichier)) # Import XLSX files as dataframe
  nom_objet = substr(basename(fichier), 1, 6) # sensor name
  
  # Formating
  dataframe[,4] = format(dataframe[,4], scientific=FALSE, digits=2)
  dataframe = dataframe[ , -1]
  colnames(dataframe) = c("Date et heure", "Temperature (deg C)", "Lumiere (lux)")
  dataframe$capteur = nom_objet
  
  assign(nom_objet, dataframe) # Create object in environment
}

##########################
#### Merge dataframes ####
##########################

# List of sensor objects imported
capteurs_list <- mget(ls(pattern = "^Arcs"))  # named with "Arcs"

# Merging into global dataframe
Capteurs <- bind_rows(capteurs_list)

# Column conversion into numeric
Capteurs$`Lumiere (lux)` <- as.numeric(Capteurs$`Lumiere (lux)`)
Capteurs$`Temperature (deg C)` <- as.numeric(Capteurs$`Temperature (deg C)`)

# Delete the beginning of recording if necessary
Capteurs <- Capteurs %>%
  filter(`Date et heure` > "2023-07-21 00:00:00")


#####################
#### Data export ####
#####################

# Export CSV
write.csv(Capteurs, file = file.path(export_path, "Capteurs_global.csv"), row.names = FALSE)











