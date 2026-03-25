################################################################################
# Snowmaking reservoir revegetation with macrophytes
#
# This script prepares the datasets used for:
# 1) Vertical and horizontal growth analyses
# 2) Clonal propagation and sexual reproduction analyses
# Two cleaned datasets are exported at the end of the script.
#
# Author: Benjamin Gerfand
# Version : March 2026
################################################################################

#### Packages ####
library(tidyverse)

####################################
#### Import monitoring datasets ####
####################################

data_path <- "data_path"   # adapt to project structure (file = "Raw_data")

suivi1 <- read.csv2(paste(data_path,"bg_helophytes_suivi1_02-09-23.csv",sep="/"),sep=";",dec=",")
suivi2 <- read.csv2(paste(data_path,"bg_helophytes_suivi2_27-06-24.csv",sep="/"),sep=";",dec=",")
suivi3 <- read.csv2(paste(data_path,"bg_helophytes_suivi3_03-09-24.csv",sep="/"),sep=";",dec=",")
suivi4 <- read.csv2(paste(data_path,"bg_helophytes_suivi4_30-06-25.csv",sep="/"),sep=";",dec=",")
suivi5 <- read.csv2(paste(data_path,"bg_helophytes_suivi5_01-09-25.csv",sep="/"),sep=";",dec=",")

######################################################
#### Identify species-mixed and inverted tussocks ####
######################################################

# During the experiment, several tussocks turned into species-mixed tussocks
# or into the wrong species (mistake or competitive exclusion)

# Identify species mixed tussocks
MEL <- unique(c(rownames(suivi2[suivi2$remarques=="melange",]),
                rownames(suivi3[str_detect(suivi3$remarques,"autre"),]),
                rownames(suivi4[str_detect(suivi4$remarques,"melange"),]),
                rownames(suivi5[str_detect(suivi5$remarques,"mélange"),])))
length(MEL) # n = 64 tussocks that evolved in species-mixed tussocks
# We will exclude all species-mixed tussocks from the analyses

# Identify inverted species tussocks
INV <- unique(c(rownames(suivi2[suivi2$remarques=="Carex",]),
                rownames(suivi3[str_detect(suivi3$remarques,"carex uniquement"),]),
                rownames(suivi4[str_detect(suivi4$remarques,"carex uniquement"),]),
                rownames(suivi5[str_detect(suivi5$remarques,"carex uniquement"),])))
length(INV) # n = 19 Eriophorum tussocks evoled into Carex tussocks during the experiment
# We will modify the species name of inverted tussocks

intersect(INV, MEL) # Among the 19 inverted tussocks, 17 were also observed in species-mixed configuration during the experiment
setdiff(INV, MEL) # Only 2 tussocks can actually be defined as inverted ("204" and "69")

# Final lists
INV2 = c("69", "204") # 2 inverted tussocks
MEL2 = MEL # n = 64 species-mixed tussocks

##################################
#### Correct inverted species ####
##################################

dfs <- c("suivi2","suivi3","suivi4","suivi5")

for(name in dfs){
  
  tmp <- get(name)
  tmp[INV2,"espece"] <- "Carex nigra"
  
  assign(name,tmp)
}

###################################
#### Merge monitoring datasets ####
###################################

suivis <- list(suivi1,suivi2,suivi3,suivi4,suivi5)

for(i in seq_along(suivis)){
  suivis[[i]]$numero_suivi <- i
}

suivi <- bind_rows(suivis)

suivi$date <- as.Date(suivi$date,"%d/%m/%Y")
suivi$annee <- format(suivi$date,"%Y")

#######################################
#### Remove species-mixed tussocks ####
#######################################

exclude_ids <- unique(suivi2[MEL2,]$ID)

suivi <- suivi %>%
  filter(!(ID %in% exclude_ids))

suivi <- suivi %>%
  filter(espece != "")


#############################################################
#############################################################
#### DATASET 1 — Growth analyses (vertical & horizontal) ####
#############################################################
#############################################################

# Dead tussocks are removed regardless of offshoot production

ids_mortality_growth <- c(
  "CN36","CN101","CN118","CN137",
  "EA144","CN1","EA28","EA39",
  "EA52","CN85","CN134","EA143"
)

growth_dataset <- suivi %>%
  filter(!ID %in% ids_mortality_growth)


################################################################
################################################################
#### DATASET 2 — Propagation & sexual reproduction analyses ####
################################################################
################################################################

# Dead tussocks are removed only if they produced no new shoots

ids_mortality_propagation <- c(
  "CN36","CN101","CN118","CN137","EA144"
)

spread_dataset <- suivi %>%
  filter(!ID %in% ids_mortality_propagation)

# Plantation survey = 0 offshoots

spread_dataset$nombre.reprises[
  spread_dataset$numero_suivi == 1
] <- 0


#################################
#### Export cleaned datasets ####
#################################

# Set export location
data_exp <- "data_export_location"   # adapt to project structure
data_exp <- "D:/Thèse Gerfand/3 - Végétalisation Adret des Tuffes/Article végétalisation berges/Soumission/Ecological engineering/Supplementary material/Data/Data for R"  

# Export
write.csv(
  growth_dataset,
  paste(data_exp,"bg_helophytes_growth_dataset.csv",sep="/"),
  row.names = FALSE
)

write.csv(
  spread_dataset,
  paste(data_exp,"bg_helophytes_spread_dataset.csv",sep="/"),
  row.names = FALSE
)


