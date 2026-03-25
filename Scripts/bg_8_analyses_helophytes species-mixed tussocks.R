################################################################################
# Snowmaking reservoir revegetation with macrophytes
# Evolution of helophyte species-mixed tussocks 
# Author: Benjamin Gerfand
# Version : September 2025
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

# Identify species mixed tussocks
MEL <- unique(c(rownames(suivi2[suivi2$remarques=="melange",]),
                rownames(suivi3[str_detect(suivi3$remarques,"autre"),]),
                rownames(suivi4[str_detect(suivi4$remarques,"melange"),]),
                rownames(suivi5[str_detect(suivi5$remarques,"mélange"),])))

##############################################
#### Proportion of species-mixed tussocks ####
##############################################

length(MEL) # n = 64 tussocks that evolved in species-mixed tussocks
(length(MEL)/288)*100 # 22.2 % of all tussocks

####################################
#### Proportion of each species ####
####################################

# list the initial planted species in mixed tussocks
initial = suivi1[MEL,]$espece

length(which(initial == "Eriophorum angustifolium")) # number of E. angustifolium tussocks that became mixed
(length(which(initial == "Eriophorum angustifolium")) / length(initial))*100 # proportion of mixed tussocks that was initially E. angustifolium

length(which(initial == "Carex nigra")) # number of C. nigra tussocks that became mixed
(length(which(initial == "Carex nigra")) / length(initial))*100 # proportion of mixed tussocks that was initially E. angustifolium






