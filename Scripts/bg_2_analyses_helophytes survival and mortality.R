################################################################################
# Snowmaking reservoir revegetation with macrophytes
# Helophyte survival and mortality events
# Author: Benjamin Gerfand
# Version : September 2025
################################################################################

#### Packages ####
library(tidyverse)
library(rstatix)

#########################
#### Import datasets ####
#########################

data_path <- "data_path"  # adapt to your project structure (file = "Raw data")
data_path <- "D:/Thèse Gerfand/3 - Végétalisation Adret des Tuffes/Article végétalisation berges/Soumission/Ecological engineering/Supplementary material/Data/Raw data"  # adapt to your project structure

root <- read.csv2(paste(data_path,"bg_helophytes_root_plantations.csv",sep="/"),sep=';',dec=",")
alti <- read.csv2(paste(data_path,"bg_helophytes_altitude.csv",sep="/"),sep=';',dec=",")
suivi1 <- read.csv2(paste(data_path,"bg_helophytes_suivi1_02-09-23.csv",sep="/"),sep=';',dec=",")
suivi2 <- read.csv2(paste(data_path,"bg_helophytes_suivi2_27-06-24.csv",sep="/"),sep=';',dec=",")
suivi3 <- read.csv2(paste(data_path,"bg_helophytes_suivi3_03-09-24.csv",sep="/"),sep=';',dec=",")
suivi4 <- read.csv2(paste(data_path,"bg_helophytes_suivi4_30-06-25.csv",sep="/"),sep=';',dec=",")
suivi5 <- read.csv2(paste(data_path,"bg_helophytes_suivi5_01-09-25.csv",sep="/"),sep=';',dec=",")

##########################################################################
#### Identify dead tussocks at the end of the experiment (Sept. 2025) ####
##########################################################################

# A tussock is considered dead if all measured traits are equal to zero
suivi5 <- suivi5 %>%
  mutate(
    survival = ifelse(
      longueur.touffe == 0 &
        largeur.touffe == 0 &
        hauteur.max.feuille == 0 &
        nombre.reprises == 0 &
        inflorescence == "non",
      0,   # dead
      1    # alive
    )
  )

suivi5[suivi5$survival == 0,] # show data relative to dead individuals

#############################################################################
#### Survival rates by species at the end of the experiment (Sept. 2025) ####
#############################################################################

survival_summary <- suivi5 %>%
  group_by(espece) %>%
  summarise(
    total = n(),
    dead = sum(survival == 0),
    alive = sum(survival == 1),
    survival_rate = round(100 * alive / total, 1)
  )

survival_summary
# Carex nigra : 4 / 144 dead → 97.2 % survival
# Eriophorum angustifolium : 1 / 144 dead → 99.3 % survival

##########################################################
#### Timing of mortality events during the experiment ####
##########################################################

monitoring_list <- list(suivi1, suivi2, suivi3, suivi4, suivi5)
dates <- as.Date(c("2023-09-20","2024-06-27","2024-09-04","2025-06-30","2025-09-01"))

classify_status <- function(df){
  
  df %>%
    mutate(
      status = case_when(
        longueur.touffe == 0 &
          largeur.touffe == 0 &
          hauteur.max.feuille == 0 &
          nombre.reprises == 0 &
          inflorescence == "non" ~ "dead",
        TRUE ~ "alive"
      )
    )
}


status_all <- map2_dfr(monitoring_list, dates, function(df, d){
  
  classify_status(df) %>%
    select(ID, espece, status) %>%
    mutate(date = d)
})


first_death <- status_all %>%
  filter(status == "dead") %>%
  group_by(ID, espece) %>%
  summarise(first_death_date = min(date), .groups="drop")

first_death
# Individuals EA143, EA52 and EA53 (Eriophorum) were considered dead during the experiment but regrew afterwards
# 1 C. nigra and 1 E. angustifolium died in September 2024
# 3 C. nigra died in June 2025


###############################################
#### Effect of species on mortality events ####
###############################################

chisq.test(table(suivi5$survival, suivi5$espece))
kruskal.test(survival ~ espece, data = suivi5)

#######################################################################
#### Effect of plant position along coir rolls on mortality events ####
#######################################################################

# Creation of 2 categories of position : at the extremeties or the center of coir rolls
suivi5 <- suivi5 %>%
  mutate(
    position = case_when(
      numero %in% 1:3 ~ "left",
      numero %in% 4:6 ~ "center",
      numero %in% 7:9 ~ "right"
    ),
    
    extremity = ifelse(position %in% c("left","right"),"extremity","center")
  )


# Fisher exact test
fisher_position <- fisher.test(table(suivi5$survival, suivi5$extremity))
fisher_position

########################################################################
#### Effect of coir rolls position within plots on mortality events ####
########################################################################

# Roll 1 or 2 
fisher_fascine <- fisher.test(table(suivi5$survival, suivi5$fascine))
fisher_fascine

###################################################################################
#### Effect of altimetric position relative to water level on mortality events ####
###################################################################################

suivi5 <- suivi5 %>%
  left_join(alti, by = c("placette","fascine")) %>%
  mutate(
    elevation_local = case_when(
      position == "left" ~ cote.alti.gauche,
      position == "center" ~ cote.alti.centre,
      position == "right" ~ cote.alti.droite
    )
  )

wilcox_elevation <- wilcox.test(elevation_local ~ survival, data = suivi5)
wilcox_elevation

#####################################################################
#### Effect of initial root volume implanted on mortality events ####
#####################################################################

# Cylindrical root volume
root <- root %>%
  mutate(
    base = pi * (0.5 * diametre.racinaire)^2,
    root_volume = base * hauteur.racinaire
  )

survival_status <- suivi5 %>%
  select(ID, survival)

data_root <- root %>%
  left_join(survival_status, by = "ID")

kruskal_root <- kruskal.test(root_volume ~ survival, data = data_root)
kruskal_root

################################
#### Descriptive statistics ####
################################

# average altitude position of living tussocks
mean_alive_altitude <- mean(suivi5$elevation_local[suivi5$survival == 1], na.rm=TRUE)

# average altitude position of dead tussocks
mean_dead_altitude <- mean(suivi5$elevation_local[suivi5$survival == 0], na.rm=TRUE)

# Location of dead tussocks
dead_positions <- suivi5 %>%
  filter(survival == 0)

dead_positions

# All 5 dead tussocks were located:
# - on the lowest fascine of their plot
# - in the three peripheral planting cavities
# - some slightly lower than the mean elevation of surviving tussocks
