################################################################################
# Snowmaking reservoir revegetation with macrophytes
#
# Light and Oxygen sensors analyses
# Sensors "ArcsV" = positionned in vegetated plots
# Sensors "ArcsT" = positionned in control unvegetated zone
# sensors Arcs V1 - V2 - V3 - T1 = positionned on summer water level (helophytes)
# sensors Arcs V4 - V5 - V6 - T2 = positionned 40-60 cm deep summer water level (hydrophytes)
#
# Author: Benjamin Gerfand
# Version : September 2025
################################################################################

#### Packages ####
library(tidyverse)
library(ggplot2)
library(scales)

#########################
#### Import datasets ####
#########################

data_path <- "data_path"  # adapt to your project structure
data_path <- "D:/Thèse Gerfand/3 - Végétalisation Adret des Tuffes/Article végétalisation berges/Soumission/Ecological engineering/Supplementary material/Data/Data for R/Sensors"  # adapt to your project structure

sensors <- read.csv2(paste(data_path,"Capteurs_global.csv",sep="/"),sep=',',dec=".")

##########################
#### Prepare datasets ####
##########################

# Ensure the sensor column is a factor with a meaningful order
sensors$capteur <- factor(sensors$capteur, 
                          levels = c("ArcsV1","ArcsV2","ArcsV3","ArcsV4",
                                     "ArcsV5","ArcsV6","ArcsT1","ArcsT2"))

# Convert 'Date et heure' to POSIXct if not already
sensors$`Date et heure` <- as.POSIXct(sensors$Date.et.heure)

# Ensure numeric columns
sensors$`Temperature (deg C)` <- as.numeric(sensors$Temperature..deg.C.)
sensors$`Lumiere (lux)` <- as.numeric(sensors$Lumiere..lux.)

############################################
#### Plot global temperature and light  ####
############################################

# Temperature by sensor (superimposed)
ggplot(sensors, aes(x = `Date et heure`, y = `Temperature (deg C)`, color = capteur)) +
  geom_line(size = 1) +
  scale_x_datetime(date_labels = "%Y-%m-%d\n%H:%M") +
  xlab("Date and Time") +
  ylab("Temperature (°C)") +
  theme_minimal() +
  ggtitle("Temperature recorded by all sensors")

# Light by sensor (superimposed)
ggplot(sensors, aes(x = `Date et heure`, y = `Lumiere (lux)`, color = capteur)) +
  geom_line(size = 1) +
  scale_x_datetime(date_labels = "%Y-%m-%d\n%H:%M") +
  xlab("Date and Time") +
  ylab("Light (lux)") +
  theme_minimal() +
  ggtitle("Light recorded by all sensors")

###################################
#### Faceted plots per sensor  ####
###################################

# Temperature per sensor
ggplot(sensors, aes(x = `Date et heure`, y = `Temperature (deg C)`)) +
  geom_line(size = 1, color = "blue") +
  facet_wrap(~capteur) +
  scale_x_datetime(date_labels = "%Y-%m-%d") +
  xlab("Date and Time") +
  ylab("Temperature (°C)") +
  theme_minimal()

# Light per sensor
ggplot(sensors, aes(x = `Date et heure`, y = `Lumiere (lux)`)) +
  geom_line(size = 1, color = "orange") +
  facet_wrap(~capteur) +
  scale_x_datetime(date_labels = "%Y-%m-%d") +
  xlab("Date and Time") +
  ylab("Light (lux)") +
  theme_minimal()

##########################################################################
#### Define key periods : 2024 Exondation, summer, and winter periods ####
##########################################################################

### Hydrophyte spring exondation
# = Period with strong temp fluctuation after snowmelt (stable Temp = 0)

# Data extraction on the identifiedperiod
hydro_spring <- sensors %>%
  filter(capteur == "ArcsV5",
         `Date et heure` >= as.POSIXct("2024-05-29 00:00:00"),
         `Date et heure` <= as.POSIXct("2024-06-30 23:59:59"))

# Visualisation
ggplot(hydro_spring, aes(x = `Date et heure`, y = `Temperature (deg C)`)) +
  geom_line(color = "blue", size = 1) +
  scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 day") +
  xlab("Date") + ylab("Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Key figures
summary(hydro_spring$`Temperature (deg C)`) #max temperature ~50°C
summary(hydro_spring$`Lumiere (lux)`)
# Exondartion period duration = 25 days


### Helophyte spring exondation
helo_spring <- sensors %>%
  filter(capteur == "ArcsV2",
         `Date et heure` >= as.POSIXct("2024-05-26 00:00:00"),
         `Date et heure` <= as.POSIXct("2024-07-02 23:59:59"))

# Visualisation
ggplot(helo_spring, aes(x = `Date et heure`, y = `Temperature (deg C)`)) +
  geom_line(color = "blue", size = 1) +
  scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 day") +
  xlab("Date") + ylab("Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Key figures
summary(helo_spring$`Temperature (deg C)`) #max temperature ~45°C
summary(helo_spring$`Lumiere (lux)`)
# Exondartion period duration = 34 days


### Helophyte summer period
# = period after reservor filling july - september
helo_summer <- sensors %>%
  filter(capteur == "ArcsV2",
         `Date et heure` >= as.POSIXct("2024-07-03 00:00:00"),
         `Date et heure` <= as.POSIXct("2024-09-15 23:59:59"))

# Key figures
summary(helo_summer$`Temperature (deg C)`)
summary(helo_summer$`Lumiere (lux)`)


### Hydrophyte summer period
hydro_summer <- sensors %>%
  filter(capteur == "ArcsV5",
         `Date et heure` >= as.POSIXct("2024-07-03 00:00:00"),
         `Date et heure` <= as.POSIXct("2024-09-15 23:59:59"))

# Key figures
summary(hydro_summer$`Temperature (deg C)`)
summary(hydro_summer$`Lumiere (lux)`)


### Helophyte Winter 2023-2024
# = Period with stable temp around 0°C (under snow cover)
helo_winter <- sensors %>%
  filter(capteur == "ArcsV2",
         `Date et heure` >= as.POSIXct("2023-11-30 23:59:59"),
         `Date et heure` <= as.POSIXct("2024-05-01 23:59:59"))

# Key figures
summary(helo_winter$`Temperature (deg C)`)
summary(helo_winter$`Lumiere (lux)`)
