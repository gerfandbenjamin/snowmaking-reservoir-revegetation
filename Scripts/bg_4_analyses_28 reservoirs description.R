################################################################################
# Ecological quality of snowmaking reservoirs (French Alps)
# Embankment design and uses of snowmaking reservoirs
# Author: Benjamin Gerfand
# Version : August 2023
################################################################################

library(tidyverse)

################################################################################
# Data import
################################################################################

# Define data folder
data_path <- "data_path"  # adapt to your project structure

# Import datasets
TEtot <- read.csv2(paste(data_path,"compil_TE$tot.csv",sep="/"),sep=';',dec=",")

bio_exp <- read.csv2(paste(data_path,"bg_bio_exp.csv",sep="/"),
                     sep=";", dec=",")

# Remove altiport reservoir (R2) which is not part of the study dataset
bio_exp <- bio_exp %>%
  filter(nom != "R2")

# Number of reservoirs analysed
n_reservoirs <- nrow(bio_exp)
n_reservoirs

################################################################################
# Embankment design and GLS coverage
################################################################################

# Frequency of GLS coverage types
table_GLS <- table(bio_exp$conf)

table_GLS

# Convert to percentages
prop_GLS <- prop.table(table_GLS) * 100
round(prop_GLS,1)

################################################################################
# Embankment slope statistics
################################################################################

# Summary statistics for embankment slope angle
summary(bio_exp$pente_moy)
sd(bio_exp$pente_moy)

# Minimum and maximum slopes
min_slope <- min(bio_exp$pente_moy, na.rm = TRUE)
max_slope <- max(bio_exp$pente_moy, na.rm = TRUE)

min_slope
max_slope

################################################################################
# Number of reservoirs with steep embankments
################################################################################

# Count reservoirs with slopes steeper than a given threshold of 18°

steep_threshold <- 18

n_steep <- sum(bio_exp$pente_moy > steep_threshold, na.rm = TRUE)

n_steep

################################################################################
# Comparison of embankment slopes depending on GLS coverage
################################################################################

# Create a binary variable: presence/absence of cover layer
bio_exp <- bio_exp %>%
  mutate(GLS_presence = ifelse(conf == "aucun","No cover","Cover"))

# Mean slope per group
bio_exp %>%
  group_by(GLS_presence) %>%
  summarise(
    mean_slope = mean(pente_moy, na.rm = TRUE),
    sd_slope = sd(pente_moy, na.rm = TRUE),
    n = n()
  )

wilcox.test(pente_moy ~ GLS_presence, data = bio_exp)

# Plot: embankment slope depending on GLS coverage
boxplot(pente_moy ~ GLS_presence,
        data = bio_exp,
        col = c("lightgrey","steelblue"),
        ylab = "Mean upstream embankment slope (°)",
        xlab = "GLS coverage")

################################################################################
# Reservoir uses
################################################################################

# Dataset with reservoir uses details
uses = unique(data.frame(TEtot$nom, TEtot$annee, TEtot$usages))

# Remove altiport reservoir (R2) which is not part of the study dataset
uses <- uses %>%
  filter(TEtot.nom != "R2")

# Repartition of uses
table(uses$TEtot.usages)

# Lnks between uses and ages
table(uses$TEtot.annee, uses$TEtot.usages)


