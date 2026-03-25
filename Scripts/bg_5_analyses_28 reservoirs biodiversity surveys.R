################################################################################
# Ecological quality of snowmaking reservoirs (French Alps)
# Presence of biodiversity and mortality in snowmaking reservoirs
# Author: Benjamin Gerfand
# Version : October 2023
################################################################################

library(tidyverse)
library(flextable)

################################################################################
# Import datasets
################################################################################

data_path <- "data_path"  # adapt to your project structure

# Explanatory variables (reservoir characteristics)
bio_exp <- read.csv2(paste(data_path,"bg_bio_exp.csv",sep="/"),sep=';',dec=",")

# Biodiversity indicators (abundance / mortality scores)
bio_ind <- read.csv2(paste(data_path,"bg_bio_ind.csv",sep="/"),sep=';',dec=",")

# Presence / absence data
bio_pres <- read.csv2(paste(data_path,"bg_bio_pres.csv",sep="/"),sep=';',dec=",")

# Species richness data
bio_sp <- read.csv2(paste(data_path,"bg_bio_sp.csv",sep="/"),sep=';',dec=",")
bio_tab <- read.csv2(paste(data_path,"bg_bio_tab.csv",sep="/"),sep=';',dec=",")


# Remove altiport reservoir R2 (not part of the analysis)
bio_exp  <- bio_exp  %>% filter(nom != "R2")
bio_ind  <- bio_ind  %>% filter(nom != "R2")
bio_pres <- bio_pres %>% filter(nom != "R2")
bio_sp   <- bio_sp   %>% filter(nom != "R2")

# Number of reservoirs surveyed
n_reservoirs <- nrow(bio_exp)

################################################################################
# Vegetation presence
################################################################################

# Terrestrial vegetation presence
perc_veg_terr <- mean(bio_pres$pres_veg == 1) * 100 # in 82% of the visited reservoirs

# Reservoirs hosting more than 5 plant species
perc_veg_rich <- mean(bio_sp$veg_nsp == ">5") * 100 # 57%

# Aquatic vegetation presence (= flooded terrestrial species)
perc_veg_aqua <- mean(bio_ind$veg_eau > 0) * 100 # 18%

################################################################################
# Faunal biodiversity occurrence
################################################################################

# Number of reservoirs in which were found each group
bio_tab$taxon = c("Frogs", "Toads", "Newts", "Amphibians", "Mortality", "Reproduction",
                  "Macroinvertebrates", 
                  "Aquatic vegetation", "Terrestrial vegetation", "Vegetation belt", "Vegetation belt+",
                  "Fishes", "Salmonids")

flextable(bio_tab, cwidth = c(2,1))

# Create a variable indicating if any spontaneous fauna was observed
bio_ind$fauna_presence <- with(bio_ind,
                               gren_fav + trit_fav + crap_fav +
                                 odo_fav + macroinv_fav)

# Reservoirs hosting at least one faunal group
bio_ind$fauna_presence <- bio_ind$fauna_presence > 0

# Percentage of reservoirs hosting biodiversity
perc_biodiversity <- mean(bio_ind$fauna_presence) * 100

perc_biodiversity # biodiversity in 93% of surveyed reservoirs

################################################################################
# Amphibians
################################################################################

# Amphibians presence
perc_amph <- mean(bio_pres$pres_amph == 1) * 100 # in 68% of the visited reservoirs

# Individual amphibian taxa
perc_frogs <- mean(bio_pres$pres_gren == 1) * 100 # 50%
perc_toads <- mean(bio_pres$pres_crap == 1) * 100 # 36%
perc_newts <- mean(bio_pres$pres_trit == 1) * 100 # 39%

# Amphibian reproduction
perc_frog_repro <- mean(rowSums(bio_ind[,c("gren_oeuf","gren_tet")]) > 0) * 100 # 39%
perc_toad_repro <- mean(rowSums(bio_ind[,c("crap_oeuf","crap_tet")]) > 0) * 100 # 32%

################################################################################
# Macroinvertebrates and flying odonates
################################################################################

# Invertebrates (excluding odonate imagos)
perc_macroinv <- mean(bio_pres$pres_macroinv == 1) * 100 # In 61% of surveyed reservoirs

# Benthic macroinvertebrates
perc_macroinv_ben <- mean(bio_pres$pres_macroinv_ben == 1) * 100 # 50%

# Pelagic macroinvertebrates
perc_macroinv_pel <- mean(bio_pres$pres_macroinv_pel == 1) * 100 # 50%

# Surface macroinvertebrates (water striders)
perc_macroinv_sur <- mean(bio_pres$pres_macroinv_sur == 1) * 100 # 21%

# Odonates presence
bio_pres$pres_odo <- rowSums(bio_ind[,c("odo_ad","odo_larv","odo_exuv")]) > 0
perc_odonates <- mean(bio_pres$pres_odo) * 100 # 7%

################################################################################
# Introduced fishes
################################################################################

# Presence of fish
perc_fish <- mean(bio_ind$poissons == "oui") * 100 # In 29% of surveyed reservoirs

# Presence of salmonids
perc_salmonids <- mean(na.omit(bio_ind$salmonides == "oui")) * 100 # In 17% of surveyed reservoirs containing fish

################################################################################
# Mortality observations
################################################################################

# Amphibian mortality
perc_newt_mort <- mean(bio_ind$trit_mort == 1) * 100 # in 11% of surveyed reservoirs
perc_toad_mort <- mean(bio_ind$crap_mort == 1) * 100 # 7%
perc_frog_mort <- mean(bio_ind$gren_mort == 1) * 100 # 4%

# Odonate mortality
perc_odo_mort <- mean(bio_ind$odo_mort == 1) * 100 # 4%

################################################################################
# Relationship between vegetation presence and reservoir age
################################################################################

# Calculate reservoir age
bio_exp$age <- 2021 - bio_exp$annee

# Kruskal-Wallis test
veg_age_test <- kruskal.test(bio_exp$age ~ as.factor(bio_ind$veg_terr))

veg_age_test

# Mean age of reservoirs hosting terrestrial vegetation
mean_age_veg <- mean(bio_exp$age[bio_ind$veg_terr == 1], na.rm = TRUE)
sd(bio_exp$age[bio_ind$veg_terr == 1], na.rm = TRUE) # 9,7 +/- 6,3 years after construction

################################################################################
# Effect of altitude on amphibian presence
################################################################################

# frogs
frog_alt_test <- wilcox.test(bio_exp$altitude ~ bio_pres$pres_gren)

# toads
toad_alt_test <- wilcox.test(bio_exp$altitude ~ bio_pres$pres_crap)

# newts
newt_alt_test <- wilcox.test(bio_exp$altitude ~ bio_pres$pres_trit)

# Global amphibian presence
amph_alt_test <- wilcox.test(bio_exp$altitude ~ bio_pres$pres_amph)

################################################################################
# Relationship between altitude and water temperature
################################################################################

# Linear model
model_temp_alt <- lm(temp_moy ~ altitude, data=bio_exp)

summary(model_temp_alt)

# Correlation test
cor.test(bio_exp$altitude, bio_exp$temp_moy)

################################################################################
# Species richness
################################################################################

# Compute biodiversity richness score
bio_ind$richness <- with(bio_ind,
                         gren_fav + trit_fav + crap_fav +
                           odo_fav + macroinv_fav + veg_fav)

# Test relationship with reservoir characteristics
kruskal.test(bio_ind$richness ~ bio_exp$drain) # presence of water draining system
kruskal.test(bio_ind$richness ~ bio_exp$orientation) # watershed orientation
kruskal.test(bio_ind$richness ~ bio_exp$position) # reservoir position

################################################################################
# Mortality drivers
################################################################################

# Total mortality score
bio_ind$mortality <- with(bio_ind,
                          crap_mort + gren_mort + trit_mort + odo_mort)

# Mortality vs confinement
boxplot(bio_ind$mortality ~ bio_exp$conf)

# Mortality vs bank slope
boxplot(bio_ind$mortality ~ bio_exp$fPente)

# Mortality vs fish presence
boxplot(bio_ind$mortality ~ bio_ind$poissons)

################################################################################
# Mortality drivers (excluding reservoirs with zero mortality)
################################################################################

# Subset reservoirs where mortality was observed
mort_data <- data.frame(
  mortality = bio_ind$mortality,
  confinement = bio_exp$conf,
  slope_class = bio_exp$fPente,
  fish = bio_ind$poissons
)

mort_data_nonzero <- mort_data %>%
  filter(mortality > 0)

# Mortality vs confinement
boxplot(mortality ~ confinement,
        data = mort_data_nonzero,
        main = "Mortality vs confinement",
        ylab = "Mortality score")

# Mortality vs bank slope
boxplot(mortality ~ slope_class,
        data = mort_data_nonzero,
        main = "Mortality vs bank slope",
        ylab = "Mortality score")

# Mortality vs fish presence
boxplot(mortality ~ fish,
        data = mort_data_nonzero,
        main = "Mortality vs fish presence",
        ylab = "Mortality score")
