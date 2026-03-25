################################################################################
# Snowmaking reservoir revegetation with macrophytes
# Hydrophyte survival and growth analysis
# Author: Benjamin Gerfand
# Version : September 2025
################################################################################

#### Packages ####
library(dplyr)
library(tidyr)

#########################
#### Import datasets ####
#########################

data_path <- "data_path"  # adapt to your project structure (file = "Data for R")
data_path <- "D:/Thèse Gerfand/3 - Végétalisation Adret des Tuffes/Article végétalisation berges/Soumission/Ecological engineering/Supplementary material/Data/Data for R"  # adapt to your project structure

suivi1 <- read.csv2(paste(data_path,"bg_hydrophytes_suivi 1_juillet 2023.csv",sep="/"),sep=';',dec=",")
suivi2 <- read.csv2(paste(data_path,"bg_hydrophytes_suivi 2_septembre 2023.csv",sep="/"),sep=';',dec=",")
hydro2025 <- read.csv2(paste(data_path,"bg_hydrophytes_survie_2025.csv",sep="/"),sep=';',dec=",")

############################################
#### Prepare monitoring dataset (2023) ####
############################################

suivi1$date <- "2023-07-12"
suivi2$date <- "2023-09-20"

suivi23<- bind_rows(suivi1, suivi2)

##########################################################
#### Hydrophyte growth during the first season (2023) ####
##########################################################

# Mean tussock area per species at the end of the season (September 2023)
growth_stats <- suivi2 %>%
  group_by(espece) %>%
  summarise(
    mean_area = mean(recouvrement.touffe..cm2., na.rm = TRUE),
    sd_area = sd(recouvrement.touffe..cm2., na.rm = TRUE),
    .groups = "drop"
  )

growth_stats

# C. globularis ≈ 63.5 ± 42.0 cm²
# C. vulgaris ≈ 50.0 ± 22.8 cm²

##################################################################
#### Statistical test: growth between July and September 2023 ####
##################################################################

# Prepare paired dataset
hydro_wide <- suivi23%>%
  select(espece, ID, date, recouvrement.touffe..cm2.) %>%
  pivot_wider(names_from = date,
              values_from = recouvrement.touffe..cm2.) %>%
  mutate(diff = `2023-09-20` - `2023-07-12`)

# Paired Wilcoxon test for each species
growth_test <- hydro_wide %>%
  group_by(espece) %>%
  summarise(
    V = wilcox.test(`2023-07-12`, `2023-09-20`, paired = TRUE)$statistic,
    p_value = wilcox.test(`2023-07-12`, `2023-09-20`, paired = TRUE)$p.value
  )

growth_test


#### No survival in June 2024 --> new plantation trials in July 2024 using Chara vulgaris in 9 plots ####
#### No survival in June and August 2025 ####
#### Regrowth in September 2025 ####


###############################################
#### Hydrophyte survival in September 2025 ####
###############################################

# Number of plots with photosynthetic hydrophyte fragments found in September 2025
plots_with_plants <- hydro2025 %>%
  filter(pres.abs == 1) %>%
  nrow()

total_plots <- nrow(hydro2025) # number of hydrophyte plots in the whole experiment

plot_survival_rate <- plots_with_plants / total_plots * 100

plots_with_plants # number of plots with surviving fragments out of 16 (number of plots in experiment)
plot_survival_rate # Result: 14 / 16 plots = 87.5%

# Meaning : there is survival in both replanted (n = 9) and non-replanted (n = 5) plots 
nrow(hydro2025[hydro2025$replantée.en.2024 == "oui" & hydro2025$pres.abs == 1,])
nrow(hydro2025[hydro2025$replantée.en.2024 == "non" & hydro2025$pres.abs == 1,])

###############################################################
#### Survival in September 2025 of replanted plots in 2024 ####
###############################################################

replanted <- hydro2025 %>%
  filter(replantée.en.2024 == "oui")

survivors_replanted <- sum(replanted$nombre.de.survie.sur.18)

survival_rate_replanted <- survivors_replanted / (18 * nrow(replanted)) * 100 # there are 18 planted cavities per plot

survival_rate_replanted_global <- (survivors_replanted / 288) * 100 # there are 288 planting cavities in total

survivors_replanted # total number of cavities with surviving fragments in replanted plots
survival_rate_replanted # % survival among replanted plots
#survival_rate_replanted_global # % survival among all replanted and non-replanted plots

###################################################################
#### Survival in September 2025 of non-replanted plots in 2024 ####
###################################################################

original <- hydro2025 %>%
  filter(replantée.en.2024 == "non")

survivors_original <- sum(na.omit(original$nombre.de.survie.sur.18))

survival_rate_original <- survivors_original / (18 * nrow(original)) * 100

survival_rate_original_global <- (survivors_original / 288) * 100 # there are 288 planting cavities in total

survivors_original # total number of cavities with surviving fragments in non-replanted plots
survival_rate_original # % survival among non-replanted plots
#survival_rate_original_global # % survival among all replanted and non-replanted plots

####################################################################################################
#### Mean survival per plot in Sept. 2025 over the 288 planting cavities regardless the species ####
####################################################################################################

hydro2025 <- hydro2025 %>%
  mutate(
    survival_percent = (nombre.de.survie.sur.18 / 18) * 100
  )

summary_stats <- hydro2025 %>%
  filter(pres.abs == 1) %>%
  summarise(
    mean_survival = mean(survival_percent),
    sd_survival = sd(survival_percent)
  )

summary_stats # mean ≈ 46% ± 22% surviving cavities per plot
sum(na.omit(hydro2025$nombre.de.survie.sur.18)) # 117 alive vs 171 dead over the 288 planting cavities
(sum(na.omit(hydro2025$nombre.de.survie.sur.18)) / 288) * 100 # % survival of hydrophytes regardless the species

##############################################################
#### Comparison between replanted and non-replanted plots ####
##############################################################

wilcox_survival <- wilcox.test(
  nombre.de.survie.sur.18 ~ replantée.en.2024,
  data = hydro2025 %>% filter(pres.abs == 1)
)

wilcox_survival # W = 15 ; p = 0.35 (not significant)
