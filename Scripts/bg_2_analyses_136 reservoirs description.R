################################################################################
# Ecological quality of snowmaking reservoirs (French Alps)
# Detailed descriptive analyses
# Author: Benjamin Gerfand
# Version : August 2023
################################################################################

#### Packages ####
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(car)        # for Levene test
library(FSA)        # for Dunn / Kruskal multiple comparisons

#### Data import ####
data_path <- "data_path"  # adapt to your project structure

retalt <- read.csv2(paste(data_path,"retaltbiodiv.csv",sep="/"),sep=';',dec=",")
TEtot <- read.csv2(paste(data_path,"compil_TE$tot.csv",sep="/"),sep=';',dec=",")

#### Data preparation ####
RA <- retalt %>%
  filter(type_ouvrage == "retenue_altitude") %>%
  mutate(altitude_sig_m = as.numeric(altitude_sig_m))

TEtot <- TEtot %>%
  filter(nom != "R2")

################################################################################
# General morphometric characteristics
################################################################################

nrow(RA) # number of snowmaking reservoirs in the studied area in 2021 = 136

morpho_summary <- RA %>%
  summarise(
    mean_volume  = mean(volume_m3, na.rm = TRUE),
    sd_volume    = sd(volume_m3, na.rm = TRUE),
    mean_area    = mean(surface_m2, na.rm = TRUE),
    sd_area      = sd(surface_m2, na.rm = TRUE)
  )

print(morpho_summary) # mean volume and surface area of snowmaking reservoirs

# Total storage capacity of all reservoirs in 2021
total_capacity <- sum(RA$volume_m3, na.rm = TRUE)
total_capacity

################################################################################
# Number of reservoirs above 1500 m elevation
################################################################################

n_above_1500 <- RA %>%
  filter(altitude_sig_m > 1500, !is.na(id_retalt)) %>%
  nrow()

cat("Number of reservoirs above 1500 m:", n_above_1500, "\n")

################################################################################
# Relationship between area and volume
################################################################################

ggplot(RA, aes(x = surface_m2, y = volume_m3)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

extremes <- RA %>%
  summarise(
    largest_volume  = nom_ouv[which.max(volume_m3)],
    smallest_volume = nom_ouv[which.min(volume_m3)],
    largest_area    = nom_ouv[which.max(surface_m2)],
    smallest_area   = nom_ouv[which.min(surface_m2)]
  )

print(extremes)

################################################################################
# Proportion of reservoirs among all mountain water bodies in the studied area
################################################################################

other_water_bodies <- 2593
total_water_bodies <- 2729

prop_RA <- nrow(RA) / total_water_bodies * 100
ratio_RA <- total_water_bodies / nrow(RA)

cat("Proportion of reservoirs:", round(prop_RA, 1), "%\n")
cat("1 reservoir per", round(ratio_RA, 1), "water bodies\n")

################################################################################
# Temporal dynamics of construction
################################################################################

length(na.omit(RA$annee_construction)) # year of construction available for n = 112 reservoirs
summary(na.omit(RA$annee_construction)) # constructed between 1989 and 2021 (median = 2006)

construction_per_year <- RA %>%
  count(annee_construction)

construction_stats <- na.omit(construction_per_year) %>%
  summarise(
    mean_per_year = mean(n),
    sd_per_year   = sd(n),
    min_per_year  = min(n),
    max_per_year  = max(n)
  )

print(construction_stats) # construction rate per year

# Identify year with maximum number of constructions
na.omit(construction_per_year) %>%
  filter(n == max(n))

# 2016 - 2021 tendancy
construction_per_year %>%
  filter(annee_construction >= 2016)

# Create two construction periods
reservoirs_year <- RA %>%
  mutate(period = ifelse(annee_construction < 2006, "pre2006", "post2006"))

# Mean storage capacity per period
reservoirs_year %>%
  group_by(period) %>%
  summarise(mean_capacity = mean(volume_m3, na.rm = TRUE),
            n = n())

# Difference before/after 2006
pre <- reservoirs_year %>%
  filter(period == "pre2006") %>%
  summarise(mean = mean(volume_m3, na.rm = TRUE)) %>%
  pull(mean)

post <- reservoirs_year %>%
  filter(period == "post2006") %>%
  summarise(mean = mean(volume_m3, na.rm = TRUE)) %>%
  pull(mean)

# % storage capacity increanse after 2006
increase_percent <- ((post - pre) / pre) * 100
increase_percent

################################################################################
# Links between year of construction and reservoir size & location
################################################################################

# Link between storage capacity and construction over time
cor_vol_year <- cor.test(RA$annee_construction,
                            RA$volume_m3,
                            method = "spearman")

cor_vol_year

# Link between altitude and construction over time
altitude_model <- lm(as.numeric(RA$altitude_sig_m) ~ annee_construction, data = RA)

summary(altitude_model)
coef(summary(altitude_model)) # t and p-value
cor(as.numeric(RA$altitude_sig_m), RA$annee_construction, use = "complete.obs") # correlation coef

par(mfrow = c(1,2))
plot(RA$annee_construction, RA$volume_m3)
plot(RA$annee_construction, as.numeric(RA$altitude_sig_m))
par(mfrow = c(1,1))

################################################################################
# Confinement types
################################################################################

conf_distribution_total <- table(RA$confinement)
print(conf_distribution_total) # all studied area

conf_distribution_visited <- TEtot %>%
  distinct(nom, conf) %>%
  count(conf)

print(conf_distribution_visited) # among visited reservoirs

################################################################################
# Slope analysis by measurement zone
################################################################################

slope_summary <- TEtot %>%
  filter(!is.na(pente)) %>%
  group_by(nom, loc) %>%
  summarise(mean_slope = mean(pente), .groups = "drop")

# Mean slope by zone
zone_means <- slope_summary %>%
  group_by(loc) %>%
  summarise(mean_slope = mean(mean_slope))

print(zone_means)

################################################################################
# Threshold comparison (18° recommendation)
################################################################################

mean_slope_RA <- slope_summary %>%
  filter(loc != "evac") %>%
  group_by(nom) %>%
  summarise(mean_slope = mean(mean_slope))

below_threshold <- sum(mean_slope_RA$mean_slope < 18, na.rm = TRUE)
above_threshold <- sum(mean_slope_RA$mean_slope > 18, na.rm = TRUE)

cat("Reservoirs below 18°:", below_threshold, "\n")
cat("Reservoirs above 18°:", above_threshold, "\n")

################################################################################
# Effect of confinement on slope
################################################################################

bxp <- as.data.frame(unique(slope_summary %>%
  pivot_wider(names_from = loc, values_from = mean_slope) %>%
  left_join(TEtot %>% select(nom, conf, annee),
            by = c("nom" = "nom"))))

ggplot(bxp, aes(x = conf, y = milieu)) +
  geom_boxplot() +
  theme_minimal()

kruskal_result <- kruskal.test(milieu ~ conf, data = bxp)
print(kruskal_result)

################################################################################
# Relationship between slope and construction year
################################################################################

model <- lm(milieu ~ annee, data = bxp)
summary(model)

ggplot(bxp, aes(x = annee, y = milieu)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

