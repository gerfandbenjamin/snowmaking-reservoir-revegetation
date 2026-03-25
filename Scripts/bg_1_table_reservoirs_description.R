################################################################################
# Ecological quality of Snowmaking reservoirs (French Alps)
# Descriptive table by county
# Author: Benjamin Gerfand
# Version : August 2023
################################################################################


#### Packages ####
library(dplyr)
library(readr)
library(tidyr)
library(flextable)


#### Data import ####
data_path <- "data_path"  # adapt to your project structure

retalt <- read.csv2(paste(data_path,"retaltbiodiv.csv",sep="/"),sep=';',dec=",")
TEtot <- read.csv2(paste(data_path,"compil_TE$tot.csv",sep="/"),sep=';',dec=",")


#### Data filtering ####
RA <- retalt %>%
  filter(type_ouvrage == "retenue_altitude")

TEtot <- TEtot %>%
  filter(nom != "R2") # removing an unconform site


#### Summary statistics by department ####
summary_stats <- RA %>%
  mutate(altitude_sig_m = as.numeric(altitude_sig_m)) %>%
  group_by(departement) %>%
  summarise(
    n_reservoirs = n(),
    visited_reservoirs = NA,
    total_volume = sum(volume_m3, na.rm = TRUE),
    min_volume   = min(volume_m3, na.rm = TRUE),
    mean_volume  = mean(volume_m3, na.rm = TRUE),
    max_volume   = max(volume_m3, na.rm = TRUE),
    total_area   = sum(surface_m2, na.rm = TRUE),
    min_area     = min(surface_m2, na.rm = TRUE),
    mean_area    = mean(surface_m2, na.rm = TRUE),
    max_area     = max(surface_m2, na.rm = TRUE),
    min_altitude = min(as.numeric(altitude_sig_m), na.rm = TRUE),
    mean_altitude= mean(as.numeric(altitude_sig_m), na.rm = TRUE),
    max_altitude = max(as.numeric(altitude_sig_m), na.rm = TRUE),
    first_year   = min(annee_construction, na.rm = TRUE),
    median_year  = median(annee_construction, na.rm = TRUE),
    .groups = "drop"
  ) 


#### Visited reservoirs per department ####
summary_stats$departement <- c("Hautes-Alpes (05)", "Isère (38)", "Savoie (73)", "Haute-Savoie (74)")
summary_stats$visited_reservoirs <- c(5, 6, 15, 2)


#### Pivot to table format ####
recap <- summary_stats %>%
  select(departement, everything()) %>%
  pivot_longer(-departement, names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = departement, values_from = value)

#### Add overall mean, sd and total columns ####
recap <- recap %>%
  mutate(
    mean  = apply(select(., -variable), 1, mean, na.rm = TRUE),
    sd    = apply(select(., -variable), 1, sd, na.rm = TRUE),
    total = apply(select(., -variable), 1, sum, na.rm = TRUE)
  )


#### Formatting ####
recap <- recap %>%
  mutate(across(-variable, ~ round(.x, 0)))


#### Creating html table ####
border_head <- fp_border_default(color = "grey50", width = 1.5) # style de lignes pour titres
border_body <- fp_border_default(color = "grey", width = 1.5) # style de lignes corps du tableau
ft <- flextable(recap, cwidth = c(3,1.5,1,1,1.5,0.75,0.75,0.75)) # table jpeg
ft = align(ft ,j = 2:8, align = "right", part = "body") # centrer valeurs des colonnes
ft = align(ft, align = "center", part = "header") # centrer les titres colonnes

ft = ft %>% 
  vline(part = "all", j = c(1,5), border = border_head) %>% # vertical lines
  vline(part = "all", j = c(7), border = border_body) %>% # vertical lines
  hline(part = "body", i = c(2,6,10,13), border = border_body) # horizontal lines 

ft


