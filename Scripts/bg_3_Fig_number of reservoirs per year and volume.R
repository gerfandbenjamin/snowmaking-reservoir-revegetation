################################################################################
# Ecological quality of snowmaking reservoirs (French Alps)
# Cumulative number of reservoirs built per volume class (1989–2021)
# Author: Benjamin Gerfand
# Version : August 2023
################################################################################

#### Packages ####
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(forcats)

#### Data import ####
data_path <- "data_path"  # adapt to your project structure

retalt <- read.csv2(paste(data_path,"retaltbiodiv.csv",sep="/"),sep=';',dec=",")

#### Data preparation ####
RA <- retalt %>%
  filter(type_ouvrage == "retenue_altitude") %>%
  select(id_retalt, volume_m3, annee_construction) %>%
  drop_na()

#### Create volume classes ####

breaks <- c(0, 15000, 30000, 60000, 100000, 405000)

labels <- c(
  "A [1000 - 15 000]",
  "B [15 000 - 30 000]",
  "C [30 000 - 60 000]",
  "D [60 000 - 100 000]",
  "E [100 000 - 405 000]"
)

RA <- RA %>%
  mutate(
    vol_class = cut(volume_m3,
                    breaks = breaks,
                    labels = labels,
                    include.lowest = TRUE)
  )

#### Count number of reservoirs built per year and class ####

year_range <- 1989:2021

RA_yearly <- RA %>%
  count(annee_construction, vol_class) %>%
  complete(
    annee_construction = year_range,
    vol_class,
    fill = list(n = 0)
  ) %>%
  arrange(vol_class, annee_construction)

#### Compute cumulative number per class ####

RA_cumu <- RA_yearly %>%
  group_by(vol_class) %>%
  mutate(cum_n = cumsum(n)) %>%
  ungroup()

# Ensure ordered factor (smallest to largest volumes)
RA_cumu$vol_class <- factor(RA_cumu$vol_class, levels = labels)

#### Plot ####

cbp1 <- rep("#000000", 5)  # black for all classes
shape_names <- c(4, 18, 16, 17, 15)  
# cross, diamond, circle, triangle, square (ggplot shape codes)

Fig1 <- ggplot(RA_cumu, 
               aes(x = annee_construction,
                   y = cum_n,
                   colour = vol_class,
                   shape = vol_class)) +
  geom_point(size = 3) +
  geom_path() +
  geom_vline(xintercept = 2006,
             colour = "black",
             linetype = "longdash") +
  geom_text(aes(x = 2006,
                y = min(cum_n),
                label = "2006"),
            vjust = -0.05,
            hjust = -0.5,
            size = 8,
            color = "black") +
  scale_color_manual(values = cbp1) +
  scale_shape_manual(values = shape_names) +
  labs(x = "Years",
       y = "Number of reservoirs",
       colour = "Volume (m3)",
       shape = "Volume (m3)") +
  theme_bw() +
  theme(
    legend.position = c(0.18, 0.8),
    text = element_text(size = 18)
  )

Fig1
