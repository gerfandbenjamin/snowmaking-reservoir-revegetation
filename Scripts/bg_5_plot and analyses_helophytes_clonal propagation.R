################################################################################
# Snowmaking reservoir revegetation with macrophytes
# Evolution of helophytes number of new vegetative shoots
# Author: Benjamin Gerfand
# Version : September 2025
################################################################################

#### Packages ####
library(tidyverse)
library(ggpubr)
library(rstatix)
library(car)

#########################
#### Import datasets ####
#########################

data_path <- "data_path"  # adapt to your project structure (file = "Data_for_R")

suivi <- read.csv2(paste(data_path,"bg_helophytes_spread_dataset.csv",sep="/"),sep=',',dec=".")

#########################
#### Prepare dataset ####
#########################

# Fix the number of new shoots to 0 for the first monitornig = plantation 
suivi[suivi$numero_suivi == 1,]$nombre.reprises <- 0

###############################################
#### Select September monitoring campaigns ####
###############################################

# Only September surveys are used for the analysis
# (2023, 2024 and 2025) to ensure seasonal comparability
suivi_sept <- suivi %>%
  filter(numero_suivi %in% c(1,3,5)) %>%
  mutate(annee = case_when(
    numero_suivi == 1 ~ "2023",
    numero_suivi == 3 ~ "2024",
    numero_suivi == 5 ~ "2025"
  ))

################################
#### Descriptive statistics ####
################################

offshoot_stats <- suivi_sept %>%
  group_by(espece, annee) %>%
  summarise(
    mean_offshoots = mean(nombre.reprises, na.rm = TRUE),
    sd_offshoots   = sd(nombre.reprises, na.rm = TRUE),
    n              = n(),
    .groups = "drop"
  )

offshoot_stats

###############################################################
#### Intra-species temporal evolution
###############################################################

# We test whether the number of offshoots increased over time
# for each species independently
test_intra <- function(species_name){
  
  df <- suivi_sept %>%
    filter(espece == species_name)
  
  kruskal_test(nombre.reprises ~ annee, data = df)
  
}

kruskal_CN <- test_intra("Carex nigra")
kruskal_EA <- test_intra("Eriophorum angustifolium")

kruskal_CN
kruskal_EA

# Post-hoc pairwise comparisons between years
dunn_CN <- suivi_sept %>%
  filter(espece == "Carex nigra") %>%
  dunn_test(nombre.reprises ~ annee, p.adjust.method = "bonferroni")

dunn_EA <- suivi_sept %>%
  filter(espece == "Eriophorum angustifolium") %>%
  dunn_test(nombre.reprises ~ annee, p.adjust.method = "bonferroni")

dunn_CN
dunn_EA

###################################################
#### Inter-species comparison (September 2025) ####
###################################################

# Filter dataset for sept. 2025
df_2025 <- suivi_sept %>%
  filter(annee == "2025")

# test
kruskal_inter <- kruskal_test(nombre.reprises ~ espece, data = df_2025)

kruskal_inter

# Mean ± sd values for each species
final_stats <- df_2025 %>%
  group_by(espece) %>%
  summarise(
    mean = mean(nombre.reprises, na.rm = TRUE),
    sd   = sd(nombre.reprises, na.rm = TRUE),
    n    = n(),
    .groups = "drop"
  )

final_stats

#################################
#### Barplot with statistics ####
#################################

##Data summary for plotting
plot_data <- suivi_sept %>%
  group_by(annee, espece) %>%
  summarise(
    mean = mean(nombre.reprises, na.rm = TRUE),
    sd   = sd(nombre.reprises, na.rm = TRUE),
    n    = n(),
    se   = sd / sqrt(n),
    .groups = "drop"
  )

## Prepare post-hoc results for plotting
# Combine post-hoc results from both species
posthoc_plot <- bind_rows(
  dunn_CN %>% mutate(espece = "Carex nigra"),
  dunn_EA %>% mutate(espece = "Eriophorum angustifolium")
)

# Significance labels
posthoc_plot <- posthoc_plot %>%
  mutate(p.adj.signif = case_when(
    p.adj < 0.001 ~ "***",
    p.adj < 0.01  ~ "**",
    p.adj < 0.05  ~ "*",
    TRUE ~ "ns"
  ))

# Y position for significance bars
posthoc_plot <- posthoc_plot %>%
  mutate(
    y.position = max(plot_data$mean + plot_data$sd) + row_number()*0.5
  )

## Barplot
cols <- c(  "Carex nigra" = "#117733",  "Eriophorum angustifolium" = "#999933")

p <- ggplot(plot_data, aes(x = annee, y = mean, fill = espece)) +
  
  geom_col(position = position_dodge(0.9),
           width = 0.7,
           color = "black") +
  
  geom_errorbar(aes(ymin = mean - sd,
                    ymax = mean + sd),
                position = position_dodge(0.9),
                width = 0.2) +
  
  facet_wrap(~espece) +
  
  scale_fill_manual(values = cols) +
  
  labs(
    x = "Year",
    y = "Mean number of vegetative offshoots"
  ) +
  
  theme_minimal(base_size = 22) +
  theme(
    legend.position = "none"
  )

# Add statistical comparisons
p + stat_pvalue_manual(
  posthoc_plot,
  label = "p.adj.signif",
  xmin = "group1",
  xmax = "group2",
  y.position = "y.position",
  tip.length = 0.01,
  size = 7
)


