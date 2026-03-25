################################################################################
# Snowmaking reservoir revegetation with macrophytes
# Evolution of annual proportion of flowering tussocks
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

data_path <- "data_path"  # adapt to your project structure (fil = "Data for R")
data_path <- "D:/Thèse Gerfand/3 - Végétalisation Adret des Tuffes/Article végétalisation berges/Soumission/Ecological engineering/Supplementary material/Data/Data for R"  # adapt to your project structure

suivi <- read.csv2(paste(data_path,"bg_helophytes_spread_dataset.csv",sep="/"),sep=',',dec=".")

#########################
#### Prepare dataset ####
#########################

# Binary flowering column: 1 = yes, 0 = no
suivi <- suivi %>%
  mutate(
    inflo_bool = ifelse(inflorescence == "oui", 1, 0),
    annee = case_when(
      numero_suivi == 1 ~ "2023",
      numero_suivi %in% c(2,3) ~ "2024",
      numero_suivi %in% c(4,5) ~ "2025"
    )
  )

###############################
#### Annual flowering data ####
###############################

# Determine if each tussock flowered at least once during the year
inflo_by_year <- suivi %>%
  group_by(ID, espece, annee) %>%
  summarise(inflo_annee = ifelse(any(inflo_bool == 1, na.rm=TRUE), 1, 0), .groups="drop")

# Summary by species x year
inflo_summary <- inflo_by_year %>%
  group_by(espece, annee) %>%
  summarise(
    proportion = mean(inflo_annee, na.rm=TRUE)*100,
    n = n(),
    .groups="drop"
  )

inflo_summary

# Carex nigra increase
CN24 = inflo_summary[inflo_summary$espece == "Carex nigra" & inflo_summary$annee == "2024",]$proportion
CN25 = inflo_summary[inflo_summary$espece == "Carex nigra" & inflo_summary$annee == "2025",]$proportion
CN25 / CN24 # x3.09

# Eriophorum angustifolium average production
EA24 = inflo_summary[inflo_summary$espece == "Eriophorum angustifolium" & inflo_summary$annee == "2024",]$proportion
EA25 = inflo_summary[inflo_summary$espece == "Eriophorum angustifolium" & inflo_summary$annee == "2025",]$proportion
(EA24 + EA25) / 2 # 64.4 %
sd(c(EA24,EA25)) # +/- 2.2 %

##########################
#### Statistical tests ####
##########################

# Remove 2023 (baseline year with no flowering by design)
inflo_by_year_test <- inflo_by_year %>%
  filter(annee != "2023")

# Global test: Kruskal-Wallis per species (excluding 2023)
kruskal_results <- inflo_by_year_test %>%
  group_by(espece) %>%
  kruskal_test(inflo_annee ~ annee) %>%
  ungroup() %>%
  mutate(signif = case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ "ns"
  ))

kruskal_results

# Post-hoc: Dunn test per species
dunn_results <- inflo_by_year_test %>%
  group_by(espece) %>%
  dunn_test(inflo_annee ~ annee, p.adjust.method="bonferroni") %>%
  ungroup()

dunn_results

#################################
#### Barplot with statistics ####
#################################

# Prepare post-hoc results for plotting
# Significance labels
posthoc_plot <- dunn_results %>%
  mutate(
    p.adj.signif = case_when(
      p.adj < 0.001 ~ "***",
      p.adj < 0.01  ~ "**",
      p.adj < 0.05  ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  rename(group1 = group1, group2 = group2)  # ggpubr requires these column names

# y-position: above the max proportion observed
posthoc_plot <- posthoc_plot %>%
  group_by(espece) %>%
  mutate(y.position = max(inflo_summary$proportion, na.rm = TRUE) + row_number()*5) %>%
  ungroup()

# Colors
cols <- c("Carex nigra" = "#117733", "Eriophorum angustifolium" = "#999933")

# Barplot
p <- ggplot(inflo_summary, aes(x = annee, y = proportion, fill = espece)) +
  geom_col(position = position_dodge(0.8), width = 0.7, color = "black") +
  facet_wrap(~espece) +
  scale_fill_manual(values = cols) +
  labs(
    x = "Year",
    y = "Proportion of tussocks with inflorescence (%)"
  ) +
  theme_minimal(base_size = 22) +
  theme(legend.position = "none")

# Add post-hoc comparisons
if(nrow(posthoc_plot) > 0){
  p <- p + stat_pvalue_manual(
    posthoc_plot,
    label = "p.adj.signif",
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    hide.ns = TRUE,
    size = 7
  )
}

# Display plot
print(p)
