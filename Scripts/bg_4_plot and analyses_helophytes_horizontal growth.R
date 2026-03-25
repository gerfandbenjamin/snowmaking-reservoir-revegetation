################################################################################
# Snowmaking reservoir revegetation with macrophytes
# Evolution of helophytes tussock surface area
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

data_path <- "data_path"  # adapt to your project structure (file = "Data for R")
data_path <- "D:/Thèse Gerfand/3 - Végétalisation Adret des Tuffes/Article végétalisation berges/Soumission/Ecological engineering/Supplementary material/Data/Data for R"  # adapt to your project structure

suivi <- read.csv2(paste(data_path,"bg_helophytes_growth_dataset.csv",sep="/"),sep=',',dec=".")

#############################
#### Surface calculation ####
#############################

# Ellipse area formula: area = pi * (0.5*length) * (0.5*width)
suivi <- suivi %>%
  mutate(recouvrement = pi * (0.5*longueur.touffe) * (0.5*largeur.touffe))

##############################################
#### Filter for September (end-of-season) ####
##############################################

suivi$date <- as.Date(suivi$date, format = "%Y-%m-%d")

suivi_sep <- suivi %>%
  filter(format(date, "%m") == "09") %>%
  mutate(annee = format(date, "%Y")) %>%
  filter(!is.na(recouvrement), espece %in% c("Carex nigra", "Eriophorum angustifolium"))

############################
#### Summary statistics ####
############################

summary_data <- suivi_sep %>%
  group_by(espece, annee) %>%
  summarise(
    mean_r = mean(recouvrement, na.rm = TRUE),
    sd_r   = sd(recouvrement, na.rm = TRUE),
    n      = sum(!is.na(recouvrement)),
    se_r   = sd_r / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(annee = factor(annee, levels = c("2023","2024","2025")))

summary_data

mean(suivi_sep[suivi_sep$espece == "Eriophorum angustifolium",]$recouvrement) # 5.8 cm²
sd(suivi_sep[suivi_sep$espece == "Eriophorum angustifolium",]$recouvrement) # +/- 3.5 cm²


################################################
#### Global statistical tests (per species) ####
################################################

# Function to choose ANOVA or Kruskal-Wallis based on normality and homoscedasticity
test_global <- function(df_sp){
  mod <- lm(recouvrement ~ annee, data = df_sp)
  
  shapiro <- shapiro.test(residuals(mod))
  levene  <- car::leveneTest(recouvrement ~ annee, data = df_sp)
  
  if(shapiro$p.value > 0.05 & levene$`Pr(>F)`[1] > 0.05){
    res <- anova(mod)
    data.frame(method = "ANOVA",
               stat   = round(res$`F value`[1],3),
               df1    = res$Df[1],
               df2    = res$Df[2],
               p.value = round(res$`Pr(>F)`[1],4))
  } else {
    res <- kruskal.test(recouvrement ~ annee, data = df_sp)
    data.frame(method = "Kruskal-Wallis",
               stat   = round(res$statistic,3),
               df1    = res$parameter,
               df2    = NA,
               p.value = round(res$p.value,4))
  }
}

global_results <- suivi_sep %>%
  group_split(espece) %>%
  map_df(~{
    out <- test_global(.x)
    out$espece <- unique(.x$espece)
    out
  }) %>%
  mutate(signif = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE ~ "ns"
  ))

print(global_results)

##########################################
#### Post-hoc comparisons (Dunn test) ####
##########################################

posthoc_tests <- suivi_sep %>%
  group_by(espece) %>%
  dunn_test(recouvrement ~ annee, p.adjust.method = "bonferroni") %>%
  mutate(
    p.signif = case_when(
      p.adj < 0.001 ~ "***",
      p.adj < 0.01  ~ "**",
      p.adj < 0.05  ~ "*",
      TRUE ~ "ns"
    )
  )

posthoc_tests

# Size effect for Carex nigra 2023 - 2025
C_nigra25 = summary_data[summary_data$espece == "Carex nigra" & summary_data$annee == "2025",]$mean_r
C_nigra23 = summary_data[summary_data$espece == "Carex nigra" & summary_data$annee == "2023",]$mean_r
C_nigra25 / C_nigra23 # increase x1.4

##################################
#### Inter-species comparison ####
##################################

# Filter data: only September 2023
sep23 <- suivi_sep %>% filter(annee == "2023")

# Kruskal-Wallis test between species
kw_species23 <- kruskal.test(recouvrement ~ espece, data = sep23)
kw_species23

# Filter data: only September 2024
sep24 <- suivi_sep %>% filter(annee == "2024")

# Kruskal-Wallis test between species
kw_species24 <- kruskal.test(recouvrement ~ espece, data = sep24)
kw_species24

# Filter data: only September 2025
sep25 <- suivi_sep %>% filter(annee == "2025")

# Kruskal-Wallis test between species
kw_species25 <- kruskal.test(recouvrement ~ espece, data = sep25)
kw_species25

#################################
#### Barplot with statistics ####
#################################

# Compute y positions
ymax_df <- summary_data %>%
  group_by(espece) %>%
  summarise(ymax = max(mean_r + sd_r, na.rm = TRUE), .groups = "drop")

# Prepare post-hoc results (ONLY significant ones)
comparisons_plot <- posthoc_tests %>%
  filter(p.adj < 0.05) %>%   # keep only significant comparisons
  left_join(ymax_df, by = "espece") %>%
  group_by(espece) %>%
  arrange(group1, group2) %>%
  mutate(
    idx = row_number(),
    step = ymax * 0.06,
    y.position = ymax + idx * step,
    
    # keep only significance stars
    p.signif = case_when(
      p.adj < 0.001 ~ "***",
      p.adj < 0.01  ~ "**",
      p.adj < 0.05  ~ "*"
    )
  ) %>%
  ungroup() %>%
  select(espece, group1, group2, p.signif, y.position)

cols <- c("Carex nigra"="#117733",
          "Eriophorum angustifolium"="#999933")

# Plot
p <- ggplot(summary_data, aes(x = annee, y = mean_r, fill = espece)) +
  geom_col(position = position_dodge(0.8),
           width = 0.7,
           color = "black") +
  
  geom_errorbar(aes(ymin = mean_r - sd_r,
                    ymax = mean_r + sd_r),
                position = position_dodge(0.8),
                width = 0.2) +
  
  facet_wrap(~espece, scales = "fixed", ncol = 2) +
  
  scale_fill_manual(values = cols) +
  
  labs(x = "", y = "Tussock surface area (cm²)") +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text  = element_text(size = 20)
  )

# Add only significant comparisons
if(nrow(comparisons_plot) > 0){
  p <- p + stat_pvalue_manual(
    comparisons_plot,
    label = "p.signif",     # ONLY stars
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    inherit.aes = FALSE,
    size = 7
  )
}

print(p)

