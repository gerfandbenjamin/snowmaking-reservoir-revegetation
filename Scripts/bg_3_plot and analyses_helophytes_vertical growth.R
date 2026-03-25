################################################################################
# Snowmaking reservoir revegetation with macrophytes
# Evolution of maximum leaf height of helophyte tussocks
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

suivi <- read.csv2(paste(data_path,"bg_helophytes_growth_dataset.csv",sep="/"),sep=',',dec=".")

#################################################
#### Average Hmax per species and monitoring ####
#################################################

summary_hmax <- suivi %>%
  group_by(espece, numero_suivi) %>%
  summarise(
    mean_hmax = mean(hauteur.max.feuille, na.rm=TRUE),
    sd_hmax   = sd(hauteur.max.feuille, na.rm=TRUE),
    n         = n(),
    se_hmax   = sd_hmax / sqrt(n),
    .groups = "drop"
  )

summary_hmax

##########################################################
#### Hmax evolution at each end of season (September) ####
##########################################################

# dataset for september
suivi$date <- as.Date(suivi$date, format = "%Y-%m-%d")
suivi_sep <- suivi %>%
  filter(format(date, "%m")=="09") %>%
  mutate(annee = format(date, "%Y"))

summary_data <- suivi_sep %>%
  group_by(espece, annee) %>%
  summarise(
    mean_h = mean(hauteur.max.feuille, na.rm = TRUE),
    sd_h   = sd(hauteur.max.feuille, na.rm = TRUE),
    n      = n(),
    .groups = "drop"
  )

summary_data

# rates per year
decline_rates <- summary_data %>%
  filter(annee %in% c("2023","2025")) %>%
  select(espece, annee, mean_h) %>%
  pivot_wider(names_from = annee, values_from = mean_h) %>%
  mutate(
    years = 2,
    annual_decline = ((`2025` / `2023`)^(1/years) - 1) * 100
  )

decline_rates

# Global statistical test per species : ANOVA or Kruskal-Wallis depending on application conditions
test_global <- function(df_sp){
  
  mod <- lm(hauteur.max.feuille ~ annee, data = df_sp)
  
  shapiro <- shapiro.test(residuals(mod))
  levene  <- car::leveneTest(hauteur.max.feuille ~ annee, data = df_sp)
  
  if(shapiro$p.value > 0.05 & levene$`Pr(>F)`[1] > 0.05){
    
    res <- anova(mod)
    
    data.frame(
      method = "ANOVA",
      stat   = round(res$`F value`[1],2),
      df1    = res$Df[1],
      df2    = res$Df[2],
      p.value = format.pval(res$`Pr(>F)`[1], digits = 3)
    )
    
  } else {
    
    res <- kruskal.test(hauteur.max.feuille ~ annee, data = df_sp)
    
    data.frame(
      method = "Kruskal-Wallis",
      stat   = round(res$statistic,1),
      df1    = res$parameter,
      df2    = NA,
      p.value = format.pval(res$p.value, digits = 3)
    )
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
    p.value<0.001 ~ "***",
    p.value<0.01  ~ "**",
    p.value<0.05  ~ "*",
    TRUE ~ "ns"
  ))

global_results

# Post-hoc test if global test significant : Dunn test
posthoc_tests <- suivi_sep %>%
  group_by(espece) %>%
  dunn_test(
    hauteur.max.feuille ~ annee,
    p.adjust.method = "bonferroni"
  ) %>%
  mutate(
    p.signif = case_when(
      p.adj < 0.001 ~ "***",
      p.adj < 0.01 ~ "**",
      p.adj < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )

posthoc_tests

###################################################################################
#### Barplot per species : evolution of Hmax at the end of the growing seasons ####
###################################################################################

# Preparation for statistical information
comparisons_plot <- posthoc_tests %>%
  mutate(
    y.position = max(summary_data$mean_h + summary_data$sd_h) +
      row_number()*3
  ) %>%
  select(espece, group1, group2, p.adj, p.signif, y.position)

# Plot creation
cols <- c("Carex nigra"="#117733","Eriophorum angustifolium"="#999933")

p <- ggplot(summary_data,
            aes(x = annee,
                y = mean_h,
                fill = espece)) +
  
  geom_col(width = 0.7,
           color = "black") +
  
  geom_errorbar(aes(
    ymin = mean_h - sd_h,
    ymax = mean_h + sd_h),
    width = 0.2) +
  
  facet_wrap(~espece, ncol = 3) +
  
  scale_fill_manual(values = cols) +
  
  labs(
    x = "",
    y = "Maximum height of the tallest leaf per tussock (cm)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 22),
    axis.title = element_text(size = 16)
  )

# Plot with statistic results
p +
  stat_pvalue_manual(
    comparisons_plot,
    label = "p.signif",
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    size = 7
  )

