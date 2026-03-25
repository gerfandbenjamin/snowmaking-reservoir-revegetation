################################################################################
# Ecological quality of snowmaking reservoirs (French Alps)
# Physico-chemical comparison: Reservoirs vs Lakes
# Violin plots + non-parametric statistics
# Version : August 2023
################################################################################

#### Packages ####
library(dplyr)
library(readr)
library(vioplot)
library(car)

#### Data import ####
data_path <- "data_path"  # adapt to your project structure

RA_raw <- read.csv2(paste(data_path,"compil_TE$tot.csv",sep="/"),sep=';',dec=",")
Lakes_raw <- read.csv2(paste(data_path,"bg_physico-chimie_lacs.csv",sep="/"),sep=';',dec=",")

################################################################################
# Data preparation
################################################################################

# Standard variable names
common_vars <- c("ph", "cond", "chla", "no3", "tac", "po2")

# Clean Reservoir dataset
RA <- RA_raw %>%
  filter(!(alim_active == "oui" & loc == "alim") | prel == "oui") %>%
  select(nom,
         ph,
         cond,
         chla,
         NO3_mgN.L,
         tac,
         pour.O2_cor) %>%
  rename(
    no3 = NO3_mgN.L,
    po2 = pour.O2_cor
  ) %>%
  group_by(nom) %>%
  summarise(across(all_of(common_vars), mean, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    tac = tac / 5,  # conversion if needed
    type = "Reservoirs"
  ) %>%
  filter(nom != "R2")

# Clean Lakes dataset
Lakes <- Lakes_raw %>%
  select(
    Lac,
    pH,
    conductivité_microS.cm,
    chla_microg.L,
    NO3_mg.L,
    TAC_meq.L,
    pour.O2_cor
  ) %>%
  rename(
    nom  = Lac,
    ph   = pH,
    cond = conductivité_microS.cm,
    chla = chla_microg.L,
    no3  = NO3_mg.L,
    tac  = TAC_meq.L,
    po2  = pour.O2_cor
  ) %>%
  mutate(type = "Lakes")

# Merge datasets
dataset <- bind_rows(
  RA %>% select(-nom),
  Lakes %>% select(-nom)
)

################################################################################
# Violin plots
################################################################################

# Couleurs
cols <- c("Lakes" = "royalblue2",
          "Reservoirs" = "firebrick1")

# Variables & labels for plots
plot_vars <- c("cond", "po2", "chla", "no3", "tac", "ph")
plot_labels <- c("Conductivity (µS/cm)",
                 "%O2",
                 "[Chlorophyll a] (µg/L)",
                 "[NO3-] (mg/L)",
                 "TAC (meq/L)",
                 "pH")

# Detection thresholds for Chla et NO3
thresholds <- list(chla = 0.5,  # µg/L
                   no3  = 0.5/4.42)  # mg/L N ?

# Plots disposition
par(mfrow = c(2, 3))

# Loop by variable
for(i in seq_along(plot_vars)) {
  
  v <- plot_vars[i]
  label <- plot_labels[i]
  
  # Sample sizes corrects
  n_lake <- sum(!is.na(dataset[[v]][dataset$type == "Lakes"]))
  n_res  <- sum(!is.na(dataset[[v]][dataset$type == "Reservoirs"]))
  
  # Wilcoxon test
  test <- wilcox.test(as.formula(paste(v, "~ type")), data = dataset)
  pval <- test$p.value
  signif_code <- cut(pval, breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                     labels = c("***", "**", "*", ".", "n.s."))
  
  # Violin plot
  vioplot(dataset[[v]] ~ dataset$type,
          col = cols,
          xlab = "", ylab = "")
  
  stripchart(dataset[[v]] ~ dataset$type,
             vertical = TRUE, method = "jitter",
             pch = 19, add = TRUE, col = c(7,3))
  
  # Title and panel letter
  mtext(paste(label, signif_code, sep = " - "), side = 3, line = 0.5, cex = 1.3)
  mtext(letters[i], side = 1, adj = 0, line = 0.7, font = 2, cex = 1.3)
  
  # Sample sizes
  ymax <- max(dataset[[v]], na.rm = TRUE)
  text(0.8, ymax*0.9, paste("n =", n_lake), font = 3)
  text(1.8, ymax*0.9, paste("n =", n_res), font = 3)
  
  # Thresholds
  if(v == "chla") {
    abline(h = thresholds$chla, lty = 2, col = 8)
    n_under <- sum(dataset[[v]] < thresholds$chla & dataset$type == "Reservoirs", na.rm = TRUE)
    text(2.5, thresholds$chla, "LQ", col = 8, pos = 3)
    text(2.5, 0, paste("n =", n_under), col = 8, xpd = TRUE)
  }
  
  if(v == "no3") {
    abline(h = thresholds$no3, lty = 2, col = 8)
    n_under <- sum(dataset[[v]] < thresholds$no3 & dataset$type == "Reservoirs", na.rm = TRUE)
    text(2.5, thresholds$no3, "LQ", col = 8, pos = 3)
    text(2.5, 0, paste("n =", n_under), col = 8, xpd = TRUE)
  }
}



par(mfrow = c(1, 1))


################################################################################
# Additional statistical analyses
################################################################################

# List of parameters to test
params <- common_vars

# Optional: nicer labels for reporting
param_labels <- c("pH", 
                  "Conductivity (µS/cm)",
                  "[Chlorophyll a] (µg/L)",
                  "[NO3-] (mg/L)",
                  "TAC (meq/L)",
                  "%O2")

# Initialize results storage
results <- data.frame(Parameter = param_labels,
                      W = NA,
                      p_value = NA,
                      stringsAsFactors = FALSE)

# Loop over parameters and perform Wilcoxon rank-sum tests
for(i in seq_along(params)){
  
  param <- params[i]
  
  # Wilcoxon rank-sum test: Reservoirs vs Lakes
  test <- wilcox.test(dataset[[param]] ~ dataset$type, data = dataset, exact = FALSE)
  
  # Store results
  results$W[i] <- test$statistic
  results$p_value[i] <- test$p.value
}

# Compute pH range and fold difference
pH_range_reservoirs <- max(dataset$ph[dataset$type == "Reservoirs"], na.rm = TRUE) -
  min(dataset$ph[dataset$type == "Reservoirs"], na.rm = TRUE)

pH_range_lakes <- max(dataset$ph[dataset$type == "Lakes"], na.rm = TRUE) -
  min(dataset$ph[dataset$type == "Lakes"], na.rm = TRUE)

pH_range_fold <- round(pH_range_lakes / pH_range_reservoirs, 1)

# Display results
print("Wilcoxon rank-sum tests for physico-chemical parameters:")
print(results)

cat("\nSurface water pH ranges:\n")
cat("Reservoirs: ", pH_range_reservoirs, "\n")
cat("Lakes:      ", pH_range_lakes, "\n")
cat("Fold difference (Lakes / Reservoirs): ", pH_range_fold, "x\n")
















# Range comparison
range_RA   <- diff(range(RA$pH, na.rm = TRUE))
range_Lake <- diff(range(Lakes$pH, na.rm = TRUE))

cat("pH range (Reservoirs):", range_RA, "\n")
cat("pH range (Lakes):", range_Lake, "\n")
cat("Range ratio (Lakes / Reservoirs):", 
    round(range_Lake / range_RA, 2), "\n")

# Levene test for homogeneity of variance
levene_result <- leveneTest(pH ~ type, data = dataset)
print(levene_result)