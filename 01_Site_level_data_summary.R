#Data summary and visualization of site level (density) I. crateriformis. Comparing south side filtered density w/ that including north.

rm(list=ls())
library(ggridges)
library(ggplot2)
library(tidyr)
library(rstatix)
library(ggsignif)
library(patchwork)
library(ggtext)
library(viridis)

load("data/ALL_COLONY_DENSITY.RData")
load("data/SOUTH_COLONY_DENSITY_filtered.RData")

# set colors
vir_colors <- viridis(n = 4, option = "C")
print(vir_colors)
custom_colors <- vir_colors
custom_colors[4] <- "gold"  # DAA520 goldenrod 

#how many sites were counted for density 
summary_by_year_and_total_all <- ALL_COLONY_DENSITY %>%
  group_by(YEAR) %>%
  summarise(
    non_na_count = sum(!is.na(DENSITY)),
    na_count = sum(is.na(DENSITY)),
    zeros = sum(DENSITY == 0),
    .groups = "drop"  
  ) 

#YEAR  non_na_count na_count zeros
#1 2015            57        0    42 #15 where density >0
#2 2018            17        0    15 #only @ 2 sites in 2018 where density was > 0
#3 2023            41        0    29 #11 sites where density >0
#4 2025            63        0    30 #33 sites where density >0

#how many sites were counted density in south only, filtering out large colonies
summary_by_year_and_total <- SOUTH_COLONY_DENSITY_filtered %>%
  group_by(YEAR) %>%
  summarise(
    non_na_count = sum(!is.na(adjusted_density)),
    na_count = sum(is.na(adjusted_density)),
    zeros = sum(adjusted_density == 0),
    .groups = "drop"  
  ) 
#YEAR non_na_count na_count zeros
#1  2015           35        0    23 #12 sites density >0
#2  2018           10        0     8 #only 2 sites in 2018 where density was > 0
#3  2023           29        0    18 #11 sites density >0
#4  2025           42        0    11 #31 

#Exploring density data by survey method including at absent areas
#test if size distribution is normal
shapiro.test(ALL_COLONY_DENSITY$DENSITY)
#Not normal, W = 0.31699, p-value < 2.2e-16
shapiro.test(SOUTH_COLONY_DENSITY_filtered$adjusted_density)
#W = 0.40396, p-value < 2.2e-16

#run Kruskal test 
kw_results <- ALL_COLONY_DENSITY %>%
  kruskal_test(DENSITY ~ YEAR)
#DENSITY   178      9.94     3 0.0191 Kruskal-Wallis

kw_results_filtered <- SOUTH_COLONY_DENSITY_filtered %>%
  kruskal_test(adjusted_density ~ YEAR)
#DENSITY   116      9.63     3 0.022 Kruskal-Wallis

dunn_results <- ALL_COLONY_DENSITY %>%
  dunn_test(DENSITY ~ YEAR, p.adjust.method = "bonferroni")
#ns
dunn_results_filtered <- SOUTH_COLONY_DENSITY_filtered %>%
  dunn_test(adjusted_density ~ YEAR, p.adjust.method = "bonferroni")
#difference between 2015 and 2025(p=0.00741, padj = 0.0445)

#Due to different site numbers in 2025, should bootstrap all data besides 2018 to 10 (# sites in 2018, the lowest).

#########################################
#visualize log-transformed distributions#
#########################################

# **Exploring transformed density data by survey method**
# Log-transform ICRA densities (log(x + 1))
SOUTH_COLONY_DENSITY_filtered <- SOUTH_COLONY_DENSITY_filtered %>%
  mutate(Log_ICRA_density = log1p(adjusted_density))%>%  # log1p(x) is equivalent to log(x + 1)
  mutate(Sqrt_ICRA_density = sqrt(adjusted_density)) #sruare root transform as it handles zeros better

# Test if log-transformed data is normal
shapiro.test(SOUTH_COLONY_DENSITY_filtered$adjusted_density)
shapiro.test(SOUTH_COLONY_DENSITY_filtered$Log_ICRA_density)
shapiro.test(SOUTH_COLONY_DENSITY_filtered$Sqrt_ICRA_density)
#none of the data are normal after transformation

# density plots
p1 <- ggplot(filtered_density, aes(x = DENSITY)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Original Data", x = "ICRA_raw_density")

p2 <- ggplot(filtered_density, aes(x = Log_ICRA_density)) +
  geom_density(fill = "red", alpha = 0.5) +
  labs(title = "Log-Transformed", x = "Log_ICRA_density")

p3 <- ggplot(filtered_density, aes(x = Sqrt_ICRA_density)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(title = "Square Root Transformed", x = "Sqrt_ICRA_density")

# qq plots
qq1 <- ggplot(filtered_density, aes(sample = DENSITY)) +
  stat_qq() + stat_qq_line() + labs(title = "QQ Plot: Original Data")

qq2 <- ggplot(filtered_density, aes(sample = Log_ICRA_density)) +
  stat_qq() + stat_qq_line() + labs(title = "QQ Plot: Log-Transformed")

qq3 <- ggplot(filtered_density, aes(sample = Sqrt_ICRA_density)) +
  stat_qq() + stat_qq_line() + labs(title = "QQ Plot: Square Root Transformed")

# save these in a grid
(p1 | p2 | p3) / (qq1 | qq2 | qq3)
ggsave("plots/south only density data transformation distributions.png")

# ridge plot of density
ggplot(filtered_density, aes(x = Sqrt_ICRA_density, y = as.factor(YEAR), fill = as.factor(YEAR))) +
  geom_density_ridges(alpha = 0.9) +
  labs(x = "Count (Sqrt transformed)", y = "Year", title = "Density Distribution of Counts by Year") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = custom_colors)

#  data to long format for bar plot and log transform do deal with zeros
mean_sd_den_per_year_site <- filtered_density %>%
  group_by(YEAR) %>%
  summarise(
    mean_den = mean(Sqrt_ICRA_density, na.rm = TRUE),
    sd_density = sd(Sqrt_ICRA_density, na.rm = TRUE),
    .groups = "drop"
  )

# make a bar plot
ggplot(mean_sd_den_per_year_site, aes(x = as.factor(YEAR), y = mean_den, fill=as.factor(YEAR))) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean_den - sd_density, ymax = mean_den + sd_density), 
                width = 0.25) +
  labs(x = "Year", y = "Mean Density ± SD", title = "Average Density per Year with SD") +
  theme_minimal()+
  scale_fill_manual(values = custom_colors)
###########################################################################
# bootstrap this data to deal with how different the site #s were per year#
###########################################################################

library(dplyr)
library(boot)
library(lme4)
library(broom.mixed)
library(tidyverse)
#bootstrap function to estimate uncertainty for mean density per year, based on resampling

# since 2018 had lowest numbner of sites sampled, bootstrap others to 10. 


dat_sub <- SOUTH_COLONY_DENSITY_filtered %>%
  filter(!is.na(adjusted_density)) %>%
  mutate(YEAR = factor(YEAR, ordered = FALSE)) %>%
  mutate(YEAR = relevel(YEAR, ref = "2015")) %>%
  
run_bootstrap <- function(n_boot = 100, target_n_sites = 10, set.seed(123)) {
  boot_results <- vector("list", n_boot)
  
  for (i in 1:n_boot) {
    # Sample sites for 2025
    sampled_sites <- dat_sub %>%
      filter(YEAR != 2018) %>%
      distinct(SITE) %>%
      slice_sample(n = target_n_sites) %>%
      pull(SITE)
    
    # Create bootstrapped dataset
    boot_data <- dat_sub %>%
      filter(YEAR != 2018 | SITE %in% sampled_sites)
    
    # Fit beta regression model
    model <- glm(adjusted_density ~ factor(YEAR) * (SITE), data = boot_data)
    
    # Store coefficients
    boot_results[[i]] <- tidy(model) %>%
      select(term, estimate) %>%
      mutate(bootstrap = i)
  }
  
  # Combine results
  boot_df <- bind_rows(boot_results)
  
  # Summarize bootstrapped estimates
  boot_summary <- boot_df %>%
    group_by(term) %>%
    summarise(
      estimate_mean = mean(estimate),
      estimate_sd = sd(estimate),
      lower_CI = quantile(estimate, 0.025),
      upper_CI = quantile(estimate, 0.975)
    )
  
  return(boot_summary)
}

#THIS ISN"T WORKING
n_boot <- 1000
target_n_sites <- 15  # 2018 only had 10 sites on south shore. 

set.seed(123)
boot_results <- vector("list", n_boot)

for (i in 1:n_boot) {
  
  # List of bootstrapped site names for all years except 2018
  sampled_sites <- SOUTH_COLONY_DENSITY_filtered %>%
    filter(YEAR != 2018) %>% #not bootstrapping 2018
    distinct(YEAR, SITE) %>%
    group_by(YEAR) %>%
    slice_sample(n = target_n_sites, replace = FALSE) %>%
    ungroup()
  
  # add 2018 sites without resampling
  sites_2018 <- SOUTH_COLONY_DENSITY_filtered %>%
    filter(YEAR == 2018) %>%
    distinct(YEAR, SITE)
  
  sampled_sites_all <- bind_rows(sampled_sites, sites_2018)
  
  # Keep only rows from sampled sites
  boot_data <- SOUTH_COLONY_DENSITY_filtered %>%
    semi_join(sampled_sites_all, by = c("YEAR", "SITE"))
  
  # Fit a linear mixed model: random intercept for Site
  #model <- lmer(adjusted_density ~ factor(YEAR) + (1 | SITE), data = boot_data)
  
  # Store model output
  #boot_results[[i]] <- tidy(model, effects = "fixed")  # Only fixed effects
}

# Combine and summarize results
boot_df <- bind_rows(boot_results, .id = "bootstrap")



# Combine and summarize
boot_df <- bind_rows(boot_results, .id = "bootstrap")

boot_summary <- boot_df %>%
  group_by(term) %>%
  summarise(
    estimate_mean = mean(estimate),
    estimate_sd = sd(estimate),
    lower_CI = quantile(estimate, 0.025),
    upper_CI = quantile(estimate, 0.975),
    .groups = "drop"
  )

boot_summary



# bar plot
ggplot(boot_summary, aes(x = as.factor(YEAR), y = mean, fill = as.factor(YEAR))) +
  geom_col() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(x = "Year", y = "Mean Density (bootstrapped CI)") +
  theme_minimal()+
  scale_fill_manual(values = custom_colors)


# do the same thing but with log transformed data
boot_mean_log <- function(data, indices) {
  d <- data[indices, ]
  d$log_density <- log1p(d$DENSITY)
  tapply(d$log_density, d$YEAR, mean, na.rm = TRUE)
}

set.seed(123)
boot_log_out <- boot(filtered_density, statistic = boot_mean_log, R = 1000)

# Back-transform (expm1 reverses log1p)
boot_means_log <- data.frame(
  YEAR = sort(unique(filtered_density$YEAR)),
  mean = expm1(colMeans(boot_log_out$t)),
  lower = expm1(apply(boot_log_out$t, 2, quantile, 0.025)),
  upper = expm1(apply(boot_log_out$t, 2, quantile, 0.975))
)

ggplot(boot_means_log, aes(x = as.factor(YEAR), y = mean, fill = as.factor(YEAR))) +
  geom_col() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(x = "Year", y = "Log (Mean Density) (colonies per m²)", fill="Year") +
  theme_minimal()+
  theme(
    panel.grid = element_blank(), 
    #panel.border = element_rect(color = "black", size = 1),  
    axis.text.x = element_text(size = 16),  
    axis.text.y = element_text(size = 16),  
   axis.title = element_text(size = 18),  
    text = element_text(size = 14),  
    plot.title = element_text(hjust = 0.5),  
    legend.position = "none",  
    legend.key.size = unit(0.6, "cm"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16)
  ) +
  scale_fill_manual(values = custom_colors)
#need to manually add significance from the Kruskal test above

ggsave("plots/south only density data barplot bootstrapped2.png", width = 4, height = 6, dpi = 300)
