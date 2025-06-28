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
library(dplyr)
library(boot)
library(lme4)
library(broom.mixed)
library(tidyverse)

load("data/ALL_COLONY_DENSITY.RData")
load("data/SOUTH_COLONY_DENSITY_filtered.RData") #does not have north sites or Feb 2025 sites. Removed the largest corals in 2025.
load("data/FEB_SOUTH_COLONY_DENSITY.RData") #includes feb 2025 sites

# set colors
vir_colors <- viridis(n = 4, option = "C")
print(vir_colors)
custom_colors <- vir_colors
custom_colors[4] <- "gold"  # DAA520 goldenrod 

#how many sites were counted for density IN ALL
summary_by_year_and_total_all <- ALL_COLONY_DENSITY %>%
  group_by(YEAR) %>%
  summarise(
    non_na_count = sum(!is.na(DENSITY)),
    na_count = sum(is.na(DENSITY)),
    zeros = sum(DENSITY == 0),
    mean_den = mean(DENSITY, na.rm = TRUE),
    sd_density = sd(DENSITY, na.rm = TRUE),
    .groups = "drop"
  ) 

summary_by_year_site <- ALL_COLONY_DENSITY %>%
  group_by(YEAR) %>%
  summarise(num_sites = n_distinct(SITE))

#YEAR  non_na_count na_count zeros
#1 2015            57        0    42 #15 where density >0
#2 2018            17        0    15 #only @ 2 sites in 2018 where density was > 0
#3 2023            41        0    29 #11 sites where density >0
#4 2025            63        0    30 #33 sites where density >0

#how many sites were counted density in south only, filtering out large colonies and feb data
summary_by_year_and_total <- SOUTH_COLONY_DENSITY_filtered %>%
  group_by(YEAR) %>%
  summarise(
    non_na_count = sum(!is.na(adjusted_density)),
    na_count = sum(is.na(adjusted_density)),
    zeros = sum(adjusted_density == 0),
    mean_den = mean(adjusted_density, na.rm = TRUE),
    sd_density = sd(adjusted_density, na.rm = TRUE),
    CI_Lower = mean_den - qt(0.975, df = n()-1) * sd_density / sqrt(n()),
    CI_Upper = mean_den + qt(0.975, df = n()-1) * sd_density / sqrt(n()),
    .groups = "drop"
  ) 
#YEAR non_na_count na_count zeros  mean_den sd_density
#1 2015           35        0    23    0.151      0.359   #12 sites
#2  2018           10        0     8    0.44       1.17   #2 sites
#3  2023           29        0    18    0.620      1.71   #11 sites 
#4  2025           15        0     3    0.618      0.896  #12 sites  were >0

#how many sites were counted density in south only (this includes feb data, not using in paper)
summary_by_year_and_total1 <- south_den1 %>%
  group_by(YEAR) %>%
  summarise(
    non_na_count = sum(!is.na(DENSITY)),
    na_count = sum(is.na(DENSITY)),
    zeros = sum(DENSITY == 0),
    mean_den = mean(DENSITY, na.rm = TRUE),
    sd_density = sd(DENSITY, na.rm = TRUE),
    .groups = "drop"
  ) 
#YEAR non_na_count na_count zeros
#1  2015           35        0    23    0.151      0.359 #12
#2  2018           10        0     8    0.44       1.17    #2
#3  2023           29        0    18    0.620      1.71    #11
#4  2025           42        0     11    0.391      0.775  #32 sites 

#Exploring density data by survey method including at absent areas
#test if size distribution is normal
shapiro.test(ALL_COLONY_DENSITY$DENSITY)
#Not normal, W = 0.31699, p-value < 2.2e-16
shapiro.test(SOUTH_COLONY_DENSITY_filtered$adjusted_density)
#W = 0.40396, p-value < 2.2e-16
shapiro.test(south_den1$DENSITY)
#W = 0.40459, p-value < 2.2e-16

#run Kruskal test 
kw_results <- ALL_COLONY_DENSITY %>%
  kruskal_test(DENSITY ~ YEAR)
#DENSITY   178      9.94     3 0.0191 Kruskal-Wallis

kw_results_filtered <- SOUTH_COLONY_DENSITY_filtered %>%
  kruskal_test(adjusted_density ~ YEAR)
#DENSITY   89      9.27     3 0.0259 Kruskal-Wallis

kw_results_feb <- south_den1 %>%
  kruskal_test(DENSITY ~ YEAR)
#DENSITY   116      9.63     3 0.022 Kruskal-Wallis


dunn_results <- ALL_COLONY_DENSITY %>%
  dunn_test(DENSITY ~ YEAR, p.adjust.method = "bonferroni")
#ns
dunn_results_filtered <- SOUTH_COLONY_DENSITY_filtered %>%
  dunn_test(adjusted_density ~ YEAR, p.adjust.method = "bonferroni")
#difference between 2015 and 2025(p=0.00489, padj = 0.0294)

dunn_results_feb <- south_den1 %>%
  dunn_test(DENSITY ~ YEAR, p.adjust.method = "bonferroni")
#difference between 2015 and 2025(p=0.00741, padj = 0.0445) 

#Due to different site numbers in 2025, could bootstrap all data besides 2018 to 10 (# sites in 2018, the lowest).

ggplot(SOUTH_COLONY_DENSITY_filtered, aes(x = as.factor(YEAR), y = adjusted_density, fill = YEAR)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
  theme_minimal()

SOUTH_COLONY_DENSITY_filtered<-SOUTH_COLONY_DENSITY_filtered%>%
mutate(adjusted_density_log = log10(adjusted_density + 1e-4))

ggplot(SOUTH_COLONY_DENSITY_filtered, aes(x = as.factor(YEAR), y = adjusted_density_log, fill = YEAR)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
  theme_minimal()

ggplot(SOUTH_COLONY_DENSITY_filtered, aes(x = as.factor(YEAR), y = adjusted_density, fill = as.factor(YEAR))) +
  geom_boxplot() +
  #geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
  scale_y_log10() +
  ylab("Adjusted Density (log₁₀ scale)") +
  theme_minimal()+
  scale_fill_manual(values = custom_colors)
ggsave("plots/logden_boxplot_south_nofebfiltered.svg")

svglite("plots/my_plot.svg", width = 3, height = 4)
ggplot(SOUTH_COLONY_DENSITY_filtered, aes(x = as.factor(YEAR), y = adjusted_density, fill = as.factor(YEAR))) +
  geom_boxplot() +
  #geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
  scale_y_log10() +
  ylab("Adjusted Density (log₁₀ scale)") +
  theme_minimal()+
  theme(panel.grid = element_blank())+
  scale_fill_manual(values = custom_colors)
dev.off()


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
p1 <- ggplot(SOUTH_COLONY_DENSITY_filtered, aes(x = DENSITY)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Original Data", x = "ICRA_raw_density")

p2 <- ggplot(SOUTH_COLONY_DENSITY_filtered, aes(x = Log_ICRA_density)) +
  geom_density(fill = "red", alpha = 0.5) +
  labs(title = "Log-Transformed", x = "Log_ICRA_density")

p3 <- ggplot(SOUTH_COLONY_DENSITY_filtered, aes(x = Sqrt_ICRA_density)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(title = "Square Root Transformed", x = "Sqrt_ICRA_density")

# qq plots
qq1 <- ggplot(SOUTH_COLONY_DENSITY_filtered, aes(sample = DENSITY)) +
  stat_qq() + stat_qq_line() + labs(title = "QQ Plot: Original Data")

qq2 <- ggplot(SOUTH_COLONY_DENSITY_filtered, aes(sample = Log_ICRA_density)) +
  stat_qq() + stat_qq_line() + labs(title = "QQ Plot: Log-Transformed")

qq3 <- ggplot(SOUTH_COLONY_DENSITY_filtered, aes(sample = Sqrt_ICRA_density)) +
  stat_qq() + stat_qq_line() + labs(title = "QQ Plot: Square Root Transformed")

# save these in a grid
(p1 | p2 | p3) / (qq1 | qq2 | qq3)
ggsave("plots/south only density data transformation distributions.png")

# ridge plot of density
ggplot(SOUTH_COLONY_DENSITY_filtered, aes(x = Sqrt_ICRA_density, y = as.factor(YEAR), fill = as.factor(YEAR))) +
  geom_density_ridges(alpha = 0.9) +
  labs(x = "Count (Sqrt transformed)", y = "Year", title = "Density Distribution of Counts by Year") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = custom_colors)

#  data to long format for bar plot and log transform do deal with zeros
mean_sd_den_per_year_site_sqrt <- SOUTH_COLONY_DENSITY_filtered %>%
  group_by(YEAR) %>%
  summarise(
    mean_den = mean(Sqrt_ICRA_density, na.rm = TRUE),
    sd_density = sd(Sqrt_ICRA_density, na.rm = TRUE),
    n = sum(!is.na(DENSITY)),
    se = sd_density / sqrt(n),
    lower_CI = mean_den - qt(0.975, df = n - 1) * se,
    upper_CI = mean_den + qt(0.975, df = n - 1) * se,
    .groups = "drop"
  )

mean_sd_den_per_year_site_feb <- south_den1 %>%
  group_by(YEAR) %>%
  summarise(
    mean_den = mean(DENSITY, na.rm = TRUE),
    sd_density = sd(DENSITY, na.rm = TRUE),
    n = sum(!is.na(DENSITY)),
    se = sd_density / sqrt(n),
    lower_CI = mean_den - qt(0.975, df = n - 1) * se,
    upper_CI = mean_den + qt(0.975, df = n - 1) * se,
    .groups = "drop"
  )

mean_sd_den_per_year_site_feb <- south_den1 %>%
  group_by(YEAR) %>%
  summarise(
    mean_den = mean(DENSITY, na.rm = TRUE),
    sd_density = sd(DENSITY, na.rm = TRUE),
    n = sum(!is.na(DENSITY)),
    se = sd_density / sqrt(n),
    lower_CI = mean_den - qt(0.975, df = n - 1) * se,
    upper_CI = mean_den + qt(0.975, df = n - 1) * se,
    .groups = "drop"
  )

mean_sd_den_per_year_site_no <- SOUTH_COLONY_DENSITY_filtered %>%
  group_by(YEAR) %>%
  summarise(
    mean_den = mean(adjusted_density, na.rm = TRUE),
    sd_density = sd(adjusted_density, na.rm = TRUE),
    n = sum(!is.na(DENSITY)),
    se = sd_density / sqrt(n),
    lower_CI = mean_den - qt(0.975, df = n - 1) * se,
    upper_CI = mean_den + qt(0.975, df = n - 1) * se,
    .groups = "drop"
  )

mean_sd_den_per_year_site_w0 <- SOUTH_COLONY_DENSITY_filtered %>%
  group_by(YEAR) %>%
  summarise(
    mean_den = mean(adjusted_density),
    sd_density = sd(adjusted_density),
    n = sum(!is.na(DENSITY)),
    se = sd_density / sqrt(n),
    lower_CI = mean_den - qt(0.975, df = n - 1) * se,
    upper_CI = mean_den + qt(0.975, df = n - 1) * se,
    .groups = "drop"
  )

mean_sd_den_per_year_site_all <- ALL_COLONY_DENSITY %>%
  group_by(YEAR) %>%
  summarise(
    mean_den = mean(DENSITY, na.rm = TRUE),
    sd_density = sd(DENSITY, na.rm = TRUE),
    n = sum(!is.na(DENSITY)),
    se = sd_density / sqrt(n),
    lower_CI = mean_den - qt(0.975, df = n - 1) * se,
    upper_CI = mean_den + qt(0.975, df = n - 1) * se,
    .groups = "drop"
  )

# make a bar plot

svglite("plots/raw_density_barplotCI.svg", width = 4, height = 6)
ggplot(mean_sd_den_per_year_site_w0, aes(x = as.factor(YEAR), y = mean_den, fill=as.factor(YEAR))) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_errorbar(aes(
    ymin = pmax(lower_CI, 0),
    ymax = upper_CI
  ), width = 0.25)+
#geom_errorbar(aes(
    # ymin = pmax(mean_den - sd_density, 0),   # Prevent error bars from going below zero
    # ymax = mean_den + sd_density), width = 0.25) +
  labs(x = "Year", y = "Mean Density ± CI") +
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
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.1)))+
  scale_fill_manual(values = custom_colors)
dev.off()
ggsave("plots/den_boxplot_south_nofeb_rawCI.png", width = 4, height = 6, dpi = 300)

#with feb data
ggplot(mean_sd_den_per_year_site_feb, aes(x = as.factor(YEAR), y = mean_den, fill=as.factor(YEAR))) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(
    ymin = pmax(lower_CI, 0),
    ymax = upper_CI
  ), width = 0.25)+
  labs(x = "Year", y = "Mean Density ± CI") +
  theme_minimal()+
  scale_fill_manual(values = custom_colors)
ggsave("plots/den_boxplot_south_wfeb.png")

#with transformed south filtered data
ggplot(mean_sd_den_per_year_site_sqrt, aes(x = as.factor(YEAR), y = mean_den, fill=as.factor(YEAR))) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_errorbar(aes(
    ymin = pmax(lower_CI, 0),
    ymax = upper_CI
  ), width = 0.25)+
  labs(x = "Year", y = "Mean Density ± CI", 
       #title = "Sqrt South no feb average density"
       ) +
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
  scale_x_discrete(expand = expansion(mult = c(0.01, 0.01)))+
  scale_fill_manual(values = custom_colors)
ggsave("plots/den_boxplot_south_nofeb_sqrt.png", width = 4, height = 6, dpi = 300)

#with all data
ggplot(mean_sd_den_per_year_site_all, aes(x = as.factor(YEAR), y = mean_den, fill=as.factor(YEAR))) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(
    ymin = pmax(lower_CI, 0),
    ymax = upper_CI
  ), width = 0.25)+
  labs(x = "Year", y = "Mean Density ± CI", title = "All average density") +
  theme_minimal()+
  scale_fill_manual(values = custom_colors)
ggsave("plots/den_boxplot_all.png")
