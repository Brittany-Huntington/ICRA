rm(list = ls())
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(readxl)
library(betareg)
library(purrr)
library(broom)
library(glmmTMB)
library(emmeans)
library(broom.mixed)
library(multcomp)
library(emmeans)

load(file ="data/All_ICRA_SIZE_PM.RData") #north and south
load(file = "data/south_only_ICRA_Colony_level_data.csv") #this contains all south data
load(file = "data/ICRA_SIZE_PM_SOUTH_sizefiltered.RData") #south data from all years but 2025 only has march data bc large colonies were removed 
load(file = "data/ICRA_2025_SIZE_PM_nofeb.RData") #march 2025 data without large colonies. USE


# set colors
vir_colors <- viridis(n = 4, option = "C")
print(vir_colors)

custom_colors <- vir_colors
custom_colors[4] <- "gold"  # DAA520 goldenrod 

#Data summaries

#how many corals were sized (Feb 2025 data size wasn't taken)
summary_by_year_and_total_all <- ALL_ICRA_SIZE_PM %>%
  group_by(Data_Source, YEAR) %>%
  summarise(
    non_na_count = sum(!is.na(COLONYLENGTH)),
    na_count = sum(is.na(COLONYLENGTH)),
    .groups = "drop"  
  ) 
#Data_Source YEAR  non_na_count na_count
# esa         2025           629      648
# ncrmp2      2015            58        0
# ncrmp2      2018            44        0
# ncrmp2      2023           180        0

#how many corals were sized in south sites only (Feb 2025 size wasn't taken)
summary_by_year_and_total_SOUTH <- ICRA_SIZE_PM_nofeb %>%
  group_by(Data_Source, YEAR) %>%
  summarise(
    non_na_count = sum(!is.na(COLONYLENGTH)),
    na_count = sum(is.na(COLONYLENGTH)),
    .groups = "drop"  
  ) 
#1 esa          2025          605        0
#2 ncrmp2       2015           52        0
#3 ncrmp2       2018           44        0
#4 ncrmp2       2023          179        0

#how many corals were measured PM by year and size class: NA = feb 2025 data
summary_by_year_and_totalPM_all <- ALL_ICRA_SIZE_PM %>%
  group_by(Data_Source, YEAR, TAIL_BINS) %>%
  summarise(non_na_count = sum(!is.na(PER_DEAD)),
            na_count = sum(is.na(PER_DEAD)) )

#how many corals were measured PM by year and size class: NA = feb 2025 data
summary_by_year_and_totalPM_south <- ICRA_SIZE_PM %>%
  group_by(Data_Source, YEAR, TAIL_BINS) %>%
  summarise(non_na_count = sum(!is.na(PER_DEAD)),
            na_count = sum(is.na(PER_DEAD)) )
#in 2025, 195 large corals were found, while in 2023, 35 were counted
#Data_Source  YEAR TAIL_BINS non_na_count na_count
# esa          2025 Q20                109        0
# esa          2025 Q80                195        0
# esa          2025 QMED               301        0
# ncrmp2       2015 Q20                 16        0
# ncrmp2       2015 Q80                  6        0
# ncrmp2       2015 QMED                31        0
# ncrmp2       2018 Q20                 11        0
# ncrmp2       2018 Q80                 11        0
# ncrmp2       2018 QMED                22        0
# ncrmp2       2023 Q20                 31        0
# ncrmp2       2023 Q80                 35        0
# ncrmp2       2023 QMED               113        0

#should bootstrap these data due to the bias for large colonies in 2025 (indeed, 195 large corals were sampled as opposed to 35 in 2023)



  # Compute mean size in 2025 of all ICRA
ICRA_2025_unfiltered<-ALL_ICRA_SIZE_PM%>%
  filter(YEAR=="2025")

ICRA_PM_site_2025<-ICRA_2025_unfiltered%>%
  group_by(SITE, LATITUDE, LONGITUDE) %>%
  summarise(date = min(DATE_), #keep date column
            mean_PM = mean(PER_DEAD, na.rm = TRUE),
            sd_PM = sd(PER_DEAD, na.rm = TRUE),
            maxPM = if (all(is.na(PER_DEAD))) NA_real_ else max(PER_DEAD, na.rm = TRUE),
            minPM = if (all(is.na(PER_DEAD))) NA_real_ else min(PER_DEAD, na.rm = TRUE),
            n = sum(!is.na(PER_DEAD)),
            se = sd_PM / sqrt(n),
            .groups = "drop")

save(ICRA_PM_site_2025, file = "data/ICRA_PM_site2025.RData")

ICRA_PM_S_site_2025<-ICRA_PM_site_2025%>%
  filter(!is.na(mean_PM), !SITE %in% c(
    "TUT-061",
    "TUT-062",
    "TUT-201",
    "TUT-069",
    "TUT-202",
    "TUT-066",
    "TUT-203",
    "TUT-058",
    "TUT-060",
    "TUT-025",
    "TUT-041",
    "TUT-030",
    "TUT-215",
    "TUT-053",
    "TUT-224",
    "TUT-035",
    "TUT-052",
    "TUT-144"))

save(ICRA_PM_S_site_2025, file = "data/ICRA_PM_S_site2025.RData")

#2025 south colony level data:
ICRA_PM_S_colony_2025<-ICRA_2025_unfiltered%>%
  filter(!is.na(PER_DEAD), !SITE %in% c(
    "TUT-061",
    "TUT-062",
    "TUT-201",
    "TUT-069",
    "TUT-202",
    "TUT-066",
    "TUT-203",
    "TUT-058",
    "TUT-060",
    "TUT-025",
    "TUT-041",
    "TUT-030",
    "TUT-215",
    "TUT-053",
    "TUT-224",
    "TUT-035",
    "TUT-052",
    "TUT-144"))
save(ICRA_PM_S_colony_2025, file = "data/ICRA_PM_S_colony2025.RData")

#compute mean size per year for measured colonies
mean_size_per_year_march <- ALL_ICRA_SIZE_PM %>%
  group_by(YEAR, TAIL_BINS) %>%
  summarise(COLONYLENGTH = mean(COLONYLENGTH, na.rm = TRUE))
#1 2015  Q20               8.21
#2 2015  QMED             24.2 
#3 2015  Q80              51.5 
#4 2018  Q20               8.18
#5 2018  QMED             23.2 
#6 2018  Q80              50.3 
#7 2023  Q20               8.54
#8 2023  QMED             24.2 
#9 2023  Q80              57.5 
#10 2025  Q20               8.07
#11 2025  QMED             26.6 
#12 2025  Q80              66.1 
#13 2025  NA              NaN 

#when excluding noth: 
mean_size_per_year_south <- ICRA_SIZE_PM %>%
  group_by(YEAR, TAIL_BINS) %>%
  summarise(COLONYLENGTH = mean(COLONYLENGTH, na.rm = TRUE))
#1 2015 Q20               8.31
#2  2015 Q80              51.5 
#3  2015 QMED             24.0 
#4  2018 Q20               8.18
#5  2018 Q80              50.3 
#6  2018 QMED             23.2 
#7  2023 Q20               8.54
#8  2023 Q80              57.5 
#9  2023 QMED             24.2 
#10  2025 Q20               8.16
#11  2025 Q80              62.1 
#12  2025 QMED             26.7 

#when excluding north: 
mean_size_per_year_south <- ICRA_SIZE_PM %>%
  group_by(YEAR) %>%
summarise(mean_size = mean(COLONYLENGTH, na.rm = TRUE),
          sd_size = sd(COLONYLENGTH, na.rm = TRUE),
          n = sum(!is.na(COLONYLENGTH)),
          se = sd_size / sqrt(n),
          lower_CI = mean_size - qt(0.975, df = n - 1) * se,
          upper_CI = mean_size + qt(0.975, df = n - 1) * se,
          .groups = "drop")

# mean PM per year
mean_PM_per_year_all <- ALL_ICRA_SIZE_PM %>%
  group_by(YEAR) %>%
  summarise(mean_PM = mean(PER_DEAD, na.rm = TRUE),
            sd_PM = sd(PER_DEAD, na.rm = TRUE),
            n = sum(!is.na(PER_DEAD)),
            se = sd_PM / sqrt(n),
            lower_CI = mean_PM - qt(0.975, df = n - 1) * se,
            upper_CI = mean_PM + qt(0.975, df = n - 1) * se,
            .groups = "drop")
#  YEAR  mean_PM sd_PM     n    se lower_CI upper_CI
# 2015     7.48  15.9    58 2.09      3.30     11.7
# 2018     7.20  14.5    44 2.19      2.79     11.6
# 2023     7.9   15.2   180 1.13      5.66     10.1
# 2025    30.1   33.3  1157 0.980    28.2      32.0
# almost four fold increase in PM

# mean PM per year south only
mean_PM_per_year_south <- ICRA_SIZE_PM_nofeb %>%
  group_by(YEAR) %>%
  summarise(mean_PM = mean(PER_DEAD, na.rm = TRUE),
            sd_PM = sd(PER_DEAD, na.rm = TRUE),
            n = sum(!is.na(PER_DEAD)),
            se = sd_PM / sqrt(n),
            lower_CI = mean_PM - qt(0.975, df = n - 1) * se,
            upper_CI = mean_PM + qt(0.975, df = n - 1) * se,
            .groups = "drop")

#YEAR mean_PM sd_PM     n    se lower_CI upper_CI
#  2015    7.81  16.6    53  2.28     3.24     12.4
#  2018    7.20  14.5    44  2.19     2.79     11.6
#  2023    7.94  15.2   179  1.14     5.69     10.2
#  2025   33.0   34.2   605  1.39    30.3      35.8


# mean PM per year
mean_PM_per_year_bin_all <- ALL_ICRA_SIZE_PM %>%
  group_by(YEAR, TAIL_BINS) %>%
  summarise(mean_PM = mean(PER_DEAD, na.rm = TRUE))
#1 2015  Q20         2.63 
#2 2015  QMED        8.97 
#3 2015  Q80        14.7  
#4 2018  Q20         0.909
#5 2018  QMED        3.73 
#6 2018  Q80        20.5  
#7 2023  Q20         1.65 
#8 2023  QMED        7.71 
#9 2023  Q80        14.1  
#10 2025  Q20         1.76 
#11 2025  QMED       25.6  
#12 2025  Q80        63.3  
#13 2025  NA         25.6

# mean PM per year south only
mean_PM_per_year_south_bin <- ICRA_SIZE_PM_nofeb %>%
  group_by(YEAR, TAIL_BINS) %>%
  summarise(mean_PM = mean(PER_DEAD, na.rm = TRUE))
#1  2015 Q20         2.81 
#2  2015 Q80        14.7  
#3  2015 QMED        9.06 
#4  2018 Q20         0.909
#5  2018 Q80        20.5  
#6  2018 QMED        3.73 
#7  2023 Q20         1.65 
#8  2023 Q80        14.1  
#9  2023 QMED        7.77 
#10  2025 Q20         1.64 
#11  2025 Q80        61.9  
#12  2025 QMED       25.7 


#calculate summary stats of PM by year
summary_stats_year <- ICRA_SIZE_PM_nofeb %>%
  group_by(YEAR)%>%
  summarise(
    Mean_PM = mean(PER_DEAD, na.rm = TRUE),
    SD_PM = sd(PER_DEAD, na.rm = TRUE),
    SE_PM = SD_PM / sqrt(n()),
    CI_Lower = Mean_PM - qt(0.975, df = n()-1) * SD_PM / sqrt(n()),
    CI_Upper = Mean_PM + qt(0.975, df = n()-1) * SD_PM / sqrt(n()),
    N = n(), 
    .groups = "drop"
  )

#calculate summary stats of PM by size and year
summary_stats_size <- ICRA_SIZE_PM_nofeb %>%
  group_by(YEAR, TAIL_BINS)%>%
  summarise(
    Mean_PM = mean(PER_DEAD, na.rm = TRUE),
    SD_PM = sd(PER_DEAD, na.rm = TRUE),
    SE_PM = SD_PM / sqrt(n()),
    CI_Lower = Mean_PM - qt(0.975, df = n()-1) * SD_PM / sqrt(n()),
    CI_Upper = Mean_PM + qt(0.975, df = n()-1) * SD_PM / sqrt(n()),
    N = n(), 
    .groups = "drop"
  )
#write.csv(summary_stats_size, "summary_stats_PM_by_size_Year.csv", row.names = FALSE)

#correct order
summary_stats_size$TAIL_BINS <- factor(summary_stats_size$TAIL_BINS, 
                                       levels = c("Q20", "QMED", "Q80"))
#correct order
summary_stats_year$TAIL_BINS <- factor(summary_stats_year$TAIL_BINS, 
                                       levels = c("Q20", "QMED", "Q80"))
#prepare facet labels for plotting
facet_labels <- c(
  "Q20" = "Small (5-12 cm)",
  "QMED" = "Medium (13-39 cm)",
  "Q80" = "Large (>40 cm)"
)

# calc the max size (for plotting later)
max_size <- ICRA_SIZE_PM %>%
  group_by(YEAR) %>%
  summarise(
    max_size = max(COLONYLENGTH, na.rm = TRUE),
    N = n())
#YEAR  max_size
#1 2015        73
#2 2018        70
#3 2023       116
#4 2025       170


shapiro.test(ICRA_SIZE_PM$PER_DEAD)
#W = 0.76919, p-value < 2.2e-16

kw_results <- ICRA_SIZE_PM %>%
  kruskal_test(PER_DEAD ~ YEAR)
#PER_DEAD   881      95.9     3 1.16e-20 Kruskal-Wallis

kw_results_bin <- ICRA_SIZE_PM %>%
  kruskal_test(PER_DEAD ~ TAIL_BINS)
#PER_DEAD   881      357.     2 2.89e-78 Kruskal-Wallis

dunn_results <- ICRA_SIZE_PM %>%
  dunn_test(PER_DEAD ~ YEAR, p.adjust.method = "bonferroni")
#each year compared to 2025 sig (**** ) others not

dunn_results_bin <- ICRA_SIZE_PM %>%
  dunn_test(PER_DEAD ~ TAIL_BINS, p.adjust.method = "bonferroni")
#each is sig (****), doesn't tell us much b/c we put the bins on the data.

#test within each size bin
ICRA_SIZE_PM %>%
  filter (TAIL_BINS == 'Q20') %>%
  kruskal_test(PER_DEAD ~ YEAR)
#PER_DEAD   167      1.03     3 0.794 Kruskal-Walli

ICRA_SIZE_PM %>%
filter (TAIL_BINS == 'Q20') %>%
  dunn_test(PER_DEAD ~ YEAR, p.adjust.method = "bonferroni")

ICRA_SIZE_PM %>%
  filter (TAIL_BINS == 'QMED') %>%
  kruskal_test(PER_DEAD ~ YEAR)
#ER_DEAD   467      50.9     3 5.22e-11 Kruskal-Wallis

 ICRA_SIZE_PM %>%
   filter (TAIL_BINS == 'QMED') %>%
  dunn_test(PER_DEAD ~ YEAR, p.adjust.method = "bonferroni")
 #each year + 2025 is sig (** - ****)
 
 
 ICRA_SIZE_PM %>%
   filter (TAIL_BINS == 'Q80') %>%
   kruskal_test(PER_DEAD ~ YEAR)
 #PER_DEAD   247      79.8     3 3.44e-17 Kruskal-Wallis
 
 ICRA_SIZE_PM %>%
   filter (TAIL_BINS == 'Q80') %>%
   dunn_test(PER_DEAD ~ YEAR, p.adjust.method = "bonferroni")
 #each year + 2025 is sig (** - ****)
 

ICRA_SIZE_PM <- ICRA_SIZE_PM %>%
  mutate(YEAR_BIN = interaction(YEAR, TAIL_BINS, sep = "_"))

kw_results_combo <- ICRA_SIZE_PM %>%
  kruskal_test(PER_DEAD ~ YEAR_BIN)

dunn_results_combo <- ICRA_SIZE_PM %>%
  dunn_test(PER_DEAD ~ YEAR_BIN, p.adjust.method = "bonferroni")



###################################
#####ridgeplot of PM by year#######
###################################
ggplot(ICRA_SIZE_PM, aes(x = PER_DEAD, y = as.factor(YEAR), fill = as.factor(YEAR))) +
  geom_density_ridges(alpha = 0.8) +  # Ridge plot
  geom_point(data = mean_PM_per_year_south, aes(x = (mean_PM), y = as.factor(YEAR)), #can change to log(mean_PM)
             color = "black", size = 3, shape = 16) +  # Means are points
  #stat_density_ridges(quantile_lines = TRUE, quantiles = 3, alpha = 0.5, color= "black", linewidth = 0.5) +  # Quantiles
  geom_text(data = max_size, 
            aes(x = max_size + (max_size * 0.05),  
                y = as.factor(YEAR), 
                label = paste0("N=", N)),  
            hjust = 2, vjust = -1, size = 3, color = "black") +  
  labs(
    x = "Partial mortality (%)",
    y = "Year",
    fill="Year") +
  scale_fill_manual(values = custom_colors)+
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"))

ggplot2::ggsave ("plots/Partial_mortality_ridge.jpeg", width = 5, height = 5, units = 'in')

#########################################
#visualize log-transformed distributions#
#########################################

# **Exploring transformed density data by survey method**
# Log-transform ICRA densities (log(x + 1))
ICRA_SIZE_PM_log <- ICRA_SIZE_PM %>%
  mutate(Log_ICRA_PM = log1p(PER_DEAD))%>%  # log1p(x) is equivalent to log(x + 1)
  mutate(Sqrt_ICRA_PM = sqrt(PER_DEAD)) #sruare root transform as it handles zeros better

# Test if log-transformed data is normal
shapiro.test(ICRA_SIZE_PM_log$PER_DEAD)
shapiro.test(ICRA_SIZE_PM_log$Log_ICRA_PM)
shapiro.test(ICRA_SIZE_PM_log$Sqrt_ICRA_PM)
#none of the data are normal after transformation

#  plots
p1 <- ggplot(ICRA_SIZE_PM_log, aes(x = PER_DEAD)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Original Data", x = "ICRA_raw_PM")

p2 <- ggplot(ICRA_SIZE_PM_log, aes(x = Log_ICRA_PM)) +
  geom_density(fill = "red", alpha = 0.5) +
  labs(title = "Log-Transformed", x = "Log_ICRA_PM")

p3 <- ggplot(ICRA_SIZE_PM_log, aes(x = Sqrt_ICRA_PM)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(title = "Square Root Transformed", x = "Sqrt_ICRA_PM")

# qq plots
qq1 <- ggplot(ICRA_SIZE_PM_log, aes(sample = PER_DEAD)) +
  stat_qq() + stat_qq_line() + labs(title = "QQ Plot: Original Data")

qq2 <- ggplot(ICRA_SIZE_PM_log, aes(sample = Log_ICRA_PM)) +
  stat_qq() + stat_qq_line() + labs(title = "QQ Plot: Log-Transformed")

qq3 <- ggplot(ICRA_SIZE_PM_log, aes(sample = Sqrt_ICRA_PM)) +
  stat_qq() + stat_qq_line() + labs(title = "QQ Plot: Square Root Transformed")

# save these in a grid
(p1 | p2 | p3) / (qq1 | qq2 | qq3)
ggsave("plots/PM data transformation distributions.png")


###################################
###ridgeplot of size by year#######
###################################

#calculate proportions of each class per year (do not have density of PM corals in 2025, nor size info for feb 2025)
size_props <- ICRA_SIZE_PM %>%
  group_by(YEAR, TAIL_BINS) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(YEAR) %>%
  mutate(prop = n / sum(n)) %>%
  select(YEAR, TAIL_BINS, prop) %>%
  pivot_wider(names_from = TAIL_BINS, values_from = prop) %>%
  rename(prop_q20 = Q20, prop_qmed = QMED, prop_q80 = Q80)

q20_cutoff <- 12
q80_cutoff <- 40

ggplot(ICRA_SIZE_PM, aes(x = COLONYLENGTH, y = as.factor(YEAR), fill = as.factor(YEAR))) +
  geom_density_ridges(alpha = 0.8) +
  
  # Vertical lines for Q20 and Q80 cutoffs
  geom_vline(xintercept = q20_cutoff, linetype = "dashed", color = "firebrick", linewidth = 0.7) +
  geom_vline(xintercept = q80_cutoff, linetype = "dashed", color = "blue", linewidth = 0.7) +
  
  # Proportion labels above each ridge
  geom_text(data = size_props,
            aes(x = q20_cutoff - 5,  # slightly left of Q20 line
                y = as.factor(YEAR),
                label = paste0(round(prop_q20 * 100), "%")),
            color = "firebrick", size = 2, vjust = -6, inherit.aes = FALSE) +
  
  geom_text(data = size_props,
            aes(x = (q20_cutoff + q80_cutoff) / 2,  # center for QMED
                y = as.factor(YEAR),
                label = paste0(round(prop_qmed * 100), "%")),
            color = "black", size = 2, vjust = -6, inherit.aes = FALSE) +
  
  geom_text(data = size_props,
            aes(x = q80_cutoff + 5,  # slightly right of Q80 line
                y = as.factor(YEAR),
                label = paste0(round(prop_q80 * 100), "%")),
            color = "blue", size = 2, vjust = -6, inherit.aes = FALSE) +
  
  labs(x = "Coral size (cm)", y = "Year", fill="Year") +
  scale_fill_manual(values = custom_colors)+
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"))

ggplot2::ggsave ("plots/Colony_size_ridge.jpeg", width = 5, height = 5, units = 'in')


########################
#boxplot of PM by year##
########################


ggplot(summary_stats_year,
       aes(x = as.factor(YEAR), y = Mean_PM, fill = as.factor(YEAR))) +
  geom_col(alpha = 1) +  
  geom_errorbar(aes(
    ymin = pmax(0, CI_Lower),  
    ymax = CI_Upper
  ), width = 0.2) +theme_minimal() +
  theme(
    panel.border = element_rect(color = "grey", fill = NA, size = 1)
  ) +
  labs(
    x = "Survey Year",
    y = "Mean partial mortality (%)",
    fill="Year"
  ) +
  scale_fill_manual(values = custom_colors)+
  theme_minimal() +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(size = 8, angle = 45, hjust = 0.7),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.border = element_rect(color = "grey", fill = NA, size = 1)
  )
ggplot2::ggsave ("plots/Partial_mortaltiy_boxplot_by_year.jpeg", width = 5, height = 5, units = 'in')


######################################
#boxplot of PM by size class and year#
######################################

#correct order
summary_stats_size$TAIL_BINS <- factor(summary_stats_size$TAIL_BINS, 
                                  levels = c("Q20", "QMED", "Q80"))

ggplot(summary_stats_size %>% filter(!is.na(TAIL_BINS)), 
       aes(x = as.factor(YEAR), y = Mean_PM, fill = as.factor(YEAR))) +
  geom_col(alpha = 1) +  
  geom_errorbar(aes(
    ymin = pmax(0, CI_Lower),  
    ymax = CI_Upper
  ), width = 0.2) +theme_minimal() +
  theme(
    panel.border = element_rect(color = "grey", fill = NA, size = 1)
  ) +
  labs(
    x = "Survey Year",
    y = "Mean percent partial mortality (%)",
    fill="Year"
  ) +
  facet_wrap(~TAIL_BINS, labeller = labeller(TAIL_BINS = facet_labels)) +  
  scale_fill_manual(values = custom_colors)+
  theme_minimal() +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(size = 8, angle = 45, hjust = 0.7),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.border = element_rect(color = "grey", fill = NA, size = 1)
  )
ggplot2::ggsave ("plots/Partial_mortaltiy_boxplot_by_size_class.jpeg", width = 5, height = 5, units = 'in')

###############################
#plot histograms of PM by year#
###############################
ICRA_SIZE_PM %>%
  filter(YEAR %in% c(2015, 2023, 2018, 2025)) %>%
  ggplot(aes(x = PER_DEAD, fill = factor(YEAR))) +
  geom_histogram(bins = 20, alpha = 0.6, position = "identity") +
  facet_wrap(~ YEAR, scales = "free_y") +
  theme_minimal() +
  labs(
    x = "Partial Mortality (%)",
    y = "Frequency",
    title = "Distribution of Partial Mortality (2015 vs 2023)",
    fill="Year"
  ) +
  scale_fill_manual(values = custom_colors)+
  theme(
    axis.text.x = element_text(size = 8,  angle = 45, hjust = 0.7),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )


##################################
#Plot dot + CI of PM across years#
##################################
ggplot(summary_stats, aes(x = factor(YEAR), y = Mean_PM, color = factor(YEAR))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Mean Partial Mortality (%)",
    color= "Year"
  ) +
  scale_color_manual(values = custom_colors)+
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 0.7),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(
    limits = c(0, 35),  # Set y-axis to start from 0
    breaks = seq(0, 30, by = 10)  # Adjust the break interval
  )
ggplot2::ggsave ("plots/Partial_mortaltiy_dotplot_CI.jpeg", width = 5, height = 5, units = 'in')


#dot plot by size class
ggplot(summary_stats_size %>% filter(!is.na(TAIL_BINS)), 
       aes(x = as.factor(YEAR), y = Mean_PM, fill = as.factor(YEAR))) +
  geom_point(shape = 21, size = 3, color = "black")+
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = as.factor(YEAR)), width = 0.2 ) +
  theme(
    panel.border = element_rect(color = "grey", fill = NA, size = 1) # Adds black border around each facet
  ) +
  labs(
    x = "Survey Year",
    y = "Mean percent partial mortality (%)"
  ) +
  facet_wrap(~TAIL_BINS, labeller = labeller(TAIL_BINS = facet_labels)) +  
  scale_fill_manual(values = custom_colors)+
  scale_color_viridis_d(option = "C") +
  theme_minimal() +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    panel.border = element_rect(color = "grey", fill = NA, size = 1)
    # Adds black border around each facet
  )
ggplot2::ggsave ("plots/Partial_mortaltiy_by_size_dotplot_CI.jpeg", width = 5, height = 5, units = 'in')

#####################################################
#Bootstrap large colonies in 2025 to match prev years#
#####################################################

#make sure no 0 or 100s
ICRA_SIZE_PM<- ICRA_SIZE_PM%>%
  mutate(PER_DEAD = (PER_DEAD *(nrow(.)-1)+0.25)/nrow(.))

#convert percent to prop
ICRA_SIZE_PM <- ICRA_SIZE_PM %>%
  mutate(PM = PER_DEAD / 100)

#first filter out large colonies from 2025
large_colonies <- ICRA_SIZE_PM %>%
  filter(TAIL_BINS == "Q80")
large_2025 <- large_colonies %>% filter(YEAR == 2025)
large_prior <- large_colonies %>% filter(YEAR < 2025)

ICRA_SIZE_PM <- ICRA_SIZE_PM %>%
  mutate(YEAR = as.factor(YEAR),
         SITE = as.factor(SITE),
         TAIL_BINS = as.factor(TAIL_BINS))

#not including TAIL_BINS as covariate b/c only modeling large here

safe_fit <- function(data) {
  tryCatch(
    {
      data <- data %>%
        mutate(YEAR = as.factor(YEAR),
               SITE = as.factor(SITE)) %>%
        filter(!is.na(PM) & PM > 0 & PM < 1)
      
      mod <- glmmTMB(PM ~ YEAR + (1 | SITE),
                     family = beta_family(link = "logit"),
                     data = data)
      tidy(mod, effects = "fixed")
    },
    error = function(e) {
      message("Model failed: ", e$message)
      NULL
    }
  )
}




#bootstrap
set.seed(123)  
nboot <- 1000
failures <- 0

boot_results <- map_dfr(1:nboot, function(i) {
  boot_2025 <- sample_n(large_2025, 35)
  
  boot_data <- bind_rows(large_prior, boot_2025)
  
  fit <- safe_fit(boot_data)
  
  if (is.null(fit)) {
    failures <<- failures + 1
    return(tibble())
  } else {
    fit %>% mutate(iteration = i)
  }
})

cat("Total model failures:", failures, "\n")

#view results
head(boot_results) #2025 sig

 boot_results %>%
      filter(term == "YEAR2025") %>%
       summarize(
             mean_estimate = mean(estimate),
             sd_estimate = sd(estimate),
            lower_CI = quantile(estimate, 0.025),
             upper_CI = quantile(estimate, 0.975)
         )
#mean 1.72 (this is the effect size .basically 2025 on the logit scale, compared to 2015. meaning it more than doubled) . 
 #CIs not 0 so significant!
 
 
 mod <- glmmTMB(PM ~ YEAR + (1 | SITE), family = beta_family(), data = ICRA_SIZE_PM)
 emmeans(mod, ~ YEAR, type = "response")
 
 mod2 <- glmmTMB(PM ~ YEAR * TAIL_BINS + (1 | SITE),
   family = beta_family(link = "logit"),
   data = ICRA_SIZE_PM
 )

 

 # Get the estimated marginal means
 pm_emmeans <- emmeans(mod2, ~ YEAR * TAIL_BINS, type = "response") %>%
   as.data.frame() %>%
   rename(Mean_PM = response,
          CI_Lower = asymp.LCL,
          CI_Upper = asymp.UCL)
 
 # Optional: rename size bins for clarity
 pm_emmeans <- pm_emmeans %>%
   mutate(
     TAIL_BINS = factor(TAIL_BINS, levels = c("Q20", "QMED", "Q80"),
                        labels = c("Small", "Medium", "Large")),
     YEAR = as.factor(YEAR)
   )
 
 ggplot(pm_emmeans,
        aes(x = YEAR, y = Mean_PM, fill = YEAR)) +
   geom_col(alpha = 1) +  
   geom_errorbar(aes(
     ymin = pmax(0, CI_Lower),  
     ymax = CI_Upper
   ), width = 0.2) +
   facet_wrap(~TAIL_BINS, nrow = 1) +
   theme_minimal() +
   labs(
     x = "Survey Year",
     y = "Estimated partial mortality (%)",
     fill = "Year"
   ) +
   scale_fill_manual(values = custom_colors) +
   theme(
     legend.position = "none", 
     axis.text.x = element_text(size = 8, angle = 45, hjust = 0.7),
     axis.text.y = element_text(size = 10),
     axis.title = element_text(size = 12),
     panel.border = element_rect(color = "grey", fill = NA, size = 1),
     strip.text = element_text(size = 12)
   )
 
 ggsave("plots/estimated_M_bootstrapped_barplots_52725.png")
 
 # Pairwise comparisons
 pairwise_contrasts <- emmeans(mod2, ~ YEAR * TAIL_BINS, type = "response") %>%
   contrast(method = "pairwise") %>%
   summary(infer = TRUE)
 
 # Compact Letter Display (groupings)
 cld_results <- cld(emmeans(mod2, ~ YEAR * TAIL_BINS, type = "response"),
                    Letters = letters,
                    adjust = "tukey") %>%
   as.data.frame() %>%
   mutate(
     TAIL_BINS = factor(TAIL_BINS, levels = c("Q20", "QMED", "Q80"),
                        labels = c("Small", "Medium", "Large"))
   )
 
 # Join CLD to your plot data
 pm_emmeans <- left_join(pm_emmeans, cld_results[, c("YEAR", "TAIL_BINS", ".group")], 
                         by = c("YEAR", "TAIL_BINS"))
 
 ##########
 #plot with sig labels
 ggplot(pm_emmeans, aes(x = YEAR, y = Mean_PM, fill = YEAR)) +
   geom_col(alpha = 1) +
   geom_errorbar(aes(
     ymin = pmax(0, CI_Lower),
     ymax = CI_Upper
   ), width = 0.2) +
   geom_text(aes(label = .group, y = CI_Upper + 0.02), size = 4) +  # adjust height as needed
   facet_wrap(~TAIL_BINS, nrow = 1) +
   theme_minimal() +
   labs(
     x = "Survey Year",
     y = "Estimated partial mortality (%)",
     fill = "Year"
   ) +
   scale_fill_manual(values = custom_colors) +
   theme(
     legend.position = "none",
     axis.text.x = element_text(size = 8, angle = 45, hjust = 0.7),
     axis.text.y = element_text(size = 10),
     axis.title = element_text(size = 12),
     panel.border = element_rect(color = "grey", fill = NA, size = 1),
     strip.text = element_text(size = 12)
   )
 (ggsave("plots/sig_labels_PM_model_barplot.png"))
 
 
 # Back-transform logit to probability
 logit_to_prob <- function(x) exp(x) / (1 + exp(x))
 
 logit_to_prob(1.75)      # mean 0.85
 logit_to_prob(1.17)      # lower CI 0.76
 logit_to_prob(2.18)      # upper CI 0.9
 
 boot_results %>%
   filter(term == "YEAR2025") %>%
   ggplot(aes(x = estimate)) +
   geom_histogram(fill = "#2C7BB6", bins = 30, color = "white") +
   geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
   labs(title = "Bootstrapped Estimates for YEAR2025 (Large Colonies)",
        x = "Logit Effect Size", y = "Count")
 

 #now to compare with other size classes, fit each
 
 small_data<- ICRA_SIZE_PM%>%
   filter(TAIL_BINS == "Q20")
 
med_data<- ICRA_SIZE_PM%>%
   filter(TAIL_BINS == "QMED")
   
 # Small colonies model ==
 mod_small <- glmmTMB(PM ~ YEAR + (1 | SITE), family = beta_family(link="logit"), data = small_data)
 summary(mod_small)
 
 # Medium colonies model
 mod_med <- glmmTMB(PM ~ YEAR + (1 | SITE), family = beta_family(link="logit"), data = med_data)
 summary(mod_med)
 
 
 summary_small <- tidy(mod_small, effects = "fixed") %>%
   filter(term != "(Intercept)") %>%
   mutate(SIZE_CLASS = "Small")
 
 summary_med <- tidy(mod_med, effects = "fixed") %>%
   filter(term != "(Intercept)") %>%
   mutate(SIZE_CLASS = "Medium")
 
 summary_large <- boot_results %>%
   filter(term != "(Intercept)") %>%
   mutate(SIZE_CLASS = "Large")
 
 summary_all <- bind_rows(summary_small, summary_med, summary_large)
 
 
 #small emmeans
 emmeans_small <- emmeans(mod_small, ~ YEAR, type = "response")
 df_small <- as.data.frame(emmeans_small) %>%
   mutate(SIZE_CLASS = "Small")
 
 # Medium colonies emmeans
 emmeans_med <- emmeans(mod_med, ~ YEAR, type = "response")
 df_med <- as.data.frame(emmeans_med) %>%
   mutate(SIZE_CLASS = "Medium")
 
 # Convert emmeans to dataframe
 df_small <- as.data.frame(emmeans_small) %>%
   mutate(SIZE_CLASS = "Small")
 df_med <- as.data.frame(emmeans_med) %>%
   mutate(SIZE_CLASS = "Medium")
 
 #use bootstrap means and CIs for the large, merge w these. first back transofrm to get proportion again
 #filter by year
 boot_years <- boot_results %>%
   filter(grepl("YEAR", term)) %>%
   # Extract year as factor for clarity
   mutate(YEAR = gsub("YEAR", "", term))
 
 #back-transform from logit to proportion
 inv_logit <- function(x) exp(x) / (1 + exp(x))
 boot_summary <- boot_years %>%
   group_by(YEAR) %>%
   summarize(
     mean_estimate_link = mean(estimate),
     lower_CI_link = quantile(estimate, 0.025),
     upper_CI_link = quantile(estimate, 0.975)
   ) %>%
   mutate(
     emmean = inv_logit(mean_estimate_link),
     lower.CL = inv_logit(lower_CI_link),
     upper.CL = inv_logit(upper_CI_link),
     SIZE_CLASS = "Large"
   ) %>%
   select(YEAR, emmean, lower.CL, upper.CL, SIZE_CLASS)
 
 df_large <- boot_summary %>% 
   mutate(SIZE_CLASS = "Large")
 
 
 summary_all <- bind_rows(
   df_small %>% rename(emmean = response, lower.CL = asymp.LCL, upper.CL = asymp.UCL),
   df_med %>% rename(emmean = response, lower.CL = asymp.LCL, upper.CL = asymp.UCL),
   df_large %>% rename(emmean = mean_estimate, lower.CL = lower_CI, upper.CL = upper_CI)
 )
 
 ggplot(summary_all, aes(x = YEAR, y = emmean, fill = SIZE_CLASS)) +
   geom_col(position = position_dodge(width = 0.8), color = "black", alpha = 0.8) +
   geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2,
                 position = position_dodge(width = 0.8)) +
   labs(x = "Year", y = "Mean Partial Mortality (PM)", fill = "Size Class") +
   theme_minimal() +
   scale_fill_manual(values = c("Small" = "skyblue", "Medium" = "orange", "Large" = "red")) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 # Small colonies contrasts
 contrast(emmeans_small, method = "pairwise", adjust = "tukey") %>% summary()
 
 # Medium colonies contrasts
 contrast(emmeans_med, method = "pairwise", adjust = "tukey") %>% summary()
 
 
 ####################################
 #run model without bootstrapping, 
 
 mod_full <- glmmTMB(
   PM ~ YEAR * TAIL_BINS + (1 | SITE),
   family = beta_family(link = "logit"),
   data = ICRA_SIZE_PM %>% filter(PM > 0 & PM < 1)
 )

 summary(mod_full) 
 
 emmeans(mod_full, ~ YEAR * TAIL_BINS, type = "response")
 
 #break up emmeans by bin 
 emmeans_q80 <- emmeans(mod_full, ~ YEAR | TAIL_BINS, at = list(TAIL_BINS = "Q80"), type = "response")
 pairs(emmeans_q80, adjust = "bonferroni")
 
 emmeans_qmed <- emmeans(mod_full, ~ YEAR | TAIL_BINS, at = list(TAIL_BINS = "QMED"), type = "response")
 pairs(emmeans_qmed, adjust = "bonferroni")
 
 emmeans_q20 <- emmeans(mod_full, ~ YEAR | TAIL_BINS, at = list(TAIL_BINS = "Q20"), type = "response")
 pairs(emmeans_q20, adjust = "bonferroni")
 
 
 