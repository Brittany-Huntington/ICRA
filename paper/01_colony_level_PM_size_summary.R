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
library(tidyverse)
library(ggpubr)
library(rstatix)

load(file ="data/ICRA_PM_SIZE_USE.Rdata")

#subset 2025 (can later add this to clean-colony level script)
s<-south_ICRA_survey_data %>%
  filter( YEAR == '2025')%>%
  group_by(SITE)%>%
  summarise(meanpm = mean(PER_DEAD))

#data summary
mean_size_per_year_south <- s %>%
  group_by(YEAR) %>%
  summarise(mean_size = mean(COLONYLENGTH, na.rm = TRUE),
            sd_size = sd(COLONYLENGTH, na.rm = TRUE),
            n = sum(!is.na(COLONYLENGTH)),
            se = sd_size / sqrt(n),
            lower_CI = mean_size - qt(0.975, df = n - 1) * se,
            upper_CI = mean_size + qt(0.975, df = n - 1) * se,
            .groups = "drop")


# mean PM per year
mean_PM_per_year_south <- s %>%
  group_by(YEAR) %>%
  summarise(mean_PM = mean(PER_DEAD, na.rm = TRUE),
            sd_PM = sd(PER_DEAD, na.rm = TRUE),
            n = sum(!is.na(PER_DEAD)),
            se = sd_PM / sqrt(n),
            lower_CI = mean_PM - qt(0.975, df = n - 1) * se,
            upper_CI = mean_PM + qt(0.975, df = n - 1) * se,
            .groups = "drop")

# mean PM per year by size class
mean_PM_per_year_all <- s %>%
  group_by(YEAR, TAIL_BINS) %>%
  summarise(mean_PM = mean(PER_DEAD, na.rm = TRUE),
            sd_PM = sd(PER_DEAD, na.rm = TRUE),
            n = sum(!is.na(PER_DEAD)),
            se = sd_PM / sqrt(n),
            lower_CI = mean_PM - qt(0.975, df = n - 1) * se,
            upper_CI = mean_PM + qt(0.975, df = n - 1) * se,
            .groups = "drop")

# calc the max size (for plotting later)
max_size <- s %>%
  group_by(YEAR) %>%
  summarise(
    max_size = max(COLONYLENGTH, na.rm = TRUE),
    N = n())

#######
#stats#
#######

#test normality
shapiro.test(s$PER_DEAD)

#KS test: PM by year
kw_results <- s %>%
  kruskal_test(PER_DEAD ~ YEAR)

#KS test: PM by year and size class
kw_results_bin <- s %>%
  kruskal_test(PER_DEAD ~ TAIL_BINS)

#Dunn's: PM by year
dunn_results <- s %>%
  dunn_test(PER_DEAD ~ YEAR, p.adjust.method = "bonferroni")

#Dunn's: PM by year and size class
dunn_results_bin <- s %>%
  dunn_test(PER_DEAD ~ TAIL_BINS, p.adjust.method = "bonferroni")

#KS and Dunn's between each year within each size class
s %>%
  filter (TAIL_BINS == 'Q20') %>%
  kruskal_test(PER_DEAD ~ YEAR)

s %>%
  filter (TAIL_BINS == 'Q20') %>%
  dunn_test(PER_DEAD ~ YEAR, p.adjust.method = "bonferroni")

s %>%
  filter (TAIL_BINS == 'QMED') %>%
  kruskal_test(PER_DEAD ~ YEAR)

s %>%
  filter (TAIL_BINS == 'QMED') %>%
  dunn_test(PER_DEAD ~ YEAR, p.adjust.method = "bonferroni")

s %>%
  filter (TAIL_BINS == 'Q80') %>%
  kruskal_test(PER_DEAD ~ YEAR)

s %>%
  filter (TAIL_BINS == 'Q80') %>%
  dunn_test(PER_DEAD ~ YEAR, p.adjust.method = "bonferroni")


#KS and Dunn's within year by size class
s %>%
  filter (YEAR == '2015') %>%
  kruskal_test(PER_DEAD ~ TAIL_BINS)

s %>%
  filter (YEAR == '2015') %>%
  dunn_test(PER_DEAD ~ TAIL_BINS, p.adjust.method = "bonferroni")

s %>%
  filter (YEAR == '2018') %>%
  kruskal_test(PER_DEAD ~ TAIL_BINS)

s %>%
  filter (YEAR == '2018') %>%
  dunn_test(PER_DEAD ~ TAIL_BINS, p.adjust.method = "bonferroni")

s %>%
  filter (YEAR == '2023') %>%
  kruskal_test(PER_DEAD ~ TAIL_BINS)

s %>%
  filter (YEAR == '2023') %>%
  dunn_test(PER_DEAD ~ TAIL_BINS, p.adjust.method = "bonferroni")

s %>%
  filter (YEAR == '2025') %>%
  kruskal_test(PER_DEAD ~ TAIL_BINS)

s %>%
  filter (YEAR == '2025') %>%
  dunn_test(PER_DEAD ~ TAIL_BINS, p.adjust.method = "bonferroni")

# set colors
vir_colors <- viridis(n = 4, option = "C")
print(vir_colors)
custom_colors <- vir_colors
custom_colors[4] <- "gold"  # DAA520 goldenrod 

#prepare facet labels for plotting
facet_labels <- c(
  "Q20" = "Small (5-12 cm)",
  "QMED" = "Medium (13-39 cm)",
  "Q80" = "Large (>40 cm)"
)

###################################
#####ridgeplot of PM by year#######
###################################
ggplot(s, aes(x = PER_DEAD, y = as.factor(YEAR), fill = as.factor(YEAR))) +
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

#ggplot2::ggsave ("plots/Partial_mortality_ridge.jpeg", width = 5, height = 5, units = 'in')

######################################
#barplot of PM by size class and year#
######################################

#correct order
summary_stats_size$TAIL_BINS <- factor(mean_PM_per_year_all$TAIL_BINS, 
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
#ggplot2::ggsave ("plots/Partial_mortaltiy_boxplot_by_size_class.jpeg", width = 5, height = 5, units = 'in')


