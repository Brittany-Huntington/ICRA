
rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(purrr)
library(sf)
library(viridis)
library(cowplot)
library("svglite")

# set colors
vir_colors <- viridis(n = 4, option = "C")
print(vir_colors)

custom_colors <- vir_colors
custom_colors[4] <- "gold"  # DAA520 goldenrod 

#load PM size data
load("data/filtered_colony_data.RData")
load("data/filtered_south_colony_data.RData")
load("data/ICRA_PM_site2025.RData")

s_check<-read.csv("merged2025_PM_S_site.csv")
load("data/ICRA_PM_SIZE_USE.Rdata")


tutuila_shape <- st_read("data/Tut_shapefiles/TUT.shp") #make sure folder has all shapefiles needed

#plot PM, density across island

# mean PM per year/site of south only
mean_PM_per_year_site_south <- filtered_south_colony_data %>%
  group_by(YEAR, SITE, LATITUDE, LONGITUDE) %>%
  summarise(mean_PM = mean(PER_DEAD, na.rm = TRUE))

# mean PM per year/site of all sites (using the size-cutoff data)
mean_PM_per_year_site <- filtered_colony_data %>%
  group_by(YEAR, SITE, LATITUDE, LONGITUDE) %>%
  summarise(mean_PM = mean(PER_DEAD, na.rm = TRUE))

# mean PM per year/site of all sites unfiltered by size cutoff
ICRA_PM_site_2025

#assign coordinates for PM data. Want to keep PM=0 but not NA
sites_sf_PM_south <- mean_PM_per_year_site_south %>%
  #filter(!is.na(mean_PM)) %>%
  distinct(LATITUDE, LONGITUDE, YEAR, .keep_all = TRUE) %>%  # keep all columns
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)  # WGS84 

sites_sf_PM <- mean_PM_per_year_site %>%
  #filter(!is.na(mean_PM)) %>%
  distinct(LATITUDE, LONGITUDE, YEAR, .keep_all = TRUE) %>%  # keep all columns
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)  # WGS84 

sites_sf_PM_2025 <- s_check %>%
  #filter(!is.na(mean_PM)) %>%
  distinct(LATITUDE, LONGITUDE, .keep_all = TRUE) %>%  # keep all columns
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)  # WGS84 


#plot PM
sites_sf_PM_south <- sites_sf_PM_south %>%
  mutate(shape_code = ifelse(is.na(mean_PM), "NA", as.character(YEAR)))
sites_sf_PM <- sites_sf_PM %>%
  mutate(shape_code = ifelse(is.na(mean_PM), "NA", as.character(YEAR)))
sites_sf_PM2025 <- sites_sf_PM_2025
  

ggplot() +
  geom_sf(data = tutuila_shape, fill = "gray90", color = "black") +
  geom_sf(data = sites_sf_PM_2025, aes(color = mean_PM), 
          size = 2, alpha = 0.7) +
  geom_text(data = sites_sf_PM_2025,
            aes(x = st_coordinates(geometry)[,1],
                y = st_coordinates(geometry)[,2],
                label = SITE),
            size = 2, hjust = -0.2, vjust = 0) +
 
  scale_color_viridis(option = "C") +
  labs(
    x = "Longitude", y = "Latitude",
    color = "Mean % PM"
  ) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 5, angle = 90, hjust = 0),
    axis.text.y = element_text(size = 5),
    axis.title = element_text(size = 8),
    text = element_text(size = 6),
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",
    legend.key.size = unit(0.2, "cm"),
    legend.box = "horizontal",  
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 5)
  )

ggplot2::ggsave ("plots/Map_Partial_mortaltiy_2025.jpeg", width = 10, height = 5, units = 'in')


p<-ggplot() +
  geom_sf(data = tutuila_shape, fill = "gray90", color = "black") +
  geom_sf(data = sites_sf_PM_south, aes(color = mean_PM, shape = shape_code), 
          size = 2, alpha = 0.7) +
  scale_color_viridis(option = "C") +
  scale_shape_manual(
    values = c("2015" = 16, "2018" = 17, "2023" = 15, "2025" = 18, "NA" = 4)
  )+
  labs(
    x = "Longitude", y = "Latitude",
    color = "Mean % PM",
    shape = "Year"
  ) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 5, angle = 90, hjust = 0),
    axis.text.y = element_text(size = 5),
    axis.title = element_text(size = 8),
    text = element_text(size = 6),
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",  # options: "bottom", "top", etc.
    #legend.direction = "horizontal",
    legend.key.size = unit(0.2, "cm"),
    legend.box = "horizontal",  # places multiple legends side by side
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 5)
  )

ggplot2::ggsave ("plots/Map_south_Partial_mortaltiy_by_year_map.jpeg", width = 5, height = 2.5, units = 'in')

##########
#Density##
##########

# mean density per year/site
mean_den_per_year_site_all <- ALL_COLONY_DENSITY %>%
  group_by(YEAR, SITE, LATITUDE, LONGITUDE) %>%
  summarise(mean_den = mean(DENSITY, na.rm = TRUE))

#assign coordinates for density data. Want to remove NA 
sites_sf_den_all <- mean_den_per_year_site_all %>%
  filter(!is.na(mean_den)) %>%
  distinct(LATITUDE, LONGITUDE, YEAR, .keep_all = TRUE) %>%  # keep all columns
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)  # WGS84 

sites_sf_den_all$log_mean_den <- log10(sites_sf_den_all$mean_den + 0.01)

#plot density
sites_sf_den_all <- sites_sf_den %>%
  mutate(shape_code = ifelse(mean_den == 0, "Zero", as.character(YEAR)))

#plot south no feb density heatmap map
ggplot() +
  geom_sf(data = tutuila_shape, fill = "gray90", color = "black") +
  geom_sf(data = sites_sf_den_all, aes(color = log_mean_den, shape = shape_code), 
          size = 2, alpha = 0.7) +
  scale_color_viridis(option = "C") +
  #scale_fill_manual(values = custom_colors) +
  scale_shape_manual(
    values = c("2015" = 16, "2018" = 17, "2023" = 15, "2025" = 18, "Zero" = 4)
  )+
  labs(
    x = "Longitude", y = "Latitude",
    color = "Log Mean density\n(colonies m²)",
    shape = "Year"
  ) +
    theme(
      axis.text.x = element_text(size = 5, angle = 90, hjust = 0),
      axis.text.y = element_text(size = 5),
      axis.title = element_text(size = 8),
      text = element_text(size = 6),
      plot.title = element_text(hjust = 0.5),
      legend.position = "top",  # options: "bottom", "top", etc.
      #legend.direction = "horizontal",
      legend.key.size = unit(0.4, "cm"),
      legend.box = "horizontal",  # places multiple legends side by side
      legend.text = element_text(size = 5),
      legend.title = element_text(size = 5)
    )
ggplot2::ggsave ("plots/Density_by_year_map.jpeg", width = 5, height = 2.5, units = 'in')

#######################
#For south sites only

# mean density per year/site
mean_den_per_year_site_south <- SOUTH_COLONY_DENSITY_filtered %>%
  group_by(YEAR, SITE, month, LATITUDE.x, LONGITUDE.x) %>%
  summarise(mean_den = mean(adjusted_density, na.rm = TRUE))

#assign coordinates for density data. Want to remove NA 
sites_sf_den_south <- mean_den_per_year_site_south %>%
  filter(!is.na(mean_den)) %>%
  distinct(LATITUDE.x, LONGITUDE.x, YEAR, .keep_all = TRUE) %>%  # keep all columns
  st_as_sf(coords = c("LONGITUDE.x", "LATITUDE.x"), crs = 4326)  # WGS84 

sites_sf_den_south$log_mean_den <- log10(sites_sf_den_south$mean_den + 0.01)

#plot density
sites_sf_den_south <- sites_sf_den_south %>%
  mutate(shape_code = ifelse(mean_den == 0, "Zero", as.character(YEAR)))

ggplot() +
  geom_sf(data = tutuila_shape, fill = "gray90", color = "black") +
  geom_sf(data = sites_sf_den_south, aes(color = log_mean_den, shape = shape_code), 
          size = 2, alpha = 0.7) +
  scale_color_viridis(option = "C") +
  #scale_fill_manual(values = custom_colors) +
  scale_shape_manual(
    values = c("2015" = 16, "2018" = 17, "2023" = 15, "2025" = 18, "Zero" = 4)
  )+
  labs(
    x = "Longitude", y = "Latitude",
    color = "Log Mean density\n(colonies m²)",
    shape = "Year"
  ) +
  theme(
    axis.text.x = element_text(size = 5, angle = 90, hjust = 0),
    axis.text.y = element_text(size = 5),
    axis.title = element_text(size = 8),
    text = element_text(size = 6),
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",  # options: "bottom", "top", etc.
    #legend.direction = "horizontal",
    legend.key.size = unit(0.4, "cm"),
    legend.box = "horizontal",  # places multiple legends side by side
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 5)
  )
ggplot2::ggsave ("plots/Density_by_year_map_south_nofeb.jpeg", width = 5, height = 2.5, units = 'in')

#plot sites w/o feb
ggplot() +
  geom_sf(data = tutuila_shape, fill = "grey", color = "black") +
  geom_sf(data = sites_sf_den_south, 
          aes(shape = as.factor(YEAR), fill = as.factor(YEAR)),
          size = 1.5, stroke = 0.5, alpha = 0.7, color = "black") +
  scale_shape_manual(values = c("2015" = 22, "2018" = 23, "2023" = 24, "2025" = 21)) +
  #scale_fill_manual(values = custom_colors) +
  scale_fill_viridis_d(option = "C") +
  labs(
    x = "Longitude", y = "Latitude",
    color = "Log Mean density\n(colonies m²)",
    shape = "Year"
  ) +
  theme(
    axis.text.x = element_text(size = 5, angle = 90, hjust = 0),
    axis.text.y = element_text(size = 5),
    axis.title = element_text(size = 8),
    text = element_text(size = 6),
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",  # options: "bottom", "top", etc.
    #legend.direction = "horizontal",
    legend.key.size = unit(0.4, "cm"),
    legend.box = "horizontal",  # places multiple legends side by side
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 5)
  )
ggplot2::ggsave ("plots/Density_by_year_map_south.jpeg", width = 5, height = 2.5, units = 'in')


#south density inly including feb wihtout xs for 0s
mean_den_per_year_site_south_feb <- south_den1 %>%
  group_by(YEAR, SITE, month, LATITUDE.x, LONGITUDE.x) %>%
  summarise(mean_den = mean(DENSITY, na.rm = TRUE))

#assign coordinates for density data. Want to remove NA 
sites_sf_den_south_feb <- mean_den_per_year_site_south_feb %>%
  filter(!is.na(mean_den)) %>%
  distinct(LATITUDE.x, LONGITUDE.x, YEAR, .keep_all = TRUE) %>%  # keep all columns
  st_as_sf(coords = c("LONGITUDE.x", "LATITUDE.x"), crs = 4326)  # WGS84 

sites_sf_den_south_feb$log_mean_den <- log10(sites_sf_den_south_feb$mean_den + 0.01)

sites_sf_den_south_feb <- sites_sf_den_south_feb %>%
  mutate(shape_code = ifelse(mean_den == 0, "Zero", as.character(YEAR)))

#plot sites w feb
ggplot() +
  geom_sf(data = tutuila_shape, fill = "grey", color = "black") +
  geom_sf(data = sites_sf_den_south_feb, 
          aes(shape = as.factor(YEAR), fill = as.factor(YEAR)),
          size = 1.5, stroke = 0.5, alpha = 0.7, color = "black") +
  scale_shape_manual(values = c("2015" = 22, "2018" = 23, "2023" = 24, "2025" = 21)) +
  #scale_fill_manual(values = custom_colors) +
  scale_fill_viridis_d(option = "C") +
  labs(
    x = "Longitude", y = "Latitude",
    color = "Log Mean density\n(colonies m²)",
    shape = "Year"
  ) +
  theme(
    axis.text.x = element_text(size = 5, angle = 90, hjust = 0),
    axis.text.y = element_text(size = 5),
    axis.title = element_text(size = 8),
    text = element_text(size = 6),
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",  # options: "bottom", "top", etc.
    #legend.direction = "horizontal",
    legend.key.size = unit(0.4, "cm"),
    legend.box = "horizontal",  # places multiple legends side by side
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 5)
  )
ggplot2::ggsave ("plots/Density_by_year_map_south_wfeb.jpeg", width = 5, height = 2.5, units = 'in')

#plot 2025 sample sites by month
sites_sf_den_south_month <- sites_sf_den_south %>%
  filter(YEAR == 2025)

sites_sf_den_south_month$month<- as.factor(sites_sf_den_south_month$month)

ggplot() +
  geom_sf(data = tutuila_shape, fill = "grey", color = "black") +
  geom_sf(data = sites_sf_den_south_month, 
          aes(shape = month, fill = month,
          size = 1.5, stroke = 0.5, alpha=0.9, color = "black")) +
  scale_shape_manual(values = c("2" = 22, "3" = 23)) +
  #scale_fill_manual(values = custom_colors) +
  labs(
    x = "Longitude.x", y = "Latitude.x"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 5, angle = 90, hjust = 0),
    axis.text.y = element_text(size = 5),
    axis.title = element_text(size = 8),
    text = element_text(size = 6),
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",
    legend.key.size = unit(0.4, "cm"),
    legend.box = "horizontal",
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 5)
  )

ggplot2::ggsave ("plots/south_Site_map_by_month2025.png", width = 5, height = 2.5, units = 'in',  bg = "transparent")


#plot sample sites of south filtered
ggplot() +
  geom_sf(data = tutuila_shape, fill = "grey", color = "black") +
  geom_sf(data = sites_sf_den_south, 
          aes(shape = as.factor(YEAR), fill = as.factor(YEAR)),
          size = 1.5, stroke = 0.5, alpha=0.9, color = "black") +
  scale_shape_manual(values = c("2015" = 22, "2018" = 23, "2023" = 24, "2025" = 21)) +
  scale_fill_manual(values = custom_colors) +
  labs(
    x = "Longitude", y = "Latitude",
    fill = "Year",
    shape = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 5, angle = 90, hjust = 0),
    axis.text.y = element_text(size = 5),
    axis.title = element_text(size = 8),
    text = element_text(size = 6),
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",
    legend.key.size = unit(0.4, "cm"),
    legend.box = "horizontal",
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 5)
  )

ggplot2::ggsave ("plots/south_Site_map_by_year1.png", width = 5, height = 2.5, units = 'in',  bg = "transparent")


#way to plot so that 2025 data is not on top
# Make sure YEAR is a factor with the correct level order
sites_sf_den_south$YEAR <- factor(sites_sf_den_south$YEAR, levels = c("2025", "2018", "2023", "2015"))

ggplot() +
  geom_sf(data = tutuila_shape, fill = "grey", color = "black") +
  
  # First draw all years except 2025 (background layers)
  geom_sf(data = subset(sites_sf_den_south, YEAR != "2025"),
          aes(fill = YEAR, shape = YEAR),
          size = 1.5, stroke = 0.5, alpha = 0.8, color = "black") +
  
  # Then draw 2025 on top (foreground layer)
  geom_sf(data = subset(sites_sf_den_south, YEAR == "2025"),
          aes(fill = YEAR, shape = YEAR),
          size = 2, stroke = 0.5, alpha = 0.9, color = "black") +
  
  scale_shape_manual(values = c("2015" = 22, "2018" = 23, "2023" = 24, "2025" = 21)) +
  scale_fill_manual(
    values = c("2015" = custom_colors[1],
               "2018" = custom_colors[2],
               "2023" = custom_colors[3],
               "2025" = custom_colors[4])
  ) +
  labs(
    x = "Longitude", y = "Latitude",
    fill = "Year",
    shape = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 5, angle = 90, hjust = 0),
    axis.text.y = element_text(size = 5),
    axis.title = element_text(size = 8),
    text = element_text(size = 6),
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",
    legend.key.size = unit(0.4, "cm"),
    legend.box = "horizontal",
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 5)
  )

ggplot2::ggsave ("plots/south_Site_map_by_year3.png", width = 5, height = 2.5, units = 'in',  bg = "transparent")



#way to plot so that 2025 data is  on top
# Make sure YEAR is a factor with the correct level order
sites_sf_den_south_feb$YEAR <- factor(sites_sf_den_south_feb$YEAR, levels = c("2025", "2018", "2023", "2015"))

ggplot() +
  geom_sf(data = tutuila_shape, fill = "grey", color = "black") +
  
  # First draw all years except 2025 (background layers)
  geom_sf(data = subset(sites_sf_den_south_feb, YEAR != "2025"),
          aes(fill = YEAR, shape = YEAR),
          size = 1.5, stroke = 0.5, alpha = 0.8, color = "black") +
  
  # Then draw 2025 on top (foreground layer)
  geom_sf(data = subset(sites_sf_den_south_feb, YEAR == "2025"),
          aes(fill = YEAR, shape = YEAR),
          size = 2, stroke = 0.5, alpha = 0.9, color = "black") +
  
  scale_shape_manual(values = c("2015" = 22, "2018" = 23, "2023" = 24, "2025" = 21)) +
  scale_fill_manual(
    values = c("2015" = custom_colors[1],
               "2018" = custom_colors[2],
               "2023" = custom_colors[3],
               "2025" = custom_colors[4])
  ) +
  labs(
    x = "Longitude", y = "Latitude",
    fill = "Year",
    shape = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 5, angle = 90, hjust = 0),
    axis.text.y = element_text(size = 5),
    axis.title = element_text(size = 8),
    text = element_text(size = 6),
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",
    legend.key.size = unit(0.4, "cm"),
    legend.box = "horizontal",
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 5)
  )

ggplot2::ggsave ("plots/southfeb_Site_map_by_year3.png", width = 5, height = 2.5, units = 'in',  bg = "transparent")


#plot sample sites of south filtered
ggplot() +
  geom_sf(data = tutuila_shape, fill = "grey", color = "black") +
  geom_sf(data = sites_sf_den_south, 
          aes(shape = as.factor(YEAR), fill = as.factor(YEAR)),
          size = 1.5, stroke = 0.5, alpha=0.9, color = "black") +
  scale_shape_manual(values = c("2015" = 22, "2018" = 23, "2023" = 24, "2025" = 21)) +
  scale_fill_manual(values = custom_colors) +
  labs(
    x = "Longitude", y = "Latitude",
    fill = "Year",
    shape = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 5, angle = 90, hjust = 0),
    axis.text.y = element_text(size = 5),
    axis.title = element_text(size = 8),
    text = element_text(size = 6),
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",
    legend.key.size = unit(0.4, "cm"),
    legend.box = "horizontal",
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 5)
  )

ggplot2::ggsave ("plots/south_Site_map_by_year_nofeb.png", width = 5, height = 2.5, units = 'in',  bg = "transparent")



