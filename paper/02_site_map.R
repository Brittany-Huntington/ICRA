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

load("data/ICRA_PM_SIZE_USE.Rdata") #PM data
load("data/SOUTH_COLONY_DENSITY_filtered.RData") #density 


tutuila_shape <- st_read("data/Tut_shapefiles/TUT.shp") #make sure folder has all shapefiles needed

#plot PM, density across island

# mean PM per year/site of south only
mean_PM_per_year_site_south <- s %>%
  group_by(YEAR, SITE, LATITUDE, LONGITUDE) %>%
  summarise(mean_PM = mean(PER_DEAD, na.rm = TRUE))

#assign coordinates for PM data. Want to keep PM=0 but not NA
sites_sf_PM_south <- mean_PM_per_year_site_south %>%
  #filter(!is.na(mean_PM)) %>%
  distinct(LATITUDE, LONGITUDE, YEAR, .keep_all = TRUE) %>%  # keep all columns
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)  # WGS84 


#plot PM
sites_sf_PM_south <- sites_sf_PM_south %>%
  mutate(shape_code = ifelse(is.na(mean_PM), "NA", as.character(YEAR)))

ggplot() +
  geom_sf(data = tutuila_shape, fill = "gray90", color = "black") +
  geom_sf(data = sites_sf_PM_south, aes(color = mean_PM), 
          size = 2, alpha = 0.7) +
  # geom_text(data = sites_sf_PM_south, #ADD SITE LABELS
  #           aes(x = st_coordinates(geometry)[,1],
  #               y = st_coordinates(geometry)[,2],
  #               label = SITE),
  #           size = 2, hjust = -0.2, vjust = 0) +
  
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

#ggplot2::ggsave ("plots/Map_Partial_mortaltiy_2025.jpeg", width = 10, height = 5, units = 'in')

# plot density
#assign coordinates for PM data. Want to keep PM=0 but not NA
colnames(SOUTH_COLONY_DENSITY_filtered)
sites_density_south <- SOUTH_COLONY_DENSITY_filtered %>%
  mutate(LATITUDE = LATITUDE.x,
         LONGITUDE = LONGITUDE.x)%>%
  #filter(!is.na(mean_PM)) %>%
  distinct(LATITUDE, LONGITUDE, YEAR, .keep_all = TRUE) %>%  # keep all columns
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)  # WGS84 

#assign coordinates for density data. Want to remove NA 
sites_sf_density_south <- sites_density_south %>%
  filter(!is.na(DENSITY)) %>%
  distinct(LATITUDE.x, LONGITUDE.x, YEAR, .keep_all = TRUE) %>%  # keep all columns
  st_as_sf(coords = c("LONGITUDE.x", "LATITUDE.x"), crs = 4326)  # WGS84 

sites_sf_density_south$log_mean_den <- log10(sites_sf_density_south$DENSITY + 0.01)


#plot sample sites of south filtered
ggplot() +
  geom_sf(data = tutuila_shape, fill = "grey", color = "black") +
  geom_sf(data = sites_sf_density_south, 
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
# Set YEAR as a factor in chronological order for the legend
sites_sf_density_south$YEAR <- factor(sites_sf_density_south$YEAR, levels = c("2015", "2018", "2023", "2025"))

ggplot() +
  geom_sf(data = tutuila_shape, fill = "grey", color = "black") +
  
  # First draw all years except 2025 (background layers)
  geom_sf(data = subset(sites_sf_density_south, YEAR != "2025"),
          aes(fill = YEAR, shape = YEAR),
          size = 1.5, stroke = 0.5, alpha = 0.8, color = "black") +
  
  # Then draw 2025 on top (foreground layer)
  geom_sf(data = subset(sites_sf_density_south, YEAR == "2025"),
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
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 14, face="bold"),
    text = element_text(size = 6),
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",
    legend.key.size = unit(0.4, "cm"),
    legend.box = "horizontal",
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 11)
  )

ggplot2::ggsave ("paper/sites_map.pdf", width = 5, height = 3, units = 'in',  bg = "transparent")



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



