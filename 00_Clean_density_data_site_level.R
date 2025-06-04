#Creating NCRMP density data at site level using filtered ICRA/ISSP colony level data
#then merging NCRMP site-level density with ESA site-level density (pulled from Dive Nav)

#load libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggridges)

rm(list=ls())

####PREP NCMRP DATA---------------

#load data 
ICRA.dat <- read.csv("NCRMP_COlony_level_TUT_filtered.csv")%>% mutate_if(is.character,as.factor)%>%
  select(YEAR, DATE_, SITE, SPCODE, COLONYID)


site <- read.csv("data/CoralBelt_Adults_raw_CLEANED_2023.csv")%>% mutate_if(is.character,as.factor)%>%
  filter(ISLANDCODE == "TUT", REEF_ZONE == "Forereef", OBS_YEAR != "2020", DEPTH_BIN == "Mid")%>%
  rename(YEAR = OBS_YEAR)%>%
  select(YEAR, DATE_, SITE, LATITUDE, LONGITUDE, MAX_DEPTH_M, TRANSECTAREA)%>%
  distinct()%>%
  droplevels()

#sum transect area per site
site <- site %>% 
  group_by(YEAR, SITE, LATITUDE, LONGITUDE, MAX_DEPTH_M)%>%
  mutate(SURVEYAREA = sum(TRANSECTAREA))%>%
  select(-TRANSECTAREA)%>%
  distinct()
#115 total sites


#colonies per site
col <-  ICRA.dat%>%
  group_by(YEAR, SITE) %>%
  summarise(COL_COUNT = n_distinct(COLONYID))

NCRMP <- left_join(site, col)%>%
  replace(is.na(.), 0) %>%
mutate(
  DATE_ = as.character(DATE_),                     # factor → character
  DATETIME = ymd_hms(DATE_),                       # parse full datetime properly
  DATE_only = as.Date(DATETIME),                   # just the date
  TIME_only = format(DATETIME, "%H:%M:%S"),        # just the time
  DATE_formatted = format(DATE_only, "%m/%d/%Y") # custom format
)

NCRMP$YEAR <- as.factor(NCRMP$YEAR)

####read in and merge ESA data to NCRMP data
ESA <- read_csv("data/ESA_Corals_Site_Density_2025.csv") %>% mutate_if(is.character,as.factor)
as.factor(ESA$YEAR <- "2025")

ESA <- ESA %>% 
  rename(DATE_formatted=Date, SITE=Site, LATITUDE =Lat, LONGITUDE = Long, SURVEYAREA = Survey_area, MAX_DEPTH_M = Max_depth_m)%>%
select(YEAR, DATE_formatted, SITE, LATITUDE, LONGITUDE, MAX_DEPTH_M, SURVEYAREA, COL_COUNT)


NCRMP <- NCRMP %>% select(YEAR, DATE_formatted, SITE, LATITUDE, LONGITUDE, MAX_DEPTH_M, SURVEYAREA, COL_COUNT)

#ESA$DATE_ <- as.factor(ESA$DATE_)

#double check column names are the same 
colnames(NCRMP)
colnames(ESA)

SITE_COUNT <- rbind(NCRMP, ESA)


#quick check of number of sites surveyed per year
tbl_sites<- SITE_COUNT%>% 
  group_by(YEAR) %>%
  summarise(N =length(unique(SITE)))

ALL_COLONY_DENSITY<- SITE_COUNT %>%
  mutate(DENSITY=(COL_COUNT/SURVEYAREA))%>%
  ungroup()

save(ALL_COLONY_DENSITY, file="data/ALL_COLONY_DENSITY.RData")

#write.csv(NCRMP, "NCRMP_ICRA_density_site-level.csv")
write_csv(SITE_COUNT, "All_ICRA_Site_Counts.csv")

#subset these data to only include data points from South side of Tutuila to remove surveying bias (ICRA is rare on north side)
#this was done by importing this data into ArcGIS and selectign points manually. See map script for visualization.
south_den<-read.csv("South_ICRA_Site_Counts.csv")

#remove feb 2025 data
south_den <- south_den %>%
  left_join(
    NCRMP %>% select(SITE, DATE_formatted) %>% rename(date_ncrmp = DATE_formatted),
    by = "SITE"
  ) %>%
  left_join(
    ESA %>% select(SITE, DATE_formatted) %>% rename(date_esa = DATE_formatted),
    by = "SITE"
  ) %>%
  mutate(
    DATE_formatted = coalesce(date_ncrmp, date_esa),     
    DATE = mdy(DATE_formatted),                          
    month = month(DATE)                                  
  ) %>%
  select(-date_ncrmp, -date_esa) #%>%
 # filter(!(month == 2 & year(DATE) == 2025))


south_den$COL_COUNT <- as.numeric(south_den$COL_COUNT)
south_den$SURVEYAREA <- as.numeric(south_den$SURVEYAREA)

south_den1<-south_den%>%
  rename(YEAR=YEAR.x) %>%
mutate(DENSITY=(COL_COUNT/SURVEYAREA))%>%
  ungroup()

save(south_den1, file="data/FEB_SOUTH_COLONY_DENSITY.RData")

#load in the colonies that were too big to be counted (from colony-level script)
load("data/colonies_removed_due_to_size.RData")


#removed_summary%>%
#  rename(YEAR = OBS_YEAR)%>%

#subtract the # colonies that should be removed. Then calculate density
SOUTH_COLONY_DENSITY<- south_den %>%
  filter(!(month == 2 & year(DATE) == 2025))%>%
  mutate(DENSITY=(COL_COUNT/SURVEYAREA))%>%
    ungroup()%>%
  rename(YEAR=YEAR.x) 


save(SOUTH_COLONY_DENSITY, file="data/SOUTH_COLONY_DENSITY.RData")

SOUTH_COLONY_DENSITY_filtered<- SOUTH_COLONY_DENSITY %>%
  left_join(removed_summary, by = "SITE") %>%
  mutate(
    adjusted_colony_count = COL_COUNT - coalesce(removed_count, 0),
    adjusted_density = adjusted_colony_count / SURVEYAREA
  )
save(SOUTH_COLONY_DENSITY, file="data/SOUTH_COLONY_DENSITY_filtered.RData")

#Use adjusted_density in future analyses:
save(SOUTH_COLONY_DENSITY_filtered, file="data/SOUTH_COLONY_DENSITY_filtered.RData")
write.csv(SOUTH_COLONY_DENSITY_filtered, file="data/SOUTH_COLONY_DENSITY_filtered.csv")
