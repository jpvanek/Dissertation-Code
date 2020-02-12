##### Summary Statistics for LCFPD Habitat Management
##### 1/3/2020
##### Updated on 2/4/2020 to include mean treat size as percent of preserve size
##### To do: update burn data to include 2018

##### Load packages --------------------------------------------------------

library(tidyverse)
library(tidylog)
library(scales)
library(here)
library(janitor)
options(scipen = 999) # fuck off scientific notation

#### Set Working Directory ------------------------------------------------

here::here()

#### View results ####

MAN_SCORE2 <- readRDS(file = "Management_Score2.RDS")

write.table(MAN_SCORE, file  = "clipboard", sep = "\t", col.names = T, row.names = F)


#### Load data -------------------------------------------------------------

# data created from QGIS project: C:\Users\John\Google Drive\Drive GIS\States\Illinois\Lake County\Management\2019 Management\Management.qgz

# general workflow for each datasource
# treat > buffer 0m > project > calculate area > spatial join one to many > export csv 


PRESERVE <- readxl::read_xlsx("Sampling Effort by Preserve.xlsx")
PRESERVE <-
    select(
        PRESERVE,
        SITE = `FULL NAME`,
        SITE_SHORT = SITE,
        PRIORITY,
        AQUIRED = Aquired,
        AREA = AREAHA
    )

PRESERVE_LIST <- select(PRESERVE, SITE)


CHEM_CSV <-
    read_csv("Management Data/Chemical Treatments.csv", guess_max = 100000) %>% 
    janitor::clean_names() #makes column names consistent

MECH_CSV <-
    read_csv("Management Data/Mechanical Treatments.csv", guess_max = 100000) %>% 
    janitor::clean_names() 

BURN_CSV <-
    read_csv("Management Data/Burn Legacy Treatments.csv", guess_max = 100000) %>% 
    janitor::clean_names()

SEED_CSV <-
    read_csv("Management Data/Seeding Legacy.csv", guess_max = 100000) %>% 
    janitor::clean_names() 

PLANT_CSV <-
    read_csv("Management Data/Planting Legacy.csv", guess_max = 100000) %>% 
    janitor::clean_names() 

#### Chemical -------------------------------------------------------------

#### Format Data

# rename annoyingly long column names
CHEM <- select(
    CHEM_CSV,
    "SITE" = site_name,
    "YEAR" = year,
    "TREAT_HECATRES" = hectares,
    "SITE_HECTARES" = site_acres,
    "PURPOSE" = purpose,
    "SP1" = spc1,
    "SP2" = spc2,
    "SP3" = spc3
)

# filter out sites that aren't a regular part of the monitoring program
# filter out data before 2009 and after 2017
# convert acres to hectares

CHEM2 <- CHEM %>%
    filter(YEAR > 2008 & YEAR < 2018) %>%
    filter(SITE != "Braeloch") %>%
    filter(SITE != "ThunderHawk Golf Club") %>%
    filter(SITE != "Illinois Beach") %>%
    filter(SITE != "Skokie River Woods") %>%
    filter(SITE != "DesPlaines River Trail") %>%
    filter(SITE != "Lake Marie") %>%
    mutate(SITE_HECTARES = SITE_HECTARES/2.471)

CHEM_RAW <- 
    CHEM2 %>%
    group_by(SITE) %>%
    summarize(SITE_HECTARES = mean(SITE_HECTARES),
              NUM_TREATS = n(),
              NUM_YEARS = n_distinct(YEAR),
              AREA_TREAT = sum(TREAT_HECATRES),
              MEAN_AREA = mean(TREAT_HECATRES))



CHEM_PRESERVE <- 
    CHEM2 %>%
    group_by(SITE) %>%
    summarize(SITE_HECTARES = mean(SITE_HECTARES),
              NUM_TREATS = n()/SITE_HECTARES,
              NUM_YEARS = n_distinct(YEAR)/9,
              PER_AREA = sum(TREAT_HECATRES)/SITE_HECTARES,
              PER_MEAN = mean(TREAT_HECATRES)/SITE_HECTARES)

CHEM_PRESERVE <- CHEM_PRESERVE %>% mutate(CHEM_SCORE = rowSums(.[3:6])) 

CHEM_PRESERVE <- left_join(x = PRESERVE_LIST, y = CHEM_PRESERVE, by = "SITE")

CHEM_PRESERVE <- mutate_if(CHEM_PRESERVE, is.numeric, replace_na, replace = 0)

# scales::rescale = (x-min(x))/diff(range(x))

CHEM_SCALED <- CHEM_PRESERVE %>% mutate(CHEM_SCALED = rescale(CHEM_SCORE))


write.table(CHEM_SCALED, file  = "clipboard", sep = "\t", col.names = T, row.names = F)






#### Mechanical -------------------------------------------------------------

#### Format Data

# rename annoyingly long column names
MECH <- select(
    MECH_CSV,
    "SITE" = site_name,
    "YEAR" = year,
    "TREAT_HECATRES" = hectares,
    "SITE_HECTARES" = site_acres,
    "PURPOSE" = action,
    "SP1" = spc1,
    "SP2" = spc2,
    "SP3" = spc3

)

MECH2 <- MECH %>%
    filter(YEAR > 2008 & YEAR < 2018) %>%
    filter(SITE != "Braeloch") %>%
    filter(SITE != "ThunderHawk Golf Club") %>%
    filter(SITE != "Illinois Beach") %>%
    filter(SITE != "Skokie River Woods") %>%
    filter(SITE != "DesPlaines River Trail") %>%
    filter(SITE != "Lake Marie") %>%
    mutate(SITE_HECTARES = SITE_HECTARES/2.471)


# filter out sites that aren't a regular part of the monitoring program
# filter out data before 2009 and after 2018
# convert acres to hectares

MECH_RAW <- 
    MECH2 %>%
    group_by(SITE) %>%
    summarize(SITE_HECTARES = mean(SITE_HECTARES),
              NUM_TREATS = n(),
              NUM_YEARS = n_distinct(YEAR),
              AREA_TREAT = sum(TREAT_HECATRES),
              MEAN_AREA = mean(TREAT_HECATRES))



MECH_PRESERVE <- 
    MECH2 %>%
    group_by(SITE) %>%
    summarize(SITE_HECTARES = mean(SITE_HECTARES),
              NUM_TREATS = n()/SITE_HECTARES,
              NUM_YEARS = n_distinct(YEAR)/9,
              PER_AREA = sum(TREAT_HECATRES)/SITE_HECTARES,
              PER_MEAN = mean(TREAT_HECATRES)/SITE_HECTARES)

MECH_PRESERVE <- MECH_PRESERVE %>% mutate(MECH_SCORE = rowSums(.[3:6])) 

MECH_PRESERVE <- left_join(x = PRESERVE_LIST, y = MECH_PRESERVE, by = "SITE")

MECH_PRESERVE <- mutate_if(MECH_PRESERVE, is.numeric, replace_na, replace = 0)

# scales::rescale = (x-min(x))/diff(range(x))

MECH_SCALED <- MECH_PRESERVE %>% mutate(MECH_SCALED = rescale(MECH_SCORE))


write.table(MECH_SCALED, file  = "clipboard", sep = "\t", col.names = T, row.names = F)

#### Burn -------------------------------------------------------------

#### Format Data

# rename annoyingly long column names
BURN <- select(
    BURN_CSV,
    "SITE" = preserve,
    "YEAR" = burn_date,
    "TREAT_HECATRES" = hectares,
    "SITE_HECTARES" = site_acres,
)

# filter out sites that aren't a regular part of the monitoring program
# filter out data before 2009 and after 2018
# convert acres to hectares

BURN2 <- BURN %>%
    mutate(YEAR = lubridate::year(YEAR)) %>%
    mutate(SITE_HECTARES = SITE_HECTARES/2.471) %>%
    
    filter(YEAR > 2008 & YEAR < 2018) %>%
    filter(SITE != "Braeloch") %>%
    filter(SITE != "ThunderHawk Golf Club") %>%
    filter(SITE != "Illinois Beach") %>%
    filter(SITE != "Skokie River Woods") %>%
    filter(SITE != "DesPlaines River Trail") %>%
    filter(SITE != "Lake Marie") %>%
    filter(SITE != "Casey Trail & Greenway") %>%
    filter(SITE != "Atkinson Stormwater Facility") %>%
    filter(SITE != "Countryside Golf Club") %>%
    filter(SITE != "Duffy Stormwater Facility") %>%
    filter(SITE != "ThunderHawk Golf Club")


BURN_RAW <- 
    BURN2 %>%
    group_by(SITE) %>%
    summarize(SITE_HECTARES = mean(SITE_HECTARES),
              NUM_TREATS = n(),
              NUM_YEARS = n_distinct(YEAR),
              AREA_TREAT = sum(TREAT_HECATRES),
              MEAN_AREA = mean(TREAT_HECATRES))



BURN_PRESERVE <- 
    BURN2 %>%
    group_by(SITE) %>%
    summarize(SITE_HECTARES = mean(SITE_HECTARES),
              NUM_TREATS = n()/SITE_HECTARES,
              NUM_YEARS = n_distinct(YEAR)/9,
              PER_AREA = sum(TREAT_HECATRES)/SITE_HECTARES,
              PER_MEAN = mean(TREAT_HECATRES)/SITE_HECTARES)

BURN_PRESERVE <- BURN_PRESERVE %>% mutate(BURN_SCORE = rowSums(.[3:6])) 

BURN_PRESERVE <- left_join(x = PRESERVE_LIST, y = BURN_PRESERVE, by = "SITE")

BURN_PRESERVE <- mutate_if(BURN_PRESERVE, is.numeric, replace_na, replace = 0)

# scales::rescale = (x-min(x))/diff(range(x))

BURN_SCALED <- BURN_PRESERVE %>% mutate(BURN_SCALED = rescale(BURN_SCORE))


write.table(BURN_SCALED, file  = "clipboard", sep = "\t", col.names = T, row.names = F)



#### Restoration -------------------------------------------------------------

#Seeding

# rename annoyingly long column names
SEED <- select(
    SEED_CSV,
    "SITE" = site_name,
    "TREAT_HECATRES" = hectares,
    "SITE_HECTARES" = site_acres,
)

# filter out sites that aren't a regular part of the monitoring program
# filter out data before 2009 and after 2018
# convert acres to hectares

SEED2 <- SEED %>%
    mutate(SITE_HECTARES = SITE_HECTARES/2.471) %>%
    filter(SITE != "Braeloch") %>%
    filter(SITE != "ThunderHawk Golf Club") %>%
    filter(SITE != "Illinois Beach") %>%
    filter(SITE != "Skokie River Woods") %>%
    filter(SITE != "DesPlaines River Trail") %>%
    filter(SITE != "Lake Marie") %>%
    filter(SITE != "Casey Trail & Greenway") %>%
    filter(SITE != "Atkinson Stormwater Facility") %>%
    filter(SITE != "Countryside Golf Club") %>%
    filter(SITE != "Duffy Stormwater Facility") %>%
    filter(SITE != "ThunderHawk Golf Club")




SEED_RAW <- 
    SEED2 %>%
    group_by(SITE) %>%
    summarize(SITE_HECTARES = mean(SITE_HECTARES),
              NUM_TREATS = n(),
              AREA_TREAT = sum(TREAT_HECATRES),
              MEAN_AREA = mean(TREAT_HECATRES))



SEED_PRESERVE <- 
    SEED2 %>%
    group_by(SITE) %>%
    summarize(SITE_HECTARES = mean(SITE_HECTARES),
              NUM_TREATS = n()/SITE_HECTARES,
              PER_AREA = sum(TREAT_HECATRES)/SITE_HECTARES,
              PER_MEAN = mean(TREAT_HECATRES)/SITE_HECTARES)

SEED_PRESERVE <- SEED_PRESERVE %>% mutate(SEED_SCORE = rowSums(.[3:5])) 

SEED_PRESERVE <- left_join(x = PRESERVE_LIST, y = SEED_PRESERVE, by = "SITE")

SEED_PRESERVE <- mutate_if(SEED_PRESERVE, is.numeric, replace_na, replace = 0)

# scales::rescale = (x-min(x))/diff(range(x))

SEED_SCALED <- SEED_PRESERVE %>% mutate(SEED_SCALED = rescale(SEED_SCORE))


write.table(SEED_SCALED, file  = "clipboard", sep = "\t", col.names = T, row.names = F)

#Planting

# rename annoyingly long column names
PLANT <- select(
    PLANT_CSV,
    "SITE" = site_name,
    "TREAT_HECATRES" = hectares,
    "SITE_HECTARES" = site_acres,
)

# filter out sites that aren't a regular part of the monitoring program
# filter out data before 2009 and after 2018
# convert acres to hectares

PLANT2 <- PLANT %>%
    mutate(SITE_HECTARES = SITE_HECTARES/2.471) %>%
    filter(SITE != "Braeloch") %>%
    filter(SITE != "ThunderHawk Golf Club") %>%
    filter(SITE != "Illinois Beach") %>%
    filter(SITE != "Skokie River Woods") %>%
    filter(SITE != "DesPlaines River Trail") %>%
    filter(SITE != "Lake Marie") %>%
    filter(SITE != "Casey Trail & Greenway") %>%
    filter(SITE != "Atkinson Stormwater Facility") %>%
    filter(SITE != "Countryside Golf Club") %>%
    filter(SITE != "Duffy Stormwater Facility") %>%
    filter(SITE != "ThunderHawk Golf Club")


PLANT_RAW <- 
    PLANT2 %>%
    group_by(SITE) %>%
    summarize(SITE_HECTARES = mean(SITE_HECTARES),
              NUM_TREATS = n(),
              AREA_TREAT = sum(TREAT_HECATRES),
              MEAN_AREA = mean(TREAT_HECATRES))



PLANT_PRESERVE <- 
    PLANT2 %>%
    group_by(SITE) %>%
    summarize(SITE_HECTARES = mean(SITE_HECTARES),
              NUM_TREATS = n()/SITE_HECTARES,
              PER_AREA = sum(TREAT_HECATRES)/SITE_HECTARES,
              PER_MEAN = mean(TREAT_HECATRES)/SITE_HECTARES)

PLANT_PRESERVE <- PLANT_PRESERVE %>% mutate(PLANT_SCORE = rowSums(.[3:5])) 

PLANT_PRESERVE <- left_join(x = PRESERVE_LIST, y = PLANT_PRESERVE, by = "SITE")

PLANT_PRESERVE <- mutate_if(PLANT_PRESERVE, is.numeric, replace_na, replace = 0)

# scales::rescale = (x-min(x))/diff(range(x))

PLANT_SCALED <- PLANT_PRESERVE %>% mutate(PLANT_SCALED = rescale(PLANT_SCORE))


write.table(PLANT_SCALED, file  = "clipboard", sep = "\t", col.names = T, row.names = F)


#Combined Seed and Plant

RESTO2 <- bind_rows(SEED2, PLANT2)

RESTO_RAW <- 
    RESTO2 %>%
    group_by(SITE) %>%
    summarize(SITE_HECTARES = mean(SITE_HECTARES),
              NUM_TREATS = n(),
              AREA_TREAT = sum(TREAT_HECATRES),
              MEAN_AREA = mean(TREAT_HECATRES))



RESTO_PRESERVE <- 
    RESTO2 %>%
    group_by(SITE) %>%
    summarize(SITE_HECTARES = mean(SITE_HECTARES),
              NUM_TREATS = n()/SITE_HECTARES,
              PER_AREA = sum(TREAT_HECATRES)/SITE_HECTARES,
              PER_MEAN = mean(TREAT_HECATRES)/SITE_HECTARES)

RESTO_PRESERVE <- RESTO_PRESERVE %>% mutate(RESTO_SCORE = rowSums(.[3:5])) 

RESTO_PRESERVE <- left_join(x = PRESERVE_LIST, y = RESTO_PRESERVE, by = "SITE")

RESTO_PRESERVE <- mutate_if(RESTO_PRESERVE, is.numeric, replace_na, replace = 0)

# scales::rescale = (x-min(x))/diff(range(x))

RESTO_SCALED <- RESTO_PRESERVE %>% mutate(RESTO_SCALED = rescale(RESTO_SCORE))


write.table(RESTO_SCALED, file  = "clipboard", sep = "\t", col.names = T, row.names = F)

#### Merge -------------------------------------------------------------

PRESERVE <- readxl::read_xlsx("Sampling Effort by Preserve.xlsx")
PRESERVE <-
    select(
        PRESERVE,
        SITE = `FULL NAME`,
        SITE_SHORT = SITE,
        PRIORITY,
        AQUIRED = Aquired,
        AREA = AREAHA
    )


MAN_SCORE <-
    left_join(x = PRESERVE, y = CHEM_SCALED, by = "SITE") %>%
    left_join(y = MECH_SCALED, by = "SITE") %>%
    left_join(y = BURN_SCALED, by = "SITE") %>%
    left_join(y = RESTO_SCALED, by = "SITE") %>%
    select(SITE,
           SITE_SHORT,
           AQUIRED,
           PRIORITY,
           AREA,
           CHEM_SCALED,
           MECH_SCALED,
           BURN_SCALED,
           RESTO_SCALED) %>%
    rowwise() %>%
    mutate(MAN_SCALED = sum(CHEM_SCALED,
                           MECH_SCALED,
                           BURN_SCALED,
                           RESTO_SCALED,
                           na.rm = T)) %>%
    mutate(MAN_SCALED = na_if(MAN_SCALED, 0)) %>%
    select(SITE = SITE_SHORT,
           PRIORITY,
           AQUIRED,
           AREA,
           CHEM_SCALED,
           MECH_SCALED,
           BURN_SCALED,
           RESTO_SCALED,
           MAN_SCALED)



write.table(MAN_SCORE, file  = "clipboard", sep = "\t", col.names = T, row.names = F)
saveRDS(MAN_SCORE,file = "Management_Score2.RDS")

