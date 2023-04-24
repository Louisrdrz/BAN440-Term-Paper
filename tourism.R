# Load libraries
options(scipen = 999)
library(tidyverse)
library(rnaturalearth) #provides a map of countries of the entire world
library(rworldmap)
library(sf) 
library(giscoR)
library(readxl)
library(readr)
library(MASS)
library(fastDummies)
source("functions.R")

# Data Preparation ####
# loading data sets needed
pop <- read_excel("population.xlsx") # population for the country that is assessed
nuts3 <- st_read("NUTS_RG_20M_2021_3035.shp/NUTS_RG_20M_2021_3035.shp") # NUT3 information
nuts2 <- nuts3 # for NUT2 information
gerset <- read_csv("gerset.csv") # Data set for Germany as a subset of the TripAdvisor data


# building data set to work with
ger_allinfo2 = fn_create_nut(gerset, "DE")

touristic_df <- read_csv("tourist_nut2.csv")
toursitic_21 <- touristic_df %>% subset(TIME_PERIOD == 2021) %>% subset(., grepl("^DE", geo)) %>% dplyr::select(geo, OBS_VALUE)


tnew <- toursitic_21[nchar(toursitic_21$geo) == 4, ]
min <- tnew[order(tnew$OBS_VALUE), ][1:15, ]
smallest_values <- subset(tnew, OBS_VALUE %in% min$OBS_VALUE)
tourismfilter <- as.list(unique(smallest_values$geo))

working_set1 <- ger_allinfo2[grepl(paste(tourismfilter, collapse = "|"), ger_allinfo2$NUTS_ID),]
unique(working_set$n_rest)


# df$groups <- cut(df$numbers, breaks = seq(0, 1000, 10), labels = FALSE)
