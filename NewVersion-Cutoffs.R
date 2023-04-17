# Load libraries
library(tidyverse)
library(rnaturalearth) #provides a map of countries of the entire world
library(rworldmap)
library(sf) 
library(giscoR)
library(readxl)
library(readr)
library(MASS)
library(fastDummies)

# creating a function to assign the NUT3 level information to the data based on longitude and latitude
fn_create_nut = function(Data_Set, Country) {
  nut3 <- Data_Set %>% dplyr::select(restaurant_link, latitude, longitude) %>% na.omit()
  nut3 <- nut3  %>% 
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)  %>% 
    sf::st_join(giscoR::gisco_nuts) %>% 
    subset(LEVL_CODE == 3)
  
  n_rest <- nut3 %>% 
    subset(., CNTR_CODE == Country & LEVL_CODE == 3) %>% # Country specific settings here
    group_by(NUTS_ID) %>% 
    summarise(n_rest = n()) %>%
    st_drop_geometry(.)
  
  nut_clean <- subset(nut3, select = c(restaurant_link, NUTS_ID)) %>% 
    st_drop_geometry(.)
  
  nuts_merged <- nut_clean %>%
    merge(., nuts3, by = "NUTS_ID", all = TRUE) %>%
    merge(., n_rest, by = "NUTS_ID", y.all = TRUE) %>%
    subset(., CNTR_CODE == Country & LEVL_CODE == 3) # Country specific settings here
  
  country_allinfo <- merge(nuts_merged, Data_Set , by = "restaurant_link", y.all = TRUE) %>% 
    dplyr::select(., !province) %>%
    rename(province = NAME_LATN)
  
  country_allinfo <- country_allinfo %>%
    merge(.,pop, by = c("province"))
  
  country_allinfo$Population <- as.integer(country_allinfo$Population)
  country_allinfo <- country_allinfo %>%
    mutate(restaurants_per_inhab = (n_rest/Population)*100000)
  
  return(country_allinfo)
}
# creating a function to plot the data for a certain threshold (number of restaurants per 100,000 inhabitants)
fn_plot_rest = function(Data_Set, Threshold) {
  Data_Set <- Data_Set %>% subset(restaurants_per_inhab < Threshold)
  rest_plot <- ggplot(data = Data_Set, aes(geometry = geometry)) +
    geom_sf() +
    geom_sf(data = Data_Set, aes(fill = restaurants_per_inhab)) +
    scale_colour_gradient(low = "#132B43", high = "#56B1F7",
                          space = "Lab", na.value = "grey50", 
                          breaks = c(0, 100, 200, 500, 1000, 2000),
                          aesthetics = "colour")
  return(rest_plot)
}

# Data Preparation ####
# loading data sets needed
pop <- read_excel("population.xlsx") # population for the country that is assessed
nuts3 <- st_read("NUTS_RG_20M_2021_3035.shp/NUTS_RG_20M_2021_3035.shp") # NUT3 information
nuts2 <- st_read("NUTS_RG_20M_2021_3035.shp/NUTS_RG_20M_2021_3035.shp") # NUT3 information
nut3area <- read_csv("reg_area3.csv")
nuts3 <- nut3area %>% dplyr::select(geo, OBS_VALUE) %>%
  rename(area = OBS_VALUE, NUTS_ID = geo) %>% 
  merge(., nuts3, by = "NUTS_ID")
gerset <- read_csv("gerset.csv") # Data set for Germany as a subset of the TripAdvisor data

# building data set to work with
ger_allinfo2 = fn_create_nut(gerset, "DE")
working_set <- ger_allinfo2

# fn_plot_rest(working_set, 75) # 75 seems to be a fair threshold to start with

# summarizing the data on the NUT3 level 
new_set <- working_set %>% group_by(NUTS_ID) %>% 
  # subset(restaurants_per_inhab < 150) %>% 
  subset() %>% 
  summarise(n_rest = mean(n_rest))

table(new_set$n_rest)
upperb = 500

# creating the regression data set by adding dummy variables
brdata <- new_set %>% 
  mutate(nrest = as.factor(ifelse(n_rest <= upperb, n_rest,upperb))) %>% 
  dplyr::select(! n_rest)

brdata <- dummy_cols(brdata, select_columns="nrest")

brdata2 <- brdata
varn <- names(brdata %>% dplyr::select(starts_with("nrest_")))

# filling the rows with 1 until the true value 
for (i in 1:nrow(brdata2)) {
  row_i <- as.matrix(brdata2[i, varn])
  first_one_col <- match(1, row_i)  # index of the first 1 in the row
  if (!is.na(first_one_col)) {
    brdata2[i, (varn[1:(first_one_col - 1)])] <- 1
  }
}

# Adding variables to the data ####
# get cuisines as Boolean variable
# (We could use this as variables to explain the number of restaurants in a region)
ger_info_cuisine <- ger_allinfo2 %>% 
  dplyr::select(restaurant_link, NUTS_ID, cuisines) %>% 
  separate_rows(cuisines, sep = ", ") %>%
  mutate(new_col = 1) %>%
  pivot_wider(names_from = cuisines, values_from = new_col, values_fill = 0)

ger_info_cuisine_nut3 <- ger_info_cuisine %>% 
  dplyr::select(! restaurant_link) %>% 
  group_by(NUTS_ID) %>% summarise_all(sum)

# get info_tag as Boolean variable
ger_info_tag <- ger_allinfo2 %>% dplyr::select(restaurant_link, NUTS_ID, top_tags) %>% 
  separate_rows(top_tags, sep = ", ") %>%
  mutate(new_col = 1) %>%
  pivot_wider(names_from = top_tags, values_from = new_col, values_fill = 0) %>%
  mutate_at(c("Cheap Eats"), as.numeric) %>%
  rename(cheap_eats = "Cheap Eats")

# extract the info about cheap food
cheap_eats <- ger_info_tag %>% 
  group_by(NUTS_ID) %>% 
  summarise(cheap = sum(cheap_eats))

# Pre-Work for Regression ####
nut2 <- nuts2 %>% subset(LEVL_CODE == 2)

# %>% dplyr::select(NUTS_ID, LEVL_CODE) %>% st_drop_geometry(.)
working_set$n_rest <- working_set$n_rest %>% as.numeric()
regression_set <- working_set %>% 
  group_by(NUTS_ID) %>% 
  summarise(Population = mean(Population),
            MOUNT_TYPE = mean(MOUNT_TYPE),
            URBN_TYPE = mean(URBN_TYPE),
            COAST_TYPE = mean(COAST_TYPE),
            n_rest = mean(n_rest),
            area = mean(area))

regression_set <- regression_set %>% 
  merge(., ger_info_cuisine_nut3, by = "NUTS_ID") %>%
  merge(., cheap_eats, by = "NUTS_ID") 
# merge(., nut2, by = "NUTS_ID") 
# subset(., grepl("^DE7", NUTS_ID))
# merge(., brdata2, by = "NUTS_ID")

regression_set$n_rest <- regression_set$n_rest %>%
  as.factor()

# create a vector of breaks for grouping
breaks <- c(0, 50, 70, 90, 110, 130, 140, 165, 190, 220, 250, 300, 450, Inf)

# create a vector of labels for the groups
labels <- c("0-50", "50-70", "70-90", "90-110", "110-130", "130-140", "140-165", "165-190", "190-220", "220-250", "250-300", "300-450", "+450")

regression_set$n_rest <- as.integer(regression_set$n_rest)
# use cut function to group the n_rest column
regression_set$group <- cut(regression_set$n_rest, breaks = breaks, labels = labels)

# calculate the average for each group
aggregate(area ~ group, data = regression_set, FUN = mean)


# Regression ####
model1 <- polr(n_rest ~ MOUNT_TYPE + URBN_TYPE + COAST_TYPE + area, data = regression_set, method = "probit")
summary(model1)

model2 <- polr(n_rest ~ log(Population) + MOUNT_TYPE + URBN_TYPE + COAST_TYPE, data = regression_set, method = "probit")
summary(model2)

model3 <- polr(group ~ log(Population) + area, data = regression_set, method = "probit")
summary(model3)


upperb <- 12 ## Number of sections, can be modified

lambda<-model3$coefficients # Estimates
theta<-model3$zeta # Cutoffs

S_N <- exp(theta - mean(regression_set$area)*lambda[2])

slab<-NULL
for (i in 1:(upperb)) {slab[i]<-paste0("$S_",i,"$")}
names(S_N)<-slab

ETR_N <- exp(theta[2:upperb] - theta[2:upperb-1]) * (1:(upperb-1))/(2:upperb)

elab<-NULL
for (i in 2:upperb) {elab[i-1]<-paste0("$s_",i,"/s_",i-1,"$")}
names(ETR_N)<-elab


knitr::kable(S_N, col.names = c("'000s"), digits = 4,
             caption = 'Entry thresholds',
             booktabs = TRUE)

knitr::kable(ETR_N, col.names = c("ETR"), digits = 4,
             caption = 'Entry threshold ratios',
             booktabs = TRUE)

