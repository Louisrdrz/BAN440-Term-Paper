# Load libraries
library(tidyverse)
library(rnaturalearth) #provides a map of countries of the entire world
library(rworldmap)
library(sf) 
library(giscoR)
library(readxl)
library(readr)

fn_create_nut = function(Data_Set, Country) {
        nut3 <- Data_Set %>% select(restaurant_link, latitude, longitude) %>% na.omit()
        nut3 <- nut3  %>% 
                sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)  %>% 
                sf::st_join(giscoR::gisco_nuts) %>% 
                subset(LEVL_CODE == 3)
        
        n_rest <- nut3 %>% subset(., CNTR_CODE == Country & LEVL_CODE == 3) %>% # Country specific settings here
                group_by(NUTS_ID) %>% summarise(n_rest = n())  %>% st_drop_geometry(.)
        
        # sum(n_rest$n_rest)
        
        nut_clean <- subset(nut3, select = c(restaurant_link, NUTS_ID)) %>% st_drop_geometry(.)
        
        
        nuts_merged <- nut_clean %>%
                merge(., nuts3, by = "NUTS_ID", all = TRUE) %>%
                merge(., n_rest, by = "NUTS_ID", y.all = TRUE) %>%
                subset(., CNTR_CODE == Country & LEVL_CODE == 3) %>% # Country specific settings here
                mutate_all(., ~ replace_na(., 0))
        
        country_allinfo <- merge(nuts_merged, Data_Set , by = "restaurant_link", y.all = TRUE) %>% 
                select(., !province) %>%
                rename(province = NAME_LATN)
        
        country_allinfo <- country_allinfo %>%
                merge(.,pop, by = c("province"))
        
        country_allinfo$Population <- as.integer(country_allinfo$Population)
        country_allinfo <- country_allinfo %>%
                mutate(restaurants_per_inhab = (n_rest/Population)*100000)
        
        return(country_allinfo)
}

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

pop <- read_excel("/Users/justushelfrich/BAN440_TP/Data/population.xlsx") 
NUTS_RG_20M_2021_3035 <- st_read("/Users/justushelfrich/BAN440_TP/Data/NUTS_RG_20M_2021_3035/NUTS_RG_20M_2021_3035.shp")
nuts3 <- NUTS_RG_20M_2021_3035

gerset <- read_csv("data/gerset.csv")
ger_allinfo2 = fn_create_nut(gerset, "DE")

working_set <- ger_allinfo2

# fn_plot_rest(working_set, 75) # 75 seems to be a fair threshold

new_set <- working_set %>% group_by(NUTS_ID) %>% subset(restaurants_per_inhab < 75) %>% summarise(n_rest = mean(n_rest))
table(new_set$n_rest)
upperb = 150

brdata <- new_set %>% mutate(nrest = as.factor(ifelse(n_rest <= upperb, n_rest,upperb))) %>% select(! n_rest)

library(fastDummies)

brdata <- dummy_cols(brdata, select_columns="nrest")


brdata2 <- brdata
varn <- names(brdata %>% dplyr::select(starts_with("nrest_")))

for (i in 1:nrow(brdata2)) {
        row_i <- as.matrix(brdata2[i, varn])
        first_one_col <- match(1, row_i)  # index of the first 1 in the row
        if (!is.na(first_one_col)) {
                brdata2[i, (varn[1:(first_one_col - 1)])] <- 1
        }
}




working_set_new <- working_set %>% group_by(NUTS_ID) %>% 
        summarise(n_rest = mean(n_rest), price_level = sum(grepl("€", value)))
table(working_set$price_level)

# get cuisines as Boolean variable (We could use this as variables to explain the number of resteraunts in a region)
ger_info_cuisine <- ger_allinfo2 %>% select(restaurant_link, NUTS_ID, cuisines) %>% 
        separate_rows(cuisines, sep = ", ") %>%
        mutate(new_col = 1) %>%
        pivot_wider(names_from = cuisines, values_from = new_col, values_fill = 0)
ger_info_cuisine_nut3 <- ger_info_cuisine %>% select(! restaurant_link) %>% group_by(NUTS_ID) %>% summarise_all(sum)


library(MASS)
working_set$n_rest <-  working_set$n_rest %>% as.factor()
model1<-polr(n_rest ~ Population, data=working_set, method="probit")
summary(model1)
