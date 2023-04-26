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
fn_plot_rest = function(Data_Set, "My Plot Title") {
        rest_plot <- ggplot(data = Data_Set, aes(geometry = geometry)) +
                geom_sf() +
                geom_sf(data = Data_Set, aes(fill = restaurants_per_inhab)) +
                scale_colour_gradient(low = "#132B43", high = "#56B1F7",
                                      space = "Lab", na.value = "grey50", 
                                      breaks = c(0, 100, 200, 500, 1000, 2000),
                                      aesthetics = "colour") +
                labs(title = "My Plot Title", color = NULL) +
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(),
                      axis.title.y=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank())
        return(rest_plot)
}