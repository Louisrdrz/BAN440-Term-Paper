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
library(knitr)
library(dplyr)
library(fastDummies)
library(patchwork)
source("functions.R")

# Data Preparation ####
# loading data sets needed
pop <- read_excel("population.xlsx") # population for the country that is assessed
nuts3 <- st_read("NUTS_RG_20M_2021_3035.shp/NUTS_RG_20M_2021_3035.shp") # NUT3 information
nuts2 <- nuts3 # for NUT2 information
nut3area <- read_csv("reg_area3.csv") # for area information on nut3
nuts3 <- nut3area %>% dplyr::select(geo, OBS_VALUE) %>%
  rename(area = OBS_VALUE, NUTS_ID = geo) %>% 
  merge(., nuts3, by = "NUTS_ID")
geom2 <- nuts3 %>% subset(LEVL_CODE == 3 & CNTR_CODE == "DE") %>% dplyr::select(NUTS_ID, geometry) %>% unique()
gerset <- read_csv("gerset.csv") # Data set for Germany as a subset of the TripAdvisor data

# building data set to work with
ger_allinfo = fn_create_nut(gerset, "DE")
info_set <- ger_allinfo


# Descriptive Stats I
plot1_set <-info_set %>% group_by(NUTS_ID) %>% dplyr::select(restaurants_per_inhab) %>% 
        unique() %>% merge(., geom2, all = TRUE)
Plot1 <- fn_plot_rest(plot1_set, "1: All NUT3 regions") # Restaurants in Germany per 100,000 inhabitants
ggsave("Plot1.png", plot = Plot1, width = 6, height = 4, dpi = 300)

# Create data set on touristic regions in Germany
touristic_df <- read_csv("tourist_nut2.csv")
toursitic_21 <- touristic_df %>% subset(TIME_PERIOD == 2021) %>% 
        subset(., grepl("^DE", geo)) %>% 
        dplyr::select(geo, OBS_VALUE)
tnew <- toursitic_21[nchar(toursitic_21$geo) == 4, ]
min <- tnew[order(tnew$OBS_VALUE), ][1:20, ]
smallest_values <- subset(tnew, OBS_VALUE %in% min$OBS_VALUE)
tourismfilter <- as.list(unique(smallest_values$geo))
working_set <- ger_allinfo[grepl(paste(tourismfilter, collapse = "|"), ger_allinfo$NUTS_ID),]

# Pre-Work for Regression ####
working_set$n_rest <- working_set$n_rest %>% as.numeric()
regression_set <- working_set %>%
                group_by(NUTS_ID) %>%
                summarise(
                        Population = (mean(Population)) / 1000,
                        MOUNT_TYPE = mean(MOUNT_TYPE),
                        URBN_TYPE = mean(URBN_TYPE),
                        COAST_TYPE = mean(COAST_TYPE),
                        n_rest = mean(n_rest),
                        area = mean(area)) %>%
                subset(area < 700 & Population < 500)

# Descriptive Stats II (how we cut the data)
plot2_set <- working_set %>% 
        group_by(NUTS_ID) %>% 
        dplyr::select(restaurants_per_inhab) %>% 
        unique() %>% merge(., geom2, all = TRUE)
Plot2 <- fn_plot_rest(plot2_set, "2: Less tourisitc regions") #Restaurants per 100,000 inhabitants: In less tourisitc regions  &
ggsave("Plot2.png", plot = Plot2, width = 6, height = 4, dpi = 300)

plot3_set <- working_set %>% 
        subset(area < 700 & Population < 500000) %>% # advanced filtering for smaller more isolated markets
        group_by(NUTS_ID) %>% 
        dplyr::select(restaurants_per_inhab) %>% 
        unique() %>% merge(., geom2, all = TRUE)
Plot3 <- fn_plot_rest(plot3_set, "3: Smaller and less dense markets") # filtered to smaller and less populated markets
ggsave("Plot3.png", plot = Plot3, width = 6, height = 4, dpi = 300)

combined_plot12 <- Plot2 + Plot3 + plot_layout(ncol = 2) # adding plots together for paper
ggsave("combined_plot12.png", plot = combined_plot, width = 6, height = 4, dpi = 300)

all_plots <- Plot1 + Plot2 + Plot3 + plot_layout(ncol = 3) + plot_annotation(title = "Restaurants in Germany per 100,000 residents, cut down for modeling:")
ggsave("all_plots.png", plot = all_plots, width = 15, height = 6, dpi = 300)
# ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== 
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
regression_set$n_rest <- regression_set$n_rest %>%
        as.factor()

model1 <- polr(group ~ log(Population) + area, data = regression_set, method = "probit")
summary(model1)

upperb <- 12 ## Number of sections, can be modified

lambda<-model1$coefficients # Estimates
theta<-model1$zeta # Cutoffs

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


# ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== 
model2 <- polr(group ~ log(Population) + URBN_TYPE + MOUNT_TYPE + COAST_TYPE, 
               data = regression_set, method = "probit")
summary(model2) # This model returned slightly better thresholds
# model2b <- polr(group ~ log(Population) + area + MOUNT_TYPE + URBN_TYPE + COAST_TYPE + cheap + Italian + German, data = regression_set, method = "probit")
# Warning message: glm.fit: fitted probabilities numerically 0 or 1 occurred 

upperb <- 12 ## Number of sections, can be modified

lambda<-model2$coefficients # Estimates
theta<-model2$zeta # Cutoffs

S_N <- exp(theta - mean(regression_set$URBN_TYPE)*lambda[2] - mean(regression_set$MOUNT_TYPE)*lambda[3] - mean(regression_set$COAST_TYPE)*lambda[4])

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

# ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== 
# advanced modelling ####
# Adding variables to the data 
# get cuisines as Boolean variable (We could use this as variables to explain the number of restaurants in a region)
ger_info_cuisine <- ger_allinfo %>% 
        dplyr::select(restaurant_link, NUTS_ID, cuisines) %>% 
        separate_rows(cuisines, sep = ", ") %>%
        mutate(new_col = 1) %>%
        pivot_wider(names_from = cuisines, values_from = new_col, values_fill = 0)

ger_info_cuisine_nut3 <- ger_info_cuisine %>% 
        dplyr::select(! restaurant_link) %>% 
        group_by(NUTS_ID) %>% 
        summarise_all(sum)

# get info_tag as Boolean variable
ger_info_tag <- ger_allinfo %>% dplyr::select(restaurant_link, NUTS_ID, top_tags) %>% 
        separate_rows(top_tags, sep = ", ") %>%
        mutate(new_col = 1) %>%
        pivot_wider(names_from = top_tags, values_from = new_col, values_fill = 0) %>%
        mutate_at(c("Cheap Eats"), as.numeric) %>%
        rename(cheap_eats = "Cheap Eats") %>% 
        dplyr::select(NUTS_ID, cheap_eats)
table(ger_info_cuisine_nut3$Italian)

# extract the info about cheap food
cheap_eats <- ger_info_tag %>% 
        group_by(NUTS_ID) %>% 
        summarise(cheap = sum(cheap_eats))
regression_set <- regression_set %>% 
        merge(., ger_info_cuisine_nut3, by = "NUTS_ID") %>%
        merge(., cheap_eats, by = "NUTS_ID") 


# create now a data set that is used for modeling
regression_set_short <- regression_set %>% 
        dplyr::select(NUTS_ID, n_rest, Population, area, cheap, URBN_TYPE, MOUNT_TYPE, COAST_TYPE, Italian, German, European, Pizza)

# ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== 
# table(regression_set_short$Italian)
# create a vector of breaks for grouping
breaks <- c(0, 5, 10, 15, 20, 30, 40, Inf)

# create a vector of labels for the groups
labels <- c("0-5", "6-10", "11-15" , "16-20", "21-30", "31-40", "40+")

regression_set_short$Italian <- as.numeric(regression_set_short$Italian)

# use cut function to group the n_rest column
regression_set_short$groupI <- cut(regression_set_short$Italian, breaks = breaks, labels = labels)

regression_set_short$Italian <- as.factor(regression_set_short$Italian)
model3 <- polr(groupI ~ log(Population) + area + cheap + URBN_TYPE, data = regression_set_short, method = "probit")
summary(model3)
upperb <- 6 ## Number of sections, can be modified

lambda<-model3$coefficients # Estimates
theta<-model3$zeta # Cutoffs

S_N <- exp(theta - mean(regression_set_short$area)*lambda[2] - mean(regression_set_short$cheap)*lambda[3]- mean(regression_set_short$URBN_TYPE)*lambda[4])

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

# ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== 

table(regression_set_short$German)
# create a vector of breaks for grouping
breaks <- c(0, 10, 20, 30, 40, 50, 60, 70, 90, Inf)

# create a vector of labels for the groups
labels <- c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-90", "90+")
regression_set_short$German <- as.numeric(regression_set_short$German)
regression_set_short$groupG <- cut(regression_set_short$German, breaks = breaks, labels = labels)
regression_set_short$groupG <- as.factor(regression_set_short$groupG)
model4 <- polr(groupG ~ log(Population) + cheap + area + URBN_TYPE, data = regression_set_short, method = "probit")
summary(model4)

upperb <- 8 ## Number of sections, can be modified

lambda<-model4$coefficients # Estimates
theta<-model4$zeta # Cutoffs

S_N <- exp(theta - mean(regression_set_short$cheap)*lambda[2] - mean(regression_set_short$area)*lambda[3]- mean(regression_set_short$URBN_TYPE)*lambda[4])

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

# ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== 
# Louis' Calculations

regression_set$n_rest <- as.numeric(regression_set$n_rest)
cols <- c("Population", "n_rest", "area")
summary <- summary(dplyr::select(regression_set, cols))
print(summary)
# Print table using knitr
kable(summary, format = "latex")

regression_set$Italian <- as.numeric(regression_set$Italian)
colSums(Filter(is.numeric, regression_set))
vect <- head(sort(colSums(Filter(is.numeric, regression_set[,9:120])), decreasing = TRUE), 15)
vect

#sum(regression_set$`NA`) / sum(vect) 

# Seems there is a lot of variance in the number of banks and population sizes, but there are a lot of markets small enough to apply the BR model.

#Look at raw correlations (not the perfect measure given that number of banks is not continuous but will be a decent approximation):
regression_set$n_rest <- as.numeric(regression_set$n_rest)
regression_set %>% filter(!is.na(Population) & area<Inf & Population>0) %>% summarize(rho=cor(Population,n_rest))




# ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== 
# ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== 
# ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== 
# ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== ======== 

###### BR model #####
new_set <- regression_set
new_set$n_rest <- as.numeric(new_set$n_rest)
table(new_set$n_rest)
upperb = 200
# creating the regression data set by adding dummy variables
brdata <- new_set %>%
        mutate(nrest = as.factor(ifelse(n_rest <= upperb, n_rest,upperb)))

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
upperb2 = 42

brformula1<-as.formula(paste0("nrest ~ Population + Population:(",paste(varn[1:upperb2+1], collapse = " + "),") + area"))
brformula1 
model_BR1<-polr(brformula1, data=brdata2,  method="probit")
# summary(model_BR2)

brformula2<-as.formula(paste0("nrest ~ Population + Population:(",paste(varn[1:upperb2+1], collapse = " + "),") + area + URBN_TYPE + Healthy + Italian + cheap + Cafe + German"))
brformula2 
model_BR2<-polr(brformula2, data=brdata2,  method="probit")
# summary(model_BR2)


