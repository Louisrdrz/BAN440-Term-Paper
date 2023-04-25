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
gerset <- read_csv("gerset.csv") # Data set for Germany as a subset of the TripAdvisor data

# building data set to work with
ger_allinfo2 = fn_create_nut(gerset, "DE")
working_set <- ger_allinfo2


Plot1 <- fn_plot_rest(working_set,1000000000)
ggsave("Plot1.png", plot = Plot1, width = 6, height = 4, dpi = 300)

touristic_df <- read_csv("tourist_nut2.csv")
toursitic_21 <- touristic_df %>% subset(TIME_PERIOD == 2021) %>% subset(., grepl("^DE", geo)) %>% dplyr::select(geo, OBS_VALUE)
tnew <- toursitic_21[nchar(toursitic_21$geo) == 4, ]
min <- tnew[order(tnew$OBS_VALUE), ][1:20, ]
smallest_values <- subset(tnew, OBS_VALUE %in% min$OBS_VALUE)
tourismfilter <- as.list(unique(smallest_values$geo))

working_set <- ger_allinfo2[grepl(paste(tourismfilter, collapse = "|"), ger_allinfo2$NUTS_ID),]

# Pre-Work for Regression ####
working_set$n_rest <- working_set$n_rest %>% as.numeric()
regression_set <- working_set %>% 
  group_by(NUTS_ID) %>% 
  summarise(Population = (mean(Population))/1000, #88% of people in Germany over 20
            MOUNT_TYPE = mean(MOUNT_TYPE),
            URBN_TYPE = mean(URBN_TYPE),
            COAST_TYPE = mean(COAST_TYPE),
            n_rest = mean(n_rest),
            area = mean(area)) %>% 
        subset(area < 700 & Population < 500)
geom <- ger_allinfo2 %>% group_by(NUTS_ID) %>% dplyr::select(geometry) %>% unique()
nut_plot <- ger_allinfo2 %>% group_by(NUTS_ID) %>% summarise(dummy = length(restaurant_link))
plot_set <- working_set %>% merge(., nut_plot, all = TRUE) %>% group_by(NUTS_ID) %>% dplyr::select(restaurants_per_inhab) %>% unique() %>% merge(., geom)
plot_set[is.na(plot_set)] <- 0

Plot2 <- fn_plot_rest(plot_set,1000000000)
ggsave("Plot2.png", plot = Plot2, width = 6, height = 4, dpi = 300)

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


# advanced modelling ####
# Adding variables to the data 
# get cuisines as Boolean variable (We could use this as variables to explain the number of restaurants in a region)
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
table(ger_info_cuisine_nut3$Italian)
# extract the info about cheap food
cheap_eats <- ger_info_tag %>% 
        group_by(NUTS_ID) %>% 
        summarise(cheap = sum(cheap_eats))
regression_set <- regression_set %>% 
        merge(., ger_info_cuisine_nut3, by = "NUTS_ID") %>%
        merge(., cheap_eats, by = "NUTS_ID") 
table(regression_set$Italian)
# create a vector of breaks for grouping
breaks <- c(0, 10, 20, 30, 40, Inf)

# create a vector of labels for the groups
labels <- c("0-10", "11-20", "21-30", "31-40", "40+")

regression_set$Italian <- as.numeric(regression_set$Italian)
# use cut function to group the n_rest column
regression_set$groupI <- cut(regression_set$Italian, breaks = breaks, labels = labels)

regression_set$Italian <- as.factor(regression_set$Italian)
model3 <- polr(Italian ~ log(Population) + cheap + URBN_TYPE, data = regression_set, method = "probit")
summary(model3)


upperb <- 40 ## Number of sections, can be modified

lambda<-model3$coefficients # Estimates
theta<-model3$zeta # Cutoffs

S_N <- exp(theta - mean(regression_set$cheap)*lambda[2])

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




#Code from the lab that is actually not needed
# summarizing the data on the NUT3 level 
# new_set <- working_set %>% group_by(NUTS_ID) %>%
#         summarise(n_rest = mean(n_rest))
# 
# table(new_set$n_rest)
# upperb = 500
# # creating the regression data set by adding dummy variables
# brdata <- new_set %>% 
#   mutate(nrest = as.factor(ifelse(n_rest <= upperb, n_rest,upperb))) %>% 
#   dplyr::select(! n_rest)
# 
# brdata <- dummy_cols(brdata, select_columns="nrest")
# 
# brdata2 <- brdata
# varn <- names(brdata %>% dplyr::select(starts_with("nrest_")))
# 
# # filling the rows with 1 until the true value 
# for (i in 1:nrow(brdata2)) {
#   row_i <- as.matrix(brdata2[i, varn])
#   first_one_col <- match(1, row_i)  # index of the first 1 in the row
#   if (!is.na(first_one_col)) {
#     brdata2[i, (varn[1:(first_one_col - 1)])] <- 1
#   }
# }