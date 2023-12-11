#load all the packages i will need!
library(tidyverse)
library(janitor)
library(censusapi)

#add census api key for later...
my_api_key <- census_api_key("9d733a93b42f2e908d9013ea4f2cd54d1efc04a8", overwrite=TRUE)

#to start out, let's add the census tracts data
#we will be looking at 4 variables: race, hh income, asthma rates, and heart disease rates
#let's start by pulling race and hh income
#to do so, we can use our census api key!

#start by specifying the variables we want
variables <- c(total_pop = "B02001_001E", white_pop = "B02001_002E", median_inc = "B19013_001E")

census_data <- getCensus(
  name = "acs/acs5",
  vintage = 2021,
  vars = variables,
  region = "tract",
  regionin = "state:36",
  key = "9d733a93b42f2e908d9013ea4f2cd54d1efc04a8")

#rename columns, so we know exactly what we're working with...
colnames(census_data) <- c("state", "county", "tract", "total_pop", "white_pop", "median_hh_inc")

#for our study, the only relevant county is Bronx county, represented by code 005
#let's also filter out tracts with 0 pop
bronx <- filter(census_data, county == "005") %>%
  filter(total_pop != 0)

#let's now pull in the health data that we'll need
Asthma2020 <- read_csv(file = "Asthma2020.csv") %>%
  clean_names() %>%
  rename(county = "census_tract_2", tract = "census_tract")
HeartDisease2020 <- read_csv(file ="HeartDisease2020.csv") %>%
  clean_names() %>%
  rename(county = "census_tract_2", tract = "census_tract")

#we'll have to clean the column that contains the county name in order to filter
#let's also get rid of any extra columns while we're at it -- we only want tract and "value"
Asthma2020$county <- substr(Asthma2020$county, start = 1, stop = nchar(Asthma2020$county) - 25)
Asthma2020_bx <- filter(Asthma2020, county == "Bronx") %>%
  select(tract, value) %>%
  rename(asthma = "value")
#the tracts in our census data only have 6 digits
#let's remove them from the asthma data so we can properly join
Asthma2020_bx$tract <- substr(Asthma2020_bx$tract, start = 6, stop = nchar(Asthma2020_bx$tract))

#repeat the process for heart disease...
HeartDisease2020$county <- substr(HeartDisease2020$county, start = 1, stop = nchar(HeartDisease2020$county) - 25)
HeartDisease2020_bx <- filter(HeartDisease2020, county == "Bronx") %>%
  select(tract, value) %>%
  rename(heart_dis = "value")
HeartDisease2020_bx$tract <- substr(HeartDisease2020_bx$tract, start = 6, stop = nchar(HeartDisease2020_bx$tract))

#yay! now we can join these 3 clean data sets together...
bronx_asthma <- left_join(bronx, Asthma2020_bx, by = "tract")
bronx2 <- left_join(bronx_asthma, HeartDisease2020_bx, by = "tract")

#we need to use QGIS now to separate variables that are within 600 ft of the cross-bronx expressway
#let's make a buffer in qgis!

#ok, i did that! let's add in the census tracts...
CBE <- read_csv(file = "CBE_Tracts.csv")
Non_CBE <- read_csv(file = "NonCBE_Tracts.csv")
#let's make a binary variable in each that shows whether it is in/out the buffer
CBE <- CBE %>%
  mutate(CBE = "yes") %>%
  select(TRACTCE, CBE) %>%
  rename(tract = "TRACTCE")
Non_CBE <- Non_CBE %>%
  mutate (CBE = "no") %>%
  select(TRACTCE, CBE) %>%
  rename(tract = "TRACTCE")
#now let's put them back together
CBE_binary <- rbind(CBE, Non_CBE)
#and add it to the bronx data set.
bronx3 <- left_join(bronx2, CBE_binary, by = "tract")

#in addition to the buffer, we will create a field that measures each tract's distance from the expressway
#to do this, first we'll create a centroid in QGIS
#then we'll calculate the centroid's distance from the nearest point on the expressway
dist <- read_csv(file = "Distance_From_CBE.csv")

dist <- dist %>%
  select(TRACTCE, HubDist) %>%
  rename(tract = "TRACTCE") %>%
  distinct(tract, .keep_all = TRUE)

bronx4 <- left_join(bronx3, dist, by = "tract")

#now we know which tracts are in/out of the buffer
#we also know each tract's distance from the expressway's nearest point
#we'll use this dataset to create a scatter plot based on distance from the expressway.
#we'll use whether in/out of the buffer as a binary variable

#we want to compare averages between tracts in/out of buffer
#but one problem is that we have median income
#so let's get data for aggregate income and # of households
#then we can calculate the average income for inside the buffer and outside

variables2 <- c(aggregate_inc = "B19313A_001E", households = "B11001_001E")

census_data2 <- getCensus(
  name = "acs/acs5",
  vintage = 2022,
  vars = variables2,
  region = "tract",
  regionin = "state:36",
  key = "9d733a93b42f2e908d9013ea4f2cd54d1efc04a8")

bronx_avg_inc <- filter(census_data2, county == "005") %>%
  rename(aggregate_inc = "B19313A_001E", households = "B11001_001E") %>%
  filter(aggregate_inc > 0) %>%
  select(tract, aggregate_inc, households)

bronx5 <- left_join(bronx4, bronx_avg_inc, by = "tract")
bronx5$asthma <- as.numeric(bronx5$asthma)
bronx5$heart_dis <- as.numeric(bronx5$heart_dis)

#ok, now we have to do our last calculation and we have a complete dataset!
#we want to find non white pop and % non white

#also, earlier i forgot to calculate n of people with asthma and heart disease
#need this so i can aggregate later between in/out of buffer

bronx_final <- bronx5 %>%
  mutate(pct_nw = ((total_pop - white_pop)/total_pop)*100,
         nw_pop = total_pop - white_pop,
         n_asthma = (total_pop*(asthma/100)),
         n_hd = (total_pop*(heart_dis/100))
         ) 

#save it to a csv that we can use easily in both datawrapper and QGIS
#but we don't need a lot of these variables anymore

bronx_superclean <- bronx_final %>%
  select(asthma, heart_dis, median_hh_inc, CBE, HubDist, pct_nw)
write_csv(bronx_superclean, "Bronx_SuperClean.csv")

#ok! now we can start aggregating and summarize variables between whether they are in/out of buffer
bx_sum <- bronx_final %>%
  group_by(CBE) %>%
  summarize(total_asthma = sum(n_asthma), total_hd = sum(n_hd), 
            total_pop = sum(total_pop),total_nw = sum(nw_pop),
            total_inc = sum(aggregate_inc, na.rm = TRUE), total_hh = sum(households, na.rm = TRUE)) %>%
  mutate(asthma_rate = (total_asthma/total_pop)*100, hd_rate = (total_hd/total_pop)*100,
         pct_nw = (total_nw/total_pop)*100, avg_inc = total_inc/total_hh) %>%
  filter(CBE %in% c("yes", "no")) %>%
  select(CBE, asthma_rate, hd_rate, pct_nw, avg_inc)
write_csv(bx_sum, "CBE_nonCBE.csv")
            
#now we have a data table with the same info, different aggregation
#instead of by tract, it's by whether or not it's in the buffer

#to datawrapper we goooooo (sry ggplot </3)

