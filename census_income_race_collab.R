#assessing representativeness of sample
library(tidyverse)
library(ggpubr)
census_income <- read.csv("./ProjectSnafu/InputData/ACSST5Y2018.S1903_data_with_overlays_2020-05-29T093325.csv",
                   skip = 1, stringsAsFactors = FALSE)
census_income$Estimate..Median.income..dollars...FAMILIES..Families <- as.numeric(census_income$Estimate..Median.income..dollars...FAMILIES..Families)
census_population <- read.csv("./ProjectSnafu/InputData/ACSST5Y2018.S0101_data_with_overlays_2020-05-29T133200.csv", stringsAsFactors = FALSE,
                              skip = 1)
census_race <- read.csv("./ProjectSnafu/InputData/ACSDP5Y2018.DP05_data_with_overlays_2020-05-29T092432.csv",
                        skip = 1, stringsAsFactors = FALSE)
geo_id_data <- read.csv("./ProjectSnafu/InputData/ppe_meta_data.csv - CURRENT - ppe_meta_data.csv - Sheet1.csv") %>% select(id, geo_id) %>% unique()
ppe_meta <- read.csv("./ProjectSnafu/InputData/meta_merged_observed.csv", stringsAsFactors = FALSE) %>% 
  left_join(geo_id_data, by = "id") %>% 
  filter(type == "grocery")
ppe_meta <- ppe_meta %>% 
  mutate(tag = "1400000US")%>% 
  unite(complete_id, c(tag, geo_id), sep = "", remove = FALSE)
ppe_id_match <- ppe_meta %>% select(id, complete_id) %>% unique()
ppe_obs <- read.csv("./ProjectSnafu/InputData/meta_merged_observed.csv", stringsAsFactors = FALSE) %>% 
  left_join(ppe_id_match, by = "id") %>% 
  group_by(complete_id) %>% 
  summarize(n = n())

##Calculating adjusted income for all census tracts in Wisconsin. Adjusted income =  Median family income for census tract divided by total number of people in census tract
all <- census_income %>% left_join(census_population, by = "id") %>% 
   mutate(adj_median_income = Estimate..Median.income..dollars...FAMILIES..Families,
                               adj_median_income_scaled = Estimate..Median.income..dollars...FAMILIES..Families/Estimate..Total..Total.population) %>% 
  select(id, adj_median_income, Estimate..Total..Total.population, adj_median_income_scaled) 

##Calculating adjusted income for census tracts we have sampled.
stuff <- ppe_obs %>% left_join(census_income, by = c("complete_id" = "id")) %>% 
  left_join(census_population, by =c("complete_id" = "id")) %>% 
  mutate(adj_median_income = Estimate..Median.income..dollars...FAMILIES..Families,
         adj_median_income_scaled = Estimate..Median.income..dollars...FAMILIES..Families/Estimate..Total..Total.population,
         zscore = (Estimate..Median.income..dollars...FAMILIES..Families-mean(all$adj_median_income, na.rm = TRUE))/sd(all$adj_median_income, na.rm = TRUE)) %>% 
  left_join(ppe_meta, by = "complete_id") %>% 
  select(complete_id, adj_median_income, name, county) %>% unique()

#a <- stuff %>% ggplot(aes(x = adj_median_income)) + geom_density()+ xlim(0, 200000)                                                   
#b <- all %>% ggplot(aes(x =adj_median_income)) + geom_density() + xlim(0, 200000)          

#ggarrange(a, b)
#ggsave("./PPE/data/income.png")

#Calculating percentage of individuals identifiying as white in each census tract. Adjusted for number of individuals residing in each census tract. 
all_race <- census_race %>% 
  mutate(percent_white = (Estimate..RACE..Total.population..One.race..White/Estimate..RACE..Total.population*100)) %>% 
  select(id, percent_white, Estimate..RACE..Total.population)

##Calculating adjusted percent white for census tracts we have sampled.
stuff_race <- ppe_obs %>% left_join(census_race, by = c("complete_id" = "id")) %>% 
  mutate(percent_white = Estimate..RACE..Total.population..One.race..White/Estimate..RACE..Total.population*100)

#c <- stuff_race %>% ggplot(aes(x =percent_white)) + geom_histogram()+ xlim(0, 100)                                                   
#d <- all_race %>% ggplot(aes(x =percent_white)) + geom_histogram() + xlim(0, 100)          

#ggarrange(c, d)
ggsave("./PPE/data/percent_white.png")


#KS tests to determine if the two distributions are similar. 
ks.test(all_race$percent_white, stuff_race$percent_white)
ks.test(all$adj_median_income, stuff$adj_median_income)


#####STOP HERE. ALL BELOW CODE IS TO GENERATE DOC THAT PEOPLE USED TO FIND COUNTIES. NOT INCLUDED IN PAPER 

###### representative of county

#find adj_median_income in county with all census tracts

all_county <- census_income %>%  left_join(census_population, by = c("id", "Geographic.Area.Name")) %>% 
  mutate(county_name = substr(id, start = 10, stop = 14)) %>% 
  group_by(county_name) %>% 
  summarize(adj_median_income_weight = sum(Estimate..Median.income..dollars...FAMILIES..Families*((Estimate..Total..Total.population/sum(Estimate..Total..Total.population, na.rm = TRUE))), na.rm = TRUE))

#find adj_median_income in county with just our census tracts
stuff_county <- ppe_obs %>%  left_join(census_income, by = c("complete_id" = "id")) %>% 
  left_join(census_population, by =c("complete_id" = "id")) %>% 
  mutate(county_name = substr(complete_id, start = 10, stop = 14)) %>% 
  group_by(county_name) %>% 
  summarize(adj_median_income_weight = sum(Estimate..Median.income..dollars...FAMILIES..Families*((Estimate..Total..Total.population/sum(Estimate..Total..Total.population, na.rm = TRUE))), na.rm =TRUE)) %>% 
  left_join(all_county, by = "county_name") %>% 
  filter(!adj_median_income_weight.x == 0)

#correlate sampled vs county 
cor(stuff_county$adj_median_income_weight.x, stuff_county$adj_median_income_weight.y)
stuff_county <- stuff_county %>% mutate(diff = abs(adj_median_income_weight.x - adj_median_income_weight.y))
hist(stuff_county$diff)

####percent white
all_race_county <- census_race %>% 
  mutate(county_name = substr(id, start = 10, stop = 14)) %>% 
  group_by(county_name) %>%
  summarize(percent_white = mean(Estimate..RACE..Total.population..One.race..White/Estimate..RACE..Total.population*100, na.rm = TRUE))

stuff_race_county <- ppe_obs %>% left_join(census_race, by = c("complete_id" = "id")) %>% 
  mutate(county_name = substr(complete_id, start = 10, stop = 14)) %>% 
  group_by(county_name) %>% 
  summarize(percent_white = mean(Estimate..RACE..Total.population..One.race..White/Estimate..RACE..Total.population*100, na.rm = TRUE)) %>% 
  select(county_name, percent_white) %>% filter(!county_name == "55127")


t <- stuff_race_county %>% left_join(all_race_county, by = "county_name")
#finding correlation between county level percent white and sampled census tract percent whie
cor(t$percent_white.x, t$percent_white.y)






#code that identifies census tracts representative of the county
stuff_small <- stuff %>% unique() %>% select(adj_median_income, complete_id)
stuff_race_small <- stuff_race %>% select(percent_white, complete_id) %>% unique()

race_income_meta_data <- stuff_small %>% 
  left_join(stuff_race_small, by = "complete_id")  %>% 
  select(complete_id, percent_white, adj_median_income) %>% 
  mutate(county_name = substr(complete_id, start = 10, stop = 14)) %>% 
  left_join(ppe_meta, by = "complete_id")
write.csv(race_income_meta_data, "./PPE/data/meta_merged_observed_mobility_race_income.csv")


census_income_small <- census_income %>% select(id, Estimate..Median.income..dollars...FAMILIES..Families)
census_race_small <- census_race %>% select(id, Percent.Estimate..RACE..Total.population..One.race..White)

find_representative_census_tract <- function(county, race_window, income_window){
  census_income_small %>% 
    left_join(census_race_small, by = "id") %>% 
    mutate(county_name = substr(id, start = 10, stop = 14)) %>% 
    left_join(all_race_county) %>% 
    left_join(all_county) %>% 
    mutate(income_diff = abs(Estimate..Median.income..dollars...FAMILIES..Families - adj_median_income_weight), 
           race_diff = abs(as.numeric(Percent.Estimate..RACE..Total.population..One.race..White) - percent_white)) %>% 
    filter(county_name %in% county, income_diff <= income_window, race_diff <= race_window)
}

zipcodes <- data.frame( name = c("Adams", "Brown", "Green", "Kenosha", "Milwaukee", "Outagamie",
                                "Racine", "Rock", "Waushara", "Fond du Lac", "Winnebago"),
                       county_name = c("55001", "55009", "55045", "55059", "55079", "55087", "55101", "55105", "55137", "55039", "55139"))

to_do <- find_representative_census_tract(zipcodes$county_name, 20, 25000) %>% 
  left_join(zipcodes, by = "county_name")
  
write.csv(to_do, "./PPE/data/future_collection_sites_income_within_25000_race_20.csv")

