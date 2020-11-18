# EMMA J. GRAHAM LINCK, MS
# Based upon the code of Tung H. Nguyen, Nick L. Arp
# Edited by Tung Nguyen - June 7, 2020

# Setup packages ####
## Setting up environment

### Install packages only if not present
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("forestplot")) install.packages("forestplot")
if (!require("ggmap")) install.packages("ggmap")
if (!require("maps")) install.packages("maps")
if (!require("mapdata")) install.packages("mapdata")

### Loading packages
library(ggpubr); library(tidyverse); library(forestplot); library(maps); library(mapdata); library(ggmap); library(tidyverse)

### Setting working directory for Github repo
setwd('~/Documents/GitHub/ProjectSnafu')

# SECTION 1: Generate Price Index AND combine into observation table ####
ppe_meta = read.csv("./InputData/ppe_meta_data.csv - CURRENT - ppe_meta_data.csv - Sheet1.csv", stringsAsFactors = FALSE)
ppe_meta = ppe_meta %>% filter(type=="grocery")
str(ppe_meta)
ppe_meta$price_of_good = as.numeric(ppe_meta$price_of_good)
AvgZscorePriceIndex = ppe_meta %>%
  filter(!(name_of_good == "oranges")) %>% 
  group_by(name_of_good) %>% 
  mutate(zscore_price = (price_of_good-mean(price_of_good, na.rm = TRUE)) / sd(price_of_good, na.rm = TRUE)) %>% 
  group_by(id) %>% 
  summarize(avg_zscore_price_index = mean(zscore_price, na.rm = TRUE), 
            median_zscore_price_index = median(zscore_price, na.rm = TRUE)) %>% 
  left_join(ppe_meta, by = "id") %>% 
  select(id = id, county = county, name = name, type = type, location = location, avg_zscore_price_index, median_zscore_price_index, pop_density, pop_total, case_rate_june19, case_rate_two_weeks_prior) %>% 
  unique()

# Making combined file of ppe and avg price
small_PI_table = AvgZscorePriceIndex %>% 
  select(id, avg_zscore_price_index, median_zscore_price_index, pop_density, pop_total, case_rate_june19, case_rate_two_weeks_prior)
ppe_obs = read.csv("InputData/ppe_observation_data - CURRENT - ppe_observation_data - Sheet1.csv", stringsAsFactors = FALSE) %>% 
  filter(type=="grocery") %>% 
  left_join(small_PI_table, by = "id")

# Convert blank observations to zero
ppe_obs[is.na(ppe_obs)] = 0

merged = ppe_obs
merged$age = replace(merged$age, merged$age=="A", "a")
merged$age = replace(merged$age, merged$age=="E", "e")
merged$age = replace(merged$age, merged$age=="Y", "y")
merged$age = replace(merged$age, merged$age=="C", "c")

# Filter out rows with the non-standard typo notation of ages
merged = merged %>% filter(age %in% c("a","e","y","c"))
old_age_to_new_age_mapping = data.frame(age = c("a", "e", "y", "c", "t"),
                                         age_two_bins = c("30+", "30+", "0-29", "0-29", "0-29")) # 2-bin age not used for analysis
merged = merged %>% left_join(old_age_to_new_age_mapping, by = "age")
hist(merged$avg_zscore_price_index)
merged$pop_total <- merged$pop_total/10000
merged$case_rate <- merged$case_rate/10
write.csv(merged, "InputData/meta_merged_observed.csv")

#Tung's edits from Oct 30th - some individuals with homemade masks did not have mask box checked
merged %>% filter(homeade == 1, mask == 0 ) # oops looks like there was a mistake at places

merged_new = merged
merged_new[which(merged_new$homeade == 1 & merged_new$mask == 0),"mask"] = 1
write.csv(merged_new, "InputData/meta_merged_observed_Oct31correction.csv")

# SECTION 2: Generate non-dane county table ####
merged <- read.csv("InputData/meta_merged_observed_Oct31correction.csv")

table1_new_names = merged_new %>% group_by(county) %>% summarize(n = n(), mask_usage = paste0(c(sum(mask),"/", n(), " (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% 
  unite(name, county:n, sep = ", n = ", remove = FALSE)
table1_new_names

table1_new_gender = merged_new %>% group_by(county, gender) %>% summarize(mask_usage = paste0(c(sum(mask), "/", n()," (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% 
  pivot_wider(names_from = gender, values_from = mask_usage)
table1_new_gender

table1_new_age = merged_new %>% group_by(county, age) %>% summarize(mask_usage = paste0(c(sum(mask), "/", n()," (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% 
  pivot_wider(names_from = age, values_from = mask_usage)
table1_new_age

table_new_homemade = merged_new %>% group_by(county) %>% summarize(homemade_mask_usage = paste0(c(sum(homeade), "/", sum(mask)," (", round(sum(homeade)/sum(mask)*100, 1), ")"), collapse = "")) #%>% 
#pivot_wider(names_from = gender, values_from = mask_usage)
table_new_homemade
write.csv(table_new_homemade, file="homemade_new_state.csv")

new_table1 = table1_new_names %>% left_join(table1_new_gender, by = "county") %>% left_join(table1_new_age, by = "county") %>% 
   left_join(table_new_homemade, by = "county")
#colnames(total) = colnames(table1)
#final_table1 = rbind(table1, total) # combine main table with total row
View(new_table1)
write.csv(new_table1, file="new_table1.csv")

##SECTION 2B: Emma added Oct 31 - Dane county store specific table
merged <- read.csv("InputData/meta_merged_observed_Oct31correction.csv")
merged_dane <- merged %>% filter(county=="dane")
table1_names = merged_dane %>% group_by(id) %>% summarize(n = n(), mask_usage = paste0(c(sum(mask),"/", n(), " (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% 
  unite(name, id:n, sep = ", n = ", remove = FALSE)
table1_names

table1_gender = merged_dane %>% group_by(id, gender) %>% summarize(mask_usage = paste0(c(sum(mask), "/", n()," (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% 
  pivot_wider(names_from = gender, values_from = mask_usage)
table1_gender

table1_age = merged_dane %>% group_by(id, age) %>% summarize(mask_usage = paste0(c(sum(mask), "/", n()," (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% 
  pivot_wider(names_from = age, values_from = mask_usage)
table1_age

## Finally, make a total row for bottom of table, as 1 row dataframe
total_dane = cbind("Dane", "Total", merged_dane %>% 
                     summarize(n = n(), mask_usage = paste0(c(sum(mask),"/",n(), " (", round(sum(mask)/n()*100, 1), ")"), collapse = "")),
                   merged_dane %>% group_by(gender) %>% summarize(mask_usage = paste0(c(sum(mask), "/",n()," (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% 
                     pivot_wider(names_from = gender, values_from = mask_usage),
                   merged_dane %>% group_by(age) %>% summarize(mask_usage = paste0(c(sum(mask), "/",n()," (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% 
                     pivot_wider(names_from = age, values_from = mask_usage))
names(total_dane) <- c("name", "id", "n", "mask usage", "Males", "Females", "Adult", "Child", "Older adult", "Young adult")

## Setup the final Data Frame
table1 = table1_names %>% left_join(table1_gender, by = "id") %>% left_join(table1_age, by = "id")
names(table1) <- names(total_dane)
final_table1 = rbind(table1, total_dane) %>% select(-name) %>% 
  select(id, n, Males, Females, Child, `Young adult`, Adult, `Older adult`)# combine main table with total row
View(final_table1)
write.csv(final_table1, "./InputData/table_dane_age_gender.csv")

# SECTION 3: Logistic Regression and Odds Ratios ####
merged = read.csv("InputData/meta_merged_observed_Oct31correction.csv",row.names = 1, check.names = F, stringsAsFactors = 1) %>% 
  filter(county=="dane")
merged$gender = as.factor(recode(merged$gender, `0` = "Male", `1` = "Female"))
merged$gender = fct_relevel(merged$gender, ref = "Male")

#merged$age = recode(merged$age, a = "30-59", c = "<18", y = "18-29", e = "60+")
#merged$age = fct_relevel(merged$age, levels = c("<18", "18-29", "30-59", "60+"))
merged$age = fct_relevel(merged$age, levels = c("a", "y", "c", "e"))

merged$age = droplevels(merged$age)


# #merged[merged$county=="kenosha","case_rate"] = 756
# 
# # Setting threshold for high case rate vs low case rate
# threshold = 800
# 
# plot(density(merged$case_rate))
# abline(v=threshold,col="red")
# 
# test = merged
# 
# # #test = merged %>% group_by(county) %>% select(county, case_rate) %>% unique()
# # #plot(density(test$case_rate))
# # #abline(v=threshold,col="red")
# # sum(test$case_rate > threshold) # 4: milwaukee, racine, brown and kenosha - large urban centers
# # sum(test$case_rate < threshold) # 17: including dane (urban, but different: high mask, low prevalence) twice for different days, so total of 20
# # 
# # # expand now to all observations, not county
# # #test = merged$case_rate
# # sum(test > threshold) # 613 observations in high prevalence
# # sum(test < threshold) # 2658 observations in low prevalence
# # 
# # # Threshold is set OK
# # merged = merged %>% mutate(severity = case_when(
# #   case_rate < threshold ~ 0,
# #   case_rate >= threshold ~ 1
# # ))
# # 
# # # side show: scratch block ####
# # test = test %>% mutate(severity = case_when(
# #   case_rate < threshold ~ 0,
# #   case_rate >= threshold ~ 1
# ))
# 
# merged$severity = as.factor(merged$severity)
# sum(is.na(merged$mask))
# 
# 
# test = merged
# #
#test[test$county=="kenosha","case_rate"] = 756
#View(test)
# model = glm(mask ~ age + avg_zscore_price_index + gender + case_rate, data = test, family = binomial)
# summary(model)
# OR = data.frame(exp(cbind("Odds ratio" = coef(model), confint.default(model, level = 0.95))), pvalue = summary(model)$coefficients[,4], check.names = F)
# OR
# 
# threshold = 600
# test = test %>% mutate(severity = case_when(
#   case_rate < threshold ~ 0,
#   case_rate >= threshold ~ 1
# ))
# model = glm(mask ~ age + avg_zscore_price_index + gender + severity, data = test, family = binomial)
# summary(model)
# 
# OR = data.frame(exp(cbind("Odds ratio" = coef(model), confint.default(model, level = 0.95))), pvalue = summary(model)$coefficients[,4], check.names = F)
# OR

# regular show: actual block ####
# Running the logistic regression

#Grahing potential effect of interaction term

# merged_top5_marker %>% 
#   group_by(county) %>% 
#   summarize(percentage_wearing_mask = sum(mask)/n()*100) %>% 
#   right_join(merged_top5_marker, by = "county") %>% 
#   ggplot(aes(x = case_rate_two_weeks_prior, y = percentage_wearing_mask, color = pop_total, label = county)) + 
#   facet_wrap(~Top5) + 
#   geom_point() +
#   geom_smooth(method = lm, se = FALSE) + 
#   scale_color_gradient(low = "grey", high = "blue", na.value = NA) +
#   geom_text(aes(label=county))
# ggsave("./interaction_term_caserate_pop_total.png")



#top 5
#merged_top5 <- merged %>% 
#   filter(county %in% c("dane", "brown", "racine", "kenosha", "milwaukee"))

# merged_nottop5 <- merged %>% 
#   filter(!(county %in% c("dane", "brown", "racine", "outagamie", "winnebago")))
#model = glm(mask ~ age + avg_zscore_price_index + gender +case_rate*pop_total, data = merged, family = binomial)

#summary(model)

# Converting logistic regression coef. into adjusted OR
#OR = data.frame(exp(cbind("Odds ratio" = coef(model), confint.default(model, level = 0.95))), pvalue = summary(model)$coefficients[,4], check.names = F)
#OR

# Remove the intercept row
#abbreviated_1 = OR[-1,1]
#abbreviated_2 = OR[-1,2]
#abbreviated_3 = OR[-1,3]
#abbreviated_4 = OR[-1,4]

## Mixed effects logistic regression

install.packages("lme4")
library(lme4)

mixed_effects <- glmer(mask ~ gender + age + (1|id), family = binomial, data= merged)
se <- sqrt(diag(vcov(mixed_effects)))
(tab <- cbind(Est = fixef(mixed_effects), LL = fixef(mixed_effects) - 1.96 * se, UL = fixef(mixed_effects) + 1.96 *
                se))
exp(tab)
mixed_effects_gender <- glmer(mask ~ gender + (1|id), family = binomial, data= merged)
se <- sqrt(diag(vcov(mixed_effects_gender)))
(tab <- cbind(Est = fixef(mixed_effects_gender), LL = fixef(mixed_effects_gender) - 1.96 * se, UL = fixef(mixed_effects_gender) + 1.96 *
                se))
exp(tab)
summary(mixed_effects_gender)

mixed_effects_age <- glmer(mask ~ age + (1|id), family = binomial, data= merged)
se <- sqrt(diag(vcov(mixed_effects_age)))
(tab <- cbind(Est = fixef(mixed_effects_age), LL = fixef(mixed_effects_age) - 1.96 * se, UL = fixef(mixed_effects_age) + 1.96 *
                se))
exp(tab)
summary(mixed_effects_age)


merged_dane_mks <- merged_dane %>% filter(mask == 1)
mixed_effects_homemade <- glmer(homeade ~ gender + age + (1|id), family = binomial, data= merged_dane_mks)
se <- sqrt(diag(vcov(mixed_effects_homemade)))
(tab_homemade <- cbind(Est = fixef(mixed_effects_homemade), LL = fixef(mixed_effects_homemade) - 1.96 * se, UL = fixef(mixed_effects_homemade) + 1.96 *
                se))
exp(tab_homemade)
summary(mixed_effects_homemade)

mixed_effects_homemade_g <- glmer(homeade ~ gender + (1|id), family = binomial, data= merged_dane_mks)
se <- sqrt(diag(vcov(mixed_effects_homemade_g)))
(tab_homemade <- cbind(Est = fixef(mixed_effects_homemade_g), LL = fixef(mixed_effects_homemade_g) - 1.96 * se, UL = fixef(mixed_effects_homemade_g) + 1.96 *
                         se))
exp(tab_homemade)
summary(mixed_effects_homemade_g)

mixed_effects_homemade_a <- glmer(homeade ~ age + (1|id), family = binomial, data= merged_dane_mks)
se <- sqrt(diag(vcov(mixed_effects_homemade_a)))
(tab_homemade <- cbind(Est = fixef(mixed_effects_homemade_a), LL = fixef(mixed_effects_homemade_a) - 1.96 * se, UL = fixef(mixed_effects_homemade_a) + 1.96 *
                         se))
exp(tab_homemade)
summary(mixed_effects_homemade_a)

#######SECTION 4: Calculating IQR
all = read.csv("InputData/meta_merged_observed_Oct31correction.csv",row.names = 1, check.names = F, stringsAsFactors = 1) %>% 
  filter(county!="dane", gender==0) %>% 
  group_by(id) %>% 
  summarize(mask_we_perc = sum(mask)/n())
summary(all$mask_we_perc)

all = read.csv("InputData/meta_merged_observed_Oct31correction.csv",row.names = 1, check.names = F, stringsAsFactors = 1) %>% 
  filter(county!="dane", gender==1) %>% 
  group_by(id) %>% 
  summarize(mask_we_perc = sum(mask)/n())
summary(all$mask_we_perc)

all = read.csv("InputData/meta_merged_observed_Oct31correction.csv",row.names = 1, check.names = F, stringsAsFactors = 1) %>% 
  filter(county!="dane", age=="c") %>% 
  group_by(id) %>% 
  summarize(mask_we_perc = sum(mask)/n())
summary(all$mask_we_perc)

all = read.csv("InputData/meta_merged_observed_Oct31correction.csv",row.names = 1, check.names = F, stringsAsFactors = 1) %>% 
  filter(county!="dane", age=="y") %>% 
  group_by(id) %>% 
  summarize(mask_we_perc = sum(mask)/n())
summary(all$mask_we_perc)
all = read.csv("InputData/meta_merged_observed_Oct31correction.csv",row.names = 1, check.names = F, stringsAsFactors = 1) %>% 
  filter(county!="dane", age=="a") %>% 
  group_by(id) %>% 
  summarize(mask_we_perc = sum(mask)/n())
summary(all$mask_we_perc)
all = read.csv("InputData/meta_merged_observed_Oct31correction.csv",row.names = 1, check.names = F, stringsAsFactors = 1) %>% 
  filter(county!="dane", age=="e") %>% 
  group_by(id) %>% 
  summarize(mask_we_perc = sum(mask)/n())
summary(all$mask_we_perc)
all = read.csv("InputData/meta_merged_observed.csv",row.names = 1, check.names = F, stringsAsFactors = 1) %>% 
  filter(county=="dane", mask == 1) %>% 
  summarize(mask_we_perc = sum(homeade)/n())

summary(all$mask_we_perc)
# # SECTION 4: Plot figure 1B for aOR ####
# tabletext=cbind(
#   c(" ", "Young adult", "Adult", "Older adult", "High price index", "Female gender", "High case prevalence/10K", "Large population/10K", "Case prevalence:population"),
#   c("aOR", formatC(abbreviated_1, digits = 2, drop0trailing = FALSE, format = "f")),
#   c("Lower CI", formatC(abbreviated_2, digits = 2, format = "f", drop0trailing = FALSE)),
#   c("Upper CI", formatC(abbreviated_3, digits = 2, format = "f", drop0trailing = FALSE)),
#   c("P-value", formatC(abbreviated_4, format = "e", digits = 2)))
# 
# tabletext
# png("forestplot.png", width = 2600, height = 980)
# forestplot(labeltext = tabletext, 
#            mean = c(NA, abbreviated_1), 
#            lower = c(NA, abbreviated_2), 
#            upper = c(NA, abbreviated_3), 
#            ci.vertices = T,
#            is.summary = c(TRUE, rep(FALSE, 9)),
#            fn.ci_norm = fpDrawCircleCI, 
#            boxsize = 0.3,
#            txt_gp = fpTxtGp(ticks = gpar(cex = 3.6), xlab = gpar(cex = 3.6 ), label = gpar(cex =3.6)),
#            xlab = " ",
#            lwd.ci = 6,
#            xlog = TRUE,
#            #xticks = c(0, 1, 2, 3, 4, 5, 6),
#            hrzl_lines = gpar(col = "#444444"),
#            linesheight = "lines", 
#            new_page = FALSE,
#            col = fpColors(box = c("black"), lines = "black"))
# 
# dev.off()
# 
# 
# ##Generating map figure
# 
# 
# 
# #state of WI boundary and is saved as a list with x,y,coordinates
# #of graph called range, and names 
# #wisconsin= map_data('state','wisconsin',fill=FALSE, col=pallete())
# #head(wisconsin)
# 
# #pull state boundaries
# states= map_data("state")
# wisconsin=subset(states,region =='wisconsin')
# 
# #add in county lines
# #column headers are: long,lat,group,order,region,subregion
# usa_counties=map_data('county')
# counties=subset(usa_counties,region=='wisconsin')
# counties$subregion = replace(counties$subregion, counties$subregion=="st croix", "st_croix")
# counties$subregion = replace(counties$subregion, counties$subregion=="fond du lac", "fond_du_lac")
# 
# observation_data = read.csv("./InputData/meta_merged_observed.csv", stringsAsFactors = FALSE) %>% 
#   group_by(county) %>% 
#   summarize(`Face Covering Use (%)` = sum(mask, na.rm = TRUE)/n()*100, `COVID-19 Prevalence \n(per 100K)` = mean(case_rate, na.rm = TRUE)) %>% 
#   unique() %>% 
#   left_join(counties, by = c("county" = "subregion")) %>% unique() 
# 
# #counties we observed in
# #view the data to see which ones to pull
# 
# #pull all rows with subregion of adams,brown,dane,grant,eau claire,green,iowa,
# #jackson,kenosha,lafayette,milwaukee,monroe, outagamie,st croix,pierce,polk,
# #rock, walworth, wood
# observed_counties=observation_data%>% 
#   filter(county %in% c('adams','brown','dane','grant','iowa',
#                        'jackson','kenosha','lafayette','milwaukee',
#                        'monroe','outagamie','st_croix','pierce','polk','walworth','wood','racine', 'winnebago', 'waushara','fond_du_lac'))
# 
# head(observed_counties) #make sure it has all the columns still
# 
# library(RColorBrewer)
# library(maps)
# library(mapdata)
# library(ggplot2)
# library(tidyverse)
# #blank WI with no gridlines in background and fixed coordinates so increasing 
# #figure wont change dimensions
# WIoutline = ggplot(data = wisconsin, mapping = aes(x = long, y = lat, group = group)) + 
#   coord_fixed(1.4) + geom_polygon(fill = "white", color = "black", size = 0.01)
# #add in the county lines with black outlines and safe as WIcounty
# WIoutline
# WIcounty = WIoutline+ theme_nothing(legend = TRUE) + geom_polygon(data=counties, fill = NA, color='black', size=0.05)+
#   geom_polygon(color='black', fill=NA, size = 0.05)
# WIcounty
# WImask = WIcounty + geom_polygon(aes(fill = `Face Covering Use (%)`), data = observed_counties) + 
#   scale_fill_distiller(palette = "Greys", direction = 1)
# WImask
# 
# # custom name for COVID Prevalence per 100,000
# observed_counties = observed_counties %>% rename(`COVID-19 Prevalence \n(per 10,000)` = `COVID-19 Prevalence \n(per 100K)`)
# 
# #add in counties that we observed at outlined in red
# WIobserved = WImask + geom_polygon(aes(color = `COVID-19 Prevalence \n(per 10,000)`), data=observed_counties,
#                                    fill=NA, size = 1) + scale_color_distiller(palette = "RdYlBu")
#   #geom_polygon(color='black',fill=NA)
# WIobserved + theme(legend.key.size = unit(.75, "cm"),
#                    legend.title = element_text(size = 16),
#                    legend.text = element_text(size = 14))
# 
# ggsave("Fig1A.png")
# 
# 
# ##NOTE TO TUNG: should have two files saved in root. One is Fig1A.png and the other is forestplot.png. 
# #Both plots go in figure_continuous.pptx file. X axis labels for forest plot are in ppt, so you can edit directly there. 
# #To change "COVID-19 Prevalence" title, change line 253. Remember to change all subsequent references to that variable name.
# 
# 
# 
# ######### RELATIVE RISK June 18 TN #####
# model = glm(mask ~ age + avg_zscore_price_index + gender + case_rate, data = merged, family = binomial)
# # ran into issue of starting value with age, so I used the solution from https://stackoverflow.com/questions/31342637/error-please-supply-starting-values
# set.seed(123)
# coefini = coef(glm(mask ~ gender + case_rate + avg_zscore_price_index, data = merged, family = binomial(link="log")))
# modelRR = glm(mask ~ age + gender + case_rate + avg_zscore_price_index, data = merged, family = binomial(link="log"), start=c(coefini,0,0,0))
# summary(modelRR)
# exp(coef(modelRR))
# 
# model = modelRR
# 
# RR = data.frame(exp(cbind("Relative risk" = coef(model), confint.default(model, level = 0.95))), pvalue = summary(model)$coefficients[,4], check.names = F)
# RR
# 
# RR = RR[c("agey","agea","agee","avg_zscore_price_index","genderFemale","case_rate"),]
# 
# abbreviated_1 = RR[,1]
# abbreviated_2 = RR[,2]
# abbreviated_3 = RR[,3]
# abbreviated_4 = RR[,4]
# 
# 
# tabletext=cbind(
#   c(" ", "Young adult", "Adult", "Older adult", "High price index", "Female gender", "High case prevalence"),
#   c("aRR", formatC(abbreviated_1, digits = 2, drop0trailing = FALSE, format = "f")),
#   c("Lower CI", formatC(abbreviated_2, digits = 2, format = "f", drop0trailing = FALSE)),
#   c("Upper CI", formatC(abbreviated_3, digits = 2, format = "f", drop0trailing = FALSE)),
#   c("P-value", formatC(abbreviated_4, format = "e", digits = 2)))
# tabletext
# png("forestplot_RR.png", width = 2400, height = 980)
# forestplot(labeltext = tabletext, 
#            mean = c(NA, abbreviated_1), 
#            lower = c(NA, abbreviated_2), 
#            upper = c(NA, abbreviated_3), 
#            ci.vertices = T,
#            is.summary = c(TRUE, rep(FALSE, 7)),
#            fn.ci_norm = fpDrawCircleCI, 
#            boxsize = 0.3,
#            txt_gp = fpTxtGp(ticks = gpar(cex = 3.6), xlab = gpar(cex = 3), label = gpar(cex =4)),
#            xlab = " ",
#            lwd.ci = 6,
#            xlog = TRUE,
#            #xticks = c(0, 1, 2, 3, 4, 5, 6),
#            hrzl_lines = gpar(col = "#444444"),
#            linesheight = "lines", 
#            new_page = FALSE,
#            col = fpColors(box = c("black"), lines = "black"))
# dev.off()
# 
# # some resources for how tung modeled this code:
# # https://stats.idre.ucla.edu/stata/faq/how-can-i-estimate-relative-risk-using-glm-for-common-outcomes-in-cohort-studies/
# # tung chose log-binomial regression model instead of the "Poisson regression with robust error variance"
# 
# ######Robust poisson model for calculation of RR########
# library(robust)
# library(robustbase)
# 
# #merged$age <- as.character(merged$age)
# #merged$gender <- as.character(merged$gender)
# #class(merged$age)
# class(merged$mask)
# merged_new <- merged
# merged_new$mask <- as.numeric(merged_new$mask)
# class(merged_new$age)
# model1 = glmrob(mask ~ age_two_bins + avg_zscore_price_index + gender + case_rate, data = merged_new,
#                    family = "poisson")
# summary(model1)
# 
# model2 = glmRob(mask ~ age_two_bins + avg_zscore_price_index + gender + case_rate, data = merged_new,
#                 family = "poisson")
# summary(model2)
# 
# OR1 = data.frame(exp(cbind("Odds ratio" = coef(model1), confint.default(model1, level = 0.95))), pvalue = summary(model1)$coefficients[,4], check.names = F)
# OR1
# 
# OR2 = data.frame(exp(cbind("Odds ratio" = coef(model2))), pvalue = summary(model2)$coefficients[,4], check.names = F)
# OR2
# 
# coef(model2)
# 
# # ran into issue of starting value with age, so I used the solution from https://stackoverflow.com/questions/31342637/error-please-supply-starting-values
# set.seed(123)
# #coefini = coef(glm(mask ~ gender + case_rate + avg_zscore_price_index, data = merged, family = binomial(link="log")))
# #modelRR = glm(mask ~ age + gender + case_rate + avg_zscore_price_index, data = merged, family = binomial(link="log"), start=c(coefini,0,0,0))
# summary(modelRR)
# exp(coef(modelRR))
# 
# model = modelRR
# 
# RR = data.frame(exp(cbind("Relative risk" = coef(model), confint.default(model, level = 0.95))), pvalue = summary(model)$coefficients[,4], check.names = F)
# RR
# 
# RR = RR[c("agey","agea","agee","avg_zscore_price_index","genderFemale","case_rate"),]
# 
# abbreviated_1 = RR[,1]
# abbreviated_2 = RR[,2]
# abbreviated_3 = RR[,3]
# abbreviated_4 = RR[,4]
# 
# 
# tabletext=cbind(
#   c(" ", "Young adult", "Adult", "Older adult", "High price index", "Female gender", "High case prevalence"),
#   c("aRR", formatC(abbreviated_1, digits = 2, drop0trailing = FALSE, format = "f")),
#   c("Lower CI", formatC(abbreviated_2, digits = 2, format = "f", drop0trailing = FALSE)),
#   c("Upper CI", formatC(abbreviated_3, digits = 2, format = "f", drop0trailing = FALSE)),
#   c("P-value", formatC(abbreviated_4, format = "e", digits = 2)))
# tabletext
# png("forestplot_RR.png", width = 2400, height = 980)
# forestplot(labeltext = tabletext, 
#            mean = c(NA, abbreviated_1), 
#            lower = c(NA, abbreviated_2), 
#            upper = c(NA, abbreviated_3), 
#            ci.vertices = T,
#            is.summary = c(TRUE, rep(FALSE, 7)),
#            fn.ci_norm = fpDrawCircleCI, 
#            boxsize = 0.3,
#            txt_gp = fpTxtGp(ticks = gpar(cex = 3.6), xlab = gpar(cex = 3), label = gpar(cex =4)),
#            xlab = " ",
#            lwd.ci = 6,
#            xlog = TRUE,
#            #xticks = c(0, 1, 2, 3, 4, 5, 6),
#            hrzl_lines = gpar(col = "#444444"),
#            linesheight = "lines", 
#            new_page = FALSE,
#            col = fpColors(box = c("black"), lines = "black"))
# dev.off()
# 
# ############
# #Section looking at effect of masks on future covid growth
# 
# merged_growth <- merged %>% 
#   mutate(diff = (case_rate_june19-case_rate)) %>% 
#   group_by(county, date) %>% 
#   summarize(percentage_mask_wearing = sum(mask)/n()*100, diff, pop_total) %>% unique()
# 
# incidence_model <- glm(diff ~ percentage_mask_wearing + pop_total, data = merged_growth)
# summary(incidence_model)
# cor(merged_growth$percentage_mask_wearing, merged_growth$diff)
# plot(merged_growth$pop_total, merged_growth$diff)
# plot(merged_growth$percentage_mask_wearing, merged_growth$diff)
