# EMMA J. GRAHAM LINCK, MS
# Based upon the code of Tung H. Nguyen, Nick L. Arp
# Edited by Tung Nguyen - June 7, 2020

# Setup packages ####
## Setting up environment

### Install packages only if not present
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("forestplot")) install.packages("forestplot")

### Loading packages
library(ggpubr); library(tidyverse); library(forestplot)

### Setting working directory for Github repo
setwd('~/Documents/GitHub/ProjectSnafu')

# SECTION 1: Generate Price Index AND combine into observation table ####
ppe_meta = read.csv("InputData/ppe_meta_data.csv - CURRENT - ppe_meta_data.csv - Sheet1.csv", stringsAsFactors = FALSE)
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
  select(id = id, county = county, name = name, type = type, location = location, avg_zscore_price_index, median_zscore_price_index) %>% 
  unique()

# Making combined file of ppe and avg price
small_PI_table = AvgZscorePriceIndex %>% 
  select(id, avg_zscore_price_index, median_zscore_price_index)
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
write.csv(merged, "InputData/meta_merged_observed.csv")


# SECTION 2: Generate TABLE 1 ####
table1_names = merged %>% group_by(county) %>% summarize(n = n(), mask_usage = paste0(c(sum(mask),"/", n(), " (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% 
  unite(name, county:n, sep = ", n = ", remove = FALSE)
table1_names

table1_gender = merged %>% group_by(county, gender) %>% summarize(mask_usage = paste0(c(sum(mask), "/", n()," (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% 
  pivot_wider(names_from = gender, values_from = mask_usage)
table1_gender

table1_age = merged %>% group_by(county, age) %>% summarize(mask_usage = paste0(c(sum(mask), "/", n()," (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% 
  pivot_wider(names_from = age, values_from = mask_usage)
table1_age

table1_price_index = merged %>% group_by(county) %>% summarize(max_PI = round(max(avg_zscore_price_index, na.rm = TRUE), 1), 
                                                               min_PI = round(min(avg_zscore_price_index, na.rm = TRUE), 1)) %>% unite(range, min_PI:max_PI, sep = "," )
table1_price_index
table1_case_rate = merged %>% select(county, case_rate, no_cases) %>% 
  unique()
table1_case_rate

table1_case_rate$combined = paste0(table1_case_rate$case_rate, " (", table1_case_rate$no_cases, ")")
table1_case_rate = table1_case_rate %>% select(county, combined) %>% # note that i took the first occurence of dane, so the higher case rate
  arrange(county, desc(combined)) %>% distinct(county, .keep_all=TRUE)
table1_case_rate

## Finally, make a total row for bottom of table, as 1 row dataframe
total = cbind("Wisconsin", "Total", merged %>% 
                summarize(n = n(), mask_usage = paste0(c(sum(mask),"/",n(), " (", round(sum(mask)/n()*100, 1), ")"), collapse = "")),
  merged %>% group_by(gender) %>% summarize(mask_usage = paste0(c(sum(mask), "/",n()," (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% 
    pivot_wider(names_from = gender, values_from = mask_usage),
  merged %>% group_by(age) %>% summarize(mask_usage = paste0(c(sum(mask), "/",n()," (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% 
    pivot_wider(names_from = age, values_from = mask_usage),
  0,0)
ncol(total)

## Setup the final Data Frame
table1 = table1_names %>% left_join(table1_gender, by = "county") %>% left_join(table1_age, by = "county") %>% 
  left_join(table1_price_index, by = "county") %>% left_join(table1_case_rate, by = "county")
colnames(total) = colnames(table1)
final_table1 = rbind(table1, total) # combine main table with total row
View(final_table1)

## Write out the Table 1 for Publication
write.table(final_table1, "Demographics/table.txt", sep="\t", quote=F, row.names=F)


# SECTION 3: Logistic Regression and Odds Ratios ####
merged = read.csv("InputData/meta_merged_observed.csv",row.names = 1, check.names = F, stringsAsFactors = 1)
merged$gender = as.factor(recode(merged$gender, `0` = "Male", `1` = "Female"))
merged$gender = fct_relevel(merged$gender, ref = "Male")

merged$age = recode(merged$age, a = "30-59", c = "<18", y = "18-29", e = "60+")
merged$age = fct_relevel(merged$age, levels = c("<18", "18-29", "30-59", "60+"))
merged$age = droplevels(merged$age)
print(dim(merged))

merged$mask = as.factor(merged$mask)
class(merged$mask)

str(merged)

#merged[merged$county=="kenosha","case_rate"] = 756

# Setting threshold for high case rate vs low case rate
threshold = 800

plot(density(merged$case_rate))
abline(v=threshold,col="red")

test = merged

#test = merged %>% group_by(county) %>% select(county, case_rate) %>% unique()
#plot(density(test$case_rate))
#abline(v=threshold,col="red")
sum(test$case_rate > threshold) # 4: milwaukee, racine, brown and kenosha - large urban centers
sum(test$case_rate < threshold) # 17: including dane (urban, but different: high mask, low prevalence) twice for different days, so total of 20

# expand now to all observations, not county
#test = merged$case_rate
sum(test > threshold) # 613 observations in high prevalence
sum(test < threshold) # 2658 observations in low prevalence

# Threshold is set OK
merged = merged %>% mutate(severity = case_when(
  case_rate < threshold ~ 0,
  case_rate >= threshold ~ 1
))

# side show: scratch block ####
test = test %>% mutate(severity = case_when(
  case_rate < threshold ~ 0,
  case_rate >= threshold ~ 1
))

merged$severity = as.factor(merged$severity)
sum(is.na(merged$mask))


test = merged
#
#test[test$county=="kenosha","case_rate"] = 756
#View(test)
model = glm(mask ~ age + avg_zscore_price_index + gender + case_rate, data = test, family = binomial)
summary(model)
OR = data.frame(exp(cbind("Odds ratio" = coef(model), confint.default(model, level = 0.95))), pvalue = summary(model)$coefficients[,4], check.names = F)
OR

threshold = 600
test = test %>% mutate(severity = case_when(
  case_rate < threshold ~ 0,
  case_rate >= threshold ~ 1
))
model = glm(mask ~ age + avg_zscore_price_index + gender + severity, data = test, family = binomial)
summary(model)

OR = data.frame(exp(cbind("Odds ratio" = coef(model), confint.default(model, level = 0.95))), pvalue = summary(model)$coefficients[,4], check.names = F)
OR

# regular show: actual block ####
# Running the logistic regression
model = glm(mask ~ age + avg_zscore_price_index + gender + severity, data = merged, family = binomial)
summary(model)

# Converting logistic regression coef. into adjusted OR
OR = data.frame(exp(cbind("Odds ratio" = coef(model), confint.default(model, level = 0.95))), pvalue = summary(model)$coefficients[,4], check.names = F)
OR

# Remove the intercept row
abbreviated_1 = OR[-1,1]
abbreviated_2 = OR[-1,2]
abbreviated_3 = OR[-1,3]
abbreviated_4 = OR[-1,4]

# SECTION 4: Plot figure 1B for aOR ####
tabletext=cbind(
  c(" ", "Young adult", "Adult", "Older adult", "High price index", "Female gender", "High case prevalence"),
  c("aOR", formatC(abbreviated_1, digits = 2, drop0trailing = FALSE, format = "f")),
  c("Lower CI", formatC(abbreviated_2, digits = 2, format = "f", drop0trailing = FALSE)),
  c("Upper CI", formatC(abbreviated_3, digits = 2, format = "f", drop0trailing = FALSE)),
  c("P-value", formatC(abbreviated_4, format = "e", digits = 2)))

tabletext
png("forestplot.png", width = 2400, height = 980)
forestplot(labeltext = tabletext, 
           mean = c(NA, abbreviated_1), 
           lower = c(NA, abbreviated_2), 
           upper = c(NA, abbreviated_3), 
           ci.vertices = T,
           is.summary = c(TRUE, rep(FALSE, 7)),
           fn.ci_norm = fpDrawCircleCI, 
           boxsize = 0.3,
           txt_gp = fpTxtGp(ticks = gpar(cex = 3.6), xlab = gpar(cex = 3), label = gpar(cex =4)),
           xlab = " ",
           lwd.ci = 6,
           xlog = TRUE,
           #xticks = c(0, 1, 2, 3, 4, 5, 6),
           hrzl_lines = gpar(col = "#444444"),
           linesheight = "lines", 
           new_page = FALSE,
           col = fpColors(box = c("black"), lines = "black"))
dev.off()

