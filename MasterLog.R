# TUNG H NGUYEN, MS
# Based upon the code of Emma Graham, Nick Arp, and Kat Ruedinger
# June 4, 2020

# Setup packages ####
# install.packages("ggpubr")
# install.packages("tidyverse")
library(ggpubr)
library(tidyverse)


# should be stable across other people:
setwd('~/Documents/GitHub/ProjectSnafu')

# Generate Price Index ####
ppe_meta = read.csv("./ProjectSnafu/InputData/ppe_meta_data.csv - CURRENT - ppe_meta_data.csv - Sheet1.csv", stringsAsFactors = FALSE)

ppe_meta = ppe_meta %>% filter(type=="grocery")

str(ppe_meta)
ppe_meta$price_of_good <- as.numeric(ppe_meta$price_of_good)
AvgZscorePriceIndex <- ppe_meta %>%
  filter(!(name_of_good=="oranges")) %>% 
  group_by(name_of_good) %>% 
  mutate(zscore_price = (price_of_good-mean(price_of_good, na.rm = TRUE))/sd(price_of_good, na.rm = TRUE)) %>% 
  group_by(id) %>% 
  summarize(avg_zscore_price_index = mean(zscore_price, na.rm = TRUE), 
            median_zscore_price_index = median(zscore_price, na.rm = TRUE)) %>% 
  left_join(ppe_meta, by = "id") %>% 
  select(id = id, county = county, name = name, type = type, location = location, avg_zscore_price_index, median_zscore_price_index) %>% 
  unique()

# making combined file of ppe and avg price

small_PI_table <- AvgZscorePriceIndex %>% 
  select(id, avg_zscore_price_index, median_zscore_price_index)
ppe_obs <- read.csv("./ProjectSnafu/InputData/ppe_observation_data - CURRENT - ppe_observation_data - Sheet1.csv", stringsAsFactors = FALSE) %>% 
  filter(type=="grocery") %>% 
  left_join(small_PI_table, by = "id")

ppe_obs[is.na(ppe_obs)] <- 0

merged <- ppe_obs
merged$age <- replace(merged$age, merged$age=="A", "a")
merged$age <- replace(merged$age, merged$age=="E", "e")
merged$age <- replace(merged$age, merged$age=="Y", "y")
merged$age <- replace(merged$age, merged$age=="C", "c")

# filter out rows with the weird ages
merged = merged %>% filter(age %in% c("a","e","y","c"))

old_age_to_new_age_mapping <- data.frame(age = c("a", "e", "y", "c", "t"),
                                         age_two_bins = c("30+", "30+", "0-29", "0-29", "0-29"))

merged <- merged %>% left_join(old_age_to_new_age_mapping, by = "age")
merged$location <- replace(merged$location, merged$location=="rurual", "rural")

hist(merged$avg_zscore_price_index)
write.csv(merged, "./ProjectSnafu/InputData/meta_merged_observed.csv")
# Table 1 ####
library(tidyverse)
#merged <- read.csv("/content/drive/Shared drives/MSTP 2019/Users/Emma/meta_merged_observed.csv", stringsAsFactors = FALSE) %>% filter(!(age %in% c("t", "0")), !(name %in% c("whl_fds", "metfalfes")), type == "grocery")

#merged$county[merged$county=="dane"] <- "Dane"
#merged$county[merged$county=="grant"] <- "Grant"
#merged$county[merged$county=="iowa"] <- "Iowa"
#merged$county[merged$county=="jackson"] <- "Jackson"
#merged$county[merged$county=="lafayette"] <- "Lafayette"
#merged$county[merged$county=="monroe"] <- "Monroe"
#merged$county[merged$county=="pierce"] <- "Pierce"
#merged$county[merged$county=="polk"] <- "Polk"
#merged$county[merged$county=="st_croix"] <- "St. Croix"
#merged$county[merged$county=="walworth"] <- "Walworth"

# table1_names <- merged %>% group_by(county) %>% 
#   summarize(n = n(), mask_usage = paste0(c(round(sum(mask)/n()*100, 1), " (", n(), ")"), collapse = "")) %>% 
#   unite(name, county:n, sep = ", n = ", remove = FALSE)
# table1_names
# table1_gender <- merged %>% group_by(county, gender) %>% 
#   summarize(mask_usage = paste0(c(round(sum(mask)/n()*100, 1), " (", n(), ")"), collapse = "")) %>% 
#   pivot_wider(names_from = gender, values_from = mask_usage)
# #table1_gender
# table1_age <- merged %>% group_by(county, age) %>% 
#   summarize(mask_usage = paste0(c(round(sum(mask)/n()*100, 1), " (", n(), ")"), collapse = "")) %>% 
#   pivot_wider(names_from = age, values_from = mask_usage)
# #table1_age
# table1_price_index <- merged %>% group_by(county) %>% 
#   summarize(max_PI = round(max(avg_zscore_price_index, na.rm = TRUE), 1), min_PI = round(min(avg_zscore_price_index, na.rm = TRUE), 1)) %>% 
#   unite(range, min_PI:max_PI, sep = " - " )
# table1_case_rate <- merged %>% select(county, case_rate) %>% unique()
# table1 <- table1_names %>% left_join(table1_gender, by = "county") %>% left_join(table1_age, by = "county") %>% left_join(table1_price_index, by = "county") %>% 
#   left_join(table1_case_rate, by = "county") %>% select(County = name, mask_usage, `0`, `1`, c, y, a, e, range, case_rate ) %>% mutate(case_rate = round(case_rate, 2))
# colnames(table1) <- c("County", "Total", "Male", "Female", "<18", "18-29", "30-59", "60+", "Price Index", "Cases per 100k")
# table1
# write.csv(table1, "Demographics/table.csv")

table1_names <- merged %>% group_by(county) %>% summarize(n = n(), mask_usage = paste0(c(sum(mask),"/",n(), " (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% 
  unite(name, county:n, sep = ", n = ", remove = FALSE)
table1_names

table1_gender <- merged %>% group_by(county, gender) %>% summarize(mask_usage = paste0(c(sum(mask), "/",n()," (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% pivot_wider(names_from = gender, values_from = mask_usage)
table1_gender

table1_age <- merged %>% group_by(county, age) %>% summarize(mask_usage = paste0(c(sum(mask), "/",n()," (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% pivot_wider(names_from = age, values_from = mask_usage)
table1_age

table1_price_index <- merged %>% group_by(county) %>% summarize(max_PI = round(max(avg_zscore_price_index, na.rm = TRUE), 1), min_PI = round(min(avg_zscore_price_index, na.rm = TRUE), 1)) %>% unite(range, min_PI:max_PI, sep = "," )
table1_price_index
table1_case_rate <- merged %>% select(county, case_rate, no_cases) %>% unique() %>% filter(no_cases != 538) #%>% mutate(case_rate = round(case_rate, 2) #%>% paste0(case_rate, " (", no_cases, ")", collapse = "")
table1_case_rate
# lol here is Tung and Nick unable to figure out how to do it in Tidy
table1_case_rate$combined <- paste0(table1_case_rate$case_rate, " (", table1_case_rate$no_cases, ")")
table1_case_rate <- table1_case_rate %>% select(county, combined) #%>% # note that i took the first occurence of dane, so the higher case rate
table1_case_rate = table1_case_rate %>% arrange(county, desc(combined)) %>% distinct(county,.keep_all=TRUE)
table1_case_rate

# Make a total column
total = cbind("Wisconsin","Total",merged %>% summarize(n = n(), mask_usage = paste0(c(sum(mask),"/",n(), " (", round(sum(mask)/n()*100, 1), ")"), collapse = "")),
  merged %>% group_by(gender) %>% summarize(mask_usage = paste0(c(sum(mask), "/",n()," (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% pivot_wider(names_from = gender, values_from = mask_usage),
  merged %>% group_by(age) %>% summarize(mask_usage = paste0(c(sum(mask), "/",n()," (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% pivot_wider(names_from = age, values_from = mask_usage),
  0,0)
ncol(total)

table1 <- table1_names %>% left_join(table1_gender, by = "county") %>% left_join(table1_age, by = "county") %>% left_join(table1_price_index, by = "county") %>% left_join(table1_case_rate, by = "county") #%>% select(County = name, mask_usage, c, y, a, e, range, combined ) #%>% mutate(combined = round(combined, 2))
colnames(total) = colnames(table1)
#colnames(table1) <- c("County", "Total", "Male", "Female", "<18", "18-29", "30-59", "60+", "Price Index", "Cases per 100k")
final_table1 = rbind(table1,total)
View(final_table1)

write.table(final_table1, "./ProjectSnafu/Demographics/table.txt",sep="\t",quote=F,row.names=F)

# Let's calculate some odds ratios ####
#install.packages("ggpubr")
library(ggpubr)
#merged <- read.csv("/content/drive/Shared drives/MSTP 2019/Project - PPE/meta_merged_observed_mobility_race_income.csv") %>% filter(!(age %in% c("t", "0")), !(name %in% c("whl_fds", "metfalfes")), type == "grocery")
merged = read.csv("./ProjectSnafu/InputData/meta_merged_observed.csv",row.names = 1, check.names = F, stringsAsFactors = 1)

merged$gender <- as.factor(recode(merged$gender, `0` = "Male", `1` = "Female"))
merged$gender <- fct_relevel(merged$gender, ref = "Male")
#merged = merged %>% filter(!name=="whl_fds")

merged$age <- recode(merged$age, a = "30-59", c = "<18", y = "18-29", e = "60+")
merged$age <- fct_relevel(merged$age, levels = c("<18", "18-29", "30-59", "60+"))
merged$age <- droplevels(merged$age)
print(dim(merged))
#gender <- merged %>% group_by(gender) %>% summarize(`Percentage wearing face-coverings` = sum(mask, na.rm = TRUE)/n()*100) %>% ggplot(aes(x = as.factor(gender), y = `Percentage wearing face-coverings`)) + geom_bar(stat = "identity") + xlab(" ") + theme_bw()
#age <- merged %>% group_by(age) %>% summarize(`Percentage wearing face-coverings` = sum(mask, na.rm = TRUE)/n()*100) %>% ggplot(aes(x = as.factor(age), y = `Percentage wearing face-coverings`)) + geom_bar(stat = "identity") + xlab(" ") + theme_bw()
#merged %>% ggplot(aes(x = workplaces_percent_change_from_baseline)) + geom_histogram()

#merged$weekend = as.factor(merged$weekend)
merged$mask = as.factor(merged$mask)
class(merged$mask)


merged <- merged %>% mutate(severity = case_when(
  case_rate < 800 ~ 0,
  case_rate >= 800 ~ 1
))
merged$severity <- as.factor(merged$severity)
sum(is.na(merged$mask))
model = glm(mask ~ age + avg_zscore_price_index + gender + severity, data = merged, family=binomial)
summary(model)

str(merged)
#View(merged)
#merged
OR = data.frame(exp(cbind("Odds ratio" = coef(model), confint.default(model, level = 0.95))), pvalue = summary(model)$coefficients[,4])
OR
# output_glm_logit <- function(place, outcome){
#   data <- merged
#   #d <- summary(glm(get(outcome)~gender + age +avg_zscore_price_index + case_rate + weekend, data = data))
#   d <- summary(glm(location~gender + age +avg_zscore_price_index + case_rate, data = data))
#   
#   print(d)
#   stuff <- data.frame(d$coefficients)[-1,]
#   stuff$name <- c("Male gender", "18-29 yrs old", "30-59 yrs old", "60+", "Price Index", "Cases per 100k")
#   rownames(stuff) <- NULL
#   stuff %>% select(Variable = name, Coefficient = Estimate, pvalue = `Pr...t..`)
#   
# }
#merged %>% ggplot(aes(x = avg_zscore_price_index)) + geom_histogram() + facet_wrap(~weekend)
#cor(merged$weekend, merged$workplaces_percent_change_from_baseline)
#table <- ggtexttable(output_glm_logit(c("urban","rural"), "mask"), rows = NULL)
#ggarrange(gender, age, table, labels = c("B", "C", "D"))
#ggsave("/content/drive/Shared drives/MSTP 2019/Project - PPE/Figure 2/figure2.png")

# install.packages("meta")
# library(meta)
# install.packages("devtools")
# 
# library(devtools)
# devtools::install_github("NightingaleHealth/ggforestplot", build_vignettes = TRUE, force = TRUE)
# library(tidyverse)
# library(ggforestplot)
# 
# # Get subset of example, log odds ratios, data frame
# df_logodds <-
#   df_logodds_associations %>%
#   dplyr::arrange(name) %>%
#   dplyr::left_join(ggforestplot::df_NG_biomarker_metadata, by = "name") %>% 
#   dplyr::filter(group == "Amino acids") %>%
#   # Set the study variable to a factor to preserve order of appearance
#   # Set class to factor to set order of display.
#   dplyr::mutate(
#     study = factor(
#       study,
#       levels = c("Meta-analysis", "NFBC-1997", "DILGOM", "FINRISK-1997", "YFS")
#     )
#   )
# library(ggforestplot)
# # Forestplot
# forestplot(
#   df = df_logodds,
#   estimate = beta,
#   logodds = TRUE,
#   colour = study,
#   shape = study,
#   title = "Associations to type 2 diabetes",
#   xlab = "Odds ratio for incident type 2 diabetes (95% CI)
#   per 1−SD increment in metabolite concentration"
# ) +
#   # You may also want to add a manual shape scale to mark meta-analysis with a
#   # diamond shape
#   ggplot2::scale_shape_manual(
#     values = c(23L, 21L, 21L, 21L, 21L),
#     labels = c("Meta-analysis", "NFBC-1997", "DILGOM", "FINRISK-1997", "YFS")
#   )


install.packages("forestplot")
library(forestplot)
#forest = structure(list(
#  mean = OR[,1],
#  lower = OR[,2],
#  upper = OR[,3],
#  .Names = c("mean", "lower", "upper"), 
#  row.names = c(NA, -7L), 
#  class = "data.frame")
#)

#forestplot(forest)
#tabletext<-cbind(
#  c(" ", row.names(OR)),
#  c("Mean", OR[,1]),
#  c("Lower CI", OR[,2]),
#  c("Upper CI", OR[,3]))

abbreviated_1 <- OR[-1,1]
abbreviated_2 <- OR[-1,2]
abbreviated_3 <- OR[-1,3]
abbreviated_4 <- OR[-1,4]


tabletext<-cbind(
  c(" ", "Young adult", "Adult", "Older adult", "High price index", "Female gender", "High case prevalence"),
  c("aOR", formatC(abbreviated_1, digits = 2, drop0trailing = FALSE, format = "f")),
  c("Lower CI", formatC(abbreviated_2, digits = 2, format = "f", drop0trailing = FALSE)),
  c("Upper CI", formatC(abbreviated_3, digits = 2, format = "f", drop0trailing = FALSE)),
  c("P-value", formatC(abbreviated_4, format = "e", digits = 2)))
tabletext
png("forestplot.png", width =2400, height=980)
forestplot( labeltext = tabletext, 
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

