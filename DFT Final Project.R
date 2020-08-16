# 1- Load the Libraries:-
library(tidyr)
library(dplyr)
library(stringr)
library(reshape2)
library(rlang)
library(ggplot2)


# 2- Import the data:-
# 1st Dataset:
total_no_of_cases <- read.csv("C:/Users/engma/Desktop/BST/DSO110-Final Group Project/DFT_Group/Original_Datasets/total_no_of_cases.csv")
View(total_no_of_cases) 

# 2nd Dataset:
covid19_homeless_impact <- read.csv("C:/Users/engma/Desktop/BST/DSO110-Final Group Project/DFT_Group/Original_Datasets/covid19_homeless_impact.csv")
View(covid19_homeless_impact)

# 3rd Dataset:
covid19_cases_by_homelessness_status <- read.csv("C:/Users/engma/Desktop/BST/DSO110-Final Group Project/DFT_Group/Original_Datasets/covid19_cases_by_homelessness_status.csv")
View(covid19_cases_by_homelessness_status)


# 3- Data Wrangling:-
# 3a: Subset only variables of interest in the 1st Dataset.
total_no_of_cases_WR1 <- total_no_of_cases[, 11:119]
View(total_no_of_cases_WR1)

# filter only for San Francisco county.
total_no_of_cases_WR1 <- filter(total_no_of_cases_WR1, county == "San Francisco")
View(total_no_of_cases_WR1)

# 3b: Transpose all dates to match counties.
total_no_of_cases_WR2 <- total_no_of_cases_WR1 %>%
  gather(date, cumulative_population_cases, X4.15.2020:X7.31.2020)
View(total_no_of_cases_WR2)

# 3c: Remove the extra "X" letter from the date column to better join with the other datasets. 
typeof(total_no_of_cases_WR2$date)  # check what data type is the date column.
nchar(total_no_of_cases_WR2$date)   # check how many characters in this string date column. 

total_no_of_cases_WR2$date <- substr(total_no_of_cases_WR2$date, 2, 10)        # Extract all remaining 9 characters but not the X
View(total_no_of_cases_WR2)


# 3d: Combining datasets together.
# First, we'll merge  as an Outer Join "total_no_of_cases_2" with "covid19_homeless_impact" by 'date' & 'county'
covid19_homeless_impact_WR1 <- merge(total_no_of_cases_WR2, covid19_homeless_impact, by=c("date","county"))
View(covid19_homeless_impact_WR1)

# Second, we'll merge this clean Dataset "covid19_homeless_impact_WR1" with our final 3rd Dataset "covid19_cases_by_homelessness_status" to add more variables of interests. 
covid19_homeless_impact_WR2 <- merge(covid19_homeless_impact_WR1, covid19_cases_by_homelessness_status, by.x=c("date"), by.y=c("specimen_collection_date"))
View(covid19_homeless_impact_WR2)

# 3f: export this clean Dataset "covid19_homeless_impact_3" to CSV format. 
write.csv(covid19_homeless_impact_WR2, "C:/Users/engma/Desktop/BST/DSO110-Final Group Project/DFT_Group/covid19_homeless_impact_WR2.csv", row.names = FALSE)


# 4- Check Assumptions:-
summary(covid19_homeless_impact_3$cumulative_population_cases)
summary(covid19_homeless_impact_3$cumulative_homeless_cases)

# 4a: Normally Distributed.
ggplot(covid19_homeless_impact_3, aes(sample = rooms)) + geom_qq()                                  # rooms_plot
ggplot(covid19_homeless_impact_3, aes(sample = rooms_occupied)) + geom_qq()                         # rooms_occupied_plot
ggplot(covid19_homeless_impact_3, aes(sample = cumulative_population_cases)) + geom_qq()            # cumulative_population_cases_plot
ggplot(covid19_homeless_impact_3, aes(sample = cumulative_homeless_cases)) + geom_qq()              # cumulative_homeless_cases_plot
ggplot(covid19_homeless_impact_3, aes(sample = total_population_by_percentage)) + geom_qq()         # total_population_by_percent_plot
ggplot(covid19_homeless_impact_3, aes(sample = total_homless_population_by_percentage)) + geom_qq() # total_homeless_pop_by_percent_plot



