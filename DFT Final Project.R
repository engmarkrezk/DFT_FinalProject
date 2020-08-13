# 1- Load the Libraries:-
library(tidyr)
library(dplyr)
library(stringr)
library(reshape2)

# 2- Import the data:-
# 1st Dataset:
total_no_of_cases <- read.csv("C:/Users/Mina/Desktop/BST/DSO110-Final Group Project/Week 2/Dataset/Original_Datasets/total_no_of_cases.csv")
View(total_no_of_cases) 

# 2nd Dataset:
covid19_homeless_impact <- read.csv("C:/Users/Mina/Desktop/BST/DSO110-Final Group Project/Week 2/Dataset/Original_Datasets/covid19_homeless_impact.csv")
View(covid19_homeless_impact)

# 3rd Dataset:
covid19_cases_by_homelessness_status <- read.csv("C:/Users/Mina/Desktop/BST/DSO110-Final Group Project/Week 2/Dataset/Original_Datasets/covid19_cases_by_homelessness_status.csv")
View(covid19_cases_by_homelessness_status)


# 3- Data Wrangling:-
# 3a: Subset only variables of interest in the 1st Dataset.
total_no_of_cases_1 <- total_no_of_cases[, 10:119]
View(total_no_of_cases_1)

# 3b: Transpose all dates to match counties.
total_no_of_cases_2 <- total_no_of_cases_1 %>%
  gather(date, total_cases, X4.15.2020:X7.31.2020)
View(total_no_of_cases_2)

# 3c: Remove the extra "X" letter from the date column to better join with the other datasets. 
typeof(total_no_of_cases_2$date)  # check what data type is the date column.
nchar(total_no_of_cases_2$date)   # check how many characters in this string date column. 

total_no_of_cases_2$date <- substr(total_no_of_cases_2$date, 2, 10)        # Extract all remaining 9 characters but not the X
View(total_no_of_cases_2)


# 3d: Combining datasets together.
# First, we'll merge  as an Outer Join "total_no_of_cases_2" with "covid19_homeless_impact" by 'date' & 'county'
covid19_homeless_impact_1 <- merge(total_no_of_cases_2, covid19_homeless_impact, by=c("date","county"))
View(covid19_homeless_impact_1)

# Second, We'll group all the dates together in one row to add all values each day in one cell + take only variables of interests. 
covid19_homeless_impact_2 <- aggregate(list(total_cases=covid19_homeless_impact_1$total_cases, rooms=covid19_homeless_impact_1$rooms, rooms_occupied=covid19_homeless_impact_1$rooms_occupied), by = list(date=covid19_homeless_impact_1$date), sum)
View(covid19_homeless_impact_2)

# Third, we'll merge this clean Dataset "covid19_homeless_impact_2" with our final 3rd Dataset "covid19_cases_by_homelessness_status" to add more variables of interests. 
covid19_homeless_impact_3 <- merge(covid19_homeless_impact_2, covid19_cases_by_homelessness_status, by.x=c("date"), by.y=c("specimen_collection_date"))
View(covid19_homeless_impact_3)

# 3f: export this clean Dataset "covid19_homeless_impact_3" to CSV format. 
write.csv(covid19_homeless_impact_3, "C:/Users/Mina/Desktop/BST/DSO110-Final Group Project/Week 2/Dataset/Wrangled_Datasets/covid19_homeless_impact_3.csv", row.names = FALSE)
