# 1- Load the Libraries:-
library(tidyr)
library(dplyr)
library(stringr)
library(reshape2)
library(rlang)
library(ggplot2)
library(gmodels)

library(car)
library(caret)
library(gvlma)
library(predictmeans)
library(e1071)
library(sandwich)

library(rcompanion)
library(IDPmisc)
library(rstatix) 




# 2- Import the data:-
covid19_homeless_impact_Final <- read.csv("C:/Users/engma/Desktop/BST/DSO110-Final Group Project/DFT_Group/covid19_homeless_impact_Final.csv")
View(covid19_homeless_impact_Final)



# 3- Run the Analyses:-
# Mark's questions: 

# Q1:
# 3a: Question Set Up: 
#    Has the rooms availability in the homeless shelters been affected by the covid cases in the county of San Francisco?


# 3b: Data Wrangling: reformat the 2 variables of interest to Categorical Data in order to run the independent Chi-Square. 
# Recode 'cumulative_homeless_cases' variable
covid19_homeless_impact_Final$cumulative_homeless_cases_R < N/A
covid19_homeless_impact_Final$cumulative_homeless_cases_R[covid19_homeless_impact_Final$cumulative_homeless_cases <= 165] <- 0
covid19_homeless_impact_Final$cumulative_homeless_cases_R[covid19_homeless_impact_Final$cumulative_homeless_cases > 165]  <- 1
View(covid19_homeless_impact_Final)

# Recode 'rooms_available' variable
covid19_homeless_impact_Final$rooms_available_R < N/A
covid19_homeless_impact_Final$rooms_available_R[covid19_homeless_impact_Final$rooms_available <= 866] <- 0
covid19_homeless_impact_Final$rooms_available_R[covid19_homeless_impact_Final$rooms_available > 866]  <- 1
View(covid19_homeless_impact_Final)

# 3c: Testing Assumptions:
# 1- we have independent data. 
# 2- the expected frequencies must be greater than 5 for each cell.

# 3d: Creating the CrossTable:
CrossTable(covid19_homeless_impact_Final$cumulative_homeless_cases_R, covid19_homeless_impact_Final$rooms_available_R, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")

# Check Assumption of Expected Frequencies: we have met this assumption. 

# 3e: Interpret Results:
# Looks like we have a p-value of 0.03. So this analysis is statistically significant. 

ggplot(covid19_homeless_impact_Final, aes(x = cumulative_homeless_cases_R, y = rooms_available_R)) + geom_boxplot(aes(group=rooms_available_R))

#          ___________________________________________________________________________________

# Q2:
# 3a: Question Set Up: 
#                     Online source has stated that the homeless population was hit 80% harder compared to the general public. 


# 3b: Data Wrangling: 
# We'll work with the last number in the variables 'cumulative_homeless_cases' & 'cumulative_general_public_cases'

# 3c: Run Analysis:
observed = c(246, 6177)
expected = c(0.80, 0.20)
chisq.test(x = observed, p = expected)

#  the p value is less than .05, then our observed and expected values differ. 
# In this case, this means that the homeless impact is not 80% compared to the general public.  
# in order to know exactly the difference of impact in percentage between the 2 population. we'll run the z-test analysis between the 2 proportions in the whole population.


# 3e: Run two proportion z-test Analysis:
# There are 9,784 homeless_population and 873,521 general_public_population in San Francisco, 
# Off of the homeless_population, there are 246 covid19 positive cases and of the general_public_population, there are 6177 covid19 positive cases.
prop.test(x = c(246, 6177), n = c(9784, 873521),
          alternative = "two.sided")
# since the p value is < 0.05 that means the proportions of the impact of covid in the 2 populations are not equal. 
# In the homeless_Population, they are impacted by %2.51
# in the general_Public_ pop, they are impacted by %0.71 

# Using this tool @ https://www.calculatorsoup.com/calculators/algebra/percent-difference-calculator.php
# we conclude that the Homeless population was hit %111.8 harder than the general public population. 

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Rachael's questions: 

# Q3:
# 3a: Question Set Up: 
#                     Is the homeless population being affected the same way as the general public population?

# 3c: Run Analysis: One proportion ztest
prop.test(x = 246, n = 6423, alternative = "less")

#          ___________________________________________________________________________________

# Q4:
# 3a: Question Set Up: 
#          Are the number of homeless cases affecting the general public. Are the homeless spreading COVID19 because they have no homes?

# 3b: Data Wrangling:
SFR <- covid19_homeless_impact_Final

# 3c: Testing Assumptions: We'll run the Linear Regression model

# Testing Linearity
scatter.smooth(x=SFR$homless_population_by_percentage, y=SFR$general_public_population_by_percentage, main="General Public cases vs Homeless cases")
## Passed the assumption of linearity.

#Testing for homoscedasticity.
### Creating a linear Model
lmMod <- lm(general_public_population_by_percentage~homless_population_by_percentage, data=SFR)

### Test for homoscedasticity
par(mfrow=c(2,2))
plot(lmMod)
#### the left graphs both don't have flat red lines, hence heteroscedasticity. 

#Further test for heteroscedasticity with Breush-Pagan test
lmtest::bptest(lmMod)
## p-value= .01 < .05 hence proving heteroscedasticity.

# correcting heteroscedasticity
distBCMod1 <- caret::BoxCoxTrans(SFR$general_public_population_by_percentage)
print(distBCMod1)

# Binding to our current Dataset
SFRR <- cbind(SFR, dist_newM=predict(distBCMod1, SFR$general_public_population_by_percentage))
lmMod_bc2 <- lm(dist_newM~homless_population_by_percentage, data=SFRR)
lmtest::bptest(lmMod_bc2)
# the p-value is still <.05 hence violating the assumption of homoscedasticity.

#From the residual graphs generated, we observe no cone so you have passed the assumption of homogeneity of variance.

# Testing for X outliers
CookD(lmMod, group=NULL, plot=TRUE, idn=3, newwd=TRUE)
### there are outliers observed rows 88,89,90.
lev = hat(model.matrix(lmMod))
plot(lev)
SFR[lev>.2,]
## we can conclude that there are no X outliers

# Testing for Y outliers
car::outlierTest(lmMod)
### since p-value < .05 so there are Y outliers

# Testing for x and Y outliers
summary(influence.measures(lmMod))
## there dffit value that is above 1 so there are influential outliers in the data.

summary(lmMod_bc2)
## reject null Hypothesis is rejected .Fstatistic is big.adjusted R^2 is .9989 so 99%chance that AM height affects PM height.

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Elijah's questions: 

# Q5:
# 3a: Question Set Up: 
#                     Is the homeless population being affected the same way as the general public population?