# Loading packages 
library("dplyr")
# easily look at means 
library("rcompanion")
# checking for normality
library("car")
# correct for violation of homogeneity

#Importing Dataset
SFR <- covid19_homeless_impact_WR2.python

# We will be running a One way Anova statistical analysis for the question of
# Is there a difference homeless and the Settled population in terms of percentage cases of population?

# Test assumptions 
# 1. Normality for Homeless population percent cases and settled population percent cases

plotNormalHistogram(SFR$total_homless_population_by_percentage )
# Homeless population percent cases Look normally distributed

plotNormalHistogram(SFR$total_population_by_percentage )
# Settled population percent cases Look normally distributed

# Testing for Homogeneity of variance since both are normally distributed
bartlett.test(total_population_by_percentage ~ county ,data=SF)


#One way anova
prop.test(x = 246, n = 6423, alternative = "less")
