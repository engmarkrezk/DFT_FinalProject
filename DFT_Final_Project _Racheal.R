# Loading packages 
library("dplyr")
# easily look at means 
library("rcompanion")
# checking for normality
library("car")
# correct for violation of homogeneity

#One way anova
prop.test(x = 246, n = 6423, alternative = "less")
#Importing Dataset
SFR <- covid19_homeless_impact_WR2_py

# Linear Regression for QN: Are the number of homeless cases affecting the general public
library("car")
library("caret")
library("gvlma")
library("predictmeans")
library("e1071")
library("sandwich")
# Testing Linearity
scatter.smooth(x=SFR$homless_population_by_percentage, y=SFR$general_public_population_by_percentage, main="General Public cases vs Homeless cases")
## Passed the assumption of linearity.

#Testing for Homoscedasticity.
### Creating a linear Model
lmMod <- lm(general_public_population_by_percentage~homless_population_by_percentage, data=SFR)
### Test for homoscedasticity
par(mfrow=c(2,2))
plot(lmMod)
#### the left graphs both dont have flat red lines, hence Heteroscedasticity.

#Further test for heteroscedasticity with Breush-Pagan test
lmtest::bptest(lmMod)
## pvalue= .01 < .05 hence proving heteroscedasticity.

# correcting heteroscedasticity
distBCMod1 <- caret::BoxCoxTrans(SFR$general_public_population_by_percentage)
print(distBCMod1)
## Binding to our dataset
SFRR <- cbind(SFR, dist_newM=predict(distBCMod1, SFR$general_public_population_by_percentage))
lmMod_bc2 <- lm(dist_newM~homless_population_by_percentage, data=SFRR)
lmtest::bptest(lmMod_bc2)
# the pvalue is still <.05 hence violating the assumption of homoscadasticity.

# Testing for outliers
CookD(lmMod, group=NULL, plot=TRUE, idn=3, newwd=TRUE)


