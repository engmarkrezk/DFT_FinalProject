# 1- Load the Libraries:-

library("rcompanion")
library("car")
library(IDPmisc )
library(ggplot2)
library(rstatix) 
library(dplyr)

Finally <- read.csv("C:/Users/engma/Desktop/BST/DSO110-Final Group Project/DFT_Group/Finally.csv")
View(Finally)


counties <-Finally


keeps <- c("ï..county","X.April", "X.May", "X.June", "X.July")
counties2 <- counties[keeps]

counties3 <- counties2[c(10,15,38,56),]

# 1- Normality Test:-

plotNormalHistogram(counties3$X.April)
plotNormalHistogram(sqrt(counties3$X.April))


plotNormalHistogram(counties3$X.May)

plotNormalHistogram(counties3$X.June)
plotNormalHistogram(counties3$X.June ^2)


plotNormalHistogram(counties3$X.July)
plotNormalHistogram(log(counties3$X.July))



# 2- Homogeneity of Variance:

leveneTest(repdat ~ Treatment.Group*contrasts, data=counties21)

data1 <- counties3
df <- data1[,2:length(colnames(data1))]
df.april <- data.frame(df[,1])
df.april$repdat <- df$X.April
df.april$contrasts <- "T1"
df.may <- data.frame(df[,1])
df.may$repdat <- df$X.May
df.may$contrasts <- "T2"
df.june <- data.frame(df[,1])
df.june$repdat <- df$X.June
df.june$contrasts <- "T3"
df.july <- data.frame(df[,1])
df.july$repdat <- df$X.July
df.july$contrasts <- "T4"
df.transform <- rbind(df.april, df.may, df.june, df.july)
rm(df.april, df.may, df.june, df.july)

leveneTest(repdat ~ df...1.*contrasts, data=df.transform)
# it should be not significant. 

RManova <- aov(repdat~contrasts+Error(df...1.), df.transform)
summary(RManova)


anova_test(counties2)

ggplot(data = df.transform , aes(x = df...1., y = repdat)) +
  geom_point() +
  labs(x = "Date",
       y = "Total Cases",
       title = "Total Cases in Four Counties",
       subtitle = "San Francisco COVID19 Cases")



ggplot(data = counties1, aes(x = Date, y = total_cases)) +
  geom_point() +
  labs(x = "Date",
       y = "Total Cases",
       title = "Total Cases in Four Counties",
       subtitle = "San Francisco COVID19 Cases")