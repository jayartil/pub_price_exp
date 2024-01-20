library(tidyverse)
library(aod)
library(ggpubr)
library(performance)
library(oddsratio)
library(ggplot2)

options("scipen"=100, "digits"=4)

#for all 380 quotes
df <- read.csv(file="3DP-Logit-Flex-NoDealer.csv",
               header=TRUE,fileEncoding = "UTF-8-BOM",sep=",")

# for high normalized value scores, gross >= 75th Percentile, Net >= 75th Percentile
df <- read.csv(file="3DP-Logit-Flex-NoDealer-75-Plus-Norm.csv",
               header=TRUE,fileEncoding = "UTF-8-BOM",sep=",")

df[, 1]  <- as.numeric(df[, 1])
df[, 2]  <- as.numeric(df[, 2])
df[, 3]  <- as.numeric(df[, 3])
df[, 4]  <- as.numeric(df[, 4])
df[, 5]  <- as.numeric(df[, 5])
df[, 6]  <- as.numeric(df[, 6])
df[, 7]  <- as.numeric(df[, 7])

head(df,4)

summary(df)

sapply(df,sd)

xtabs(~extwarramt, data = df)

# Do One Predictor Variable, One at a time

# PVD must be handled differently, only include quotes irrespective of outcomes where we sold 3DP with DEXA, Trident, or APBS.

mylogit <- glm(outcome ~ gross, data = df, family = "binomial")

mylogit <- glm(outcome ~ extwarramt, data = df, family = "binomial")

mylogit <- glm(outcome ~ tradeamt, data = df, family = "binomial")

mylogit <- glm(outcome ~ flexdisc, data = df, family = "binomial")

mylogit <- glm(outcome ~ covidamt, data = df, family = "binomial")

mylogit <- glm(outcome ~ pvdamt, data = df, family = "binomial")

mylogit <- glm(outcome ~ gross + extwarramt + tradeamt + pvdamt + flexdisc + 
                 covidamt, data = df, family = "binomial")

# As per above, Only Ext Warranty and Flexible Discount turned out to be significant, run logit using these two predictors

#mylogit <- glm(outcome ~ extwarramt + flexdisc + extwarramt*flexdisc, data = df, family = "binomial")
#fit_glm <- glm(outcome ~ extwarramt + flexdisc, data = df, family = "binomial")

mylogit <- glm(outcome ~ gross, data = df, family = "binomial")
fit_glm <- glm(outcome ~ gross, data = df, family = "binomial")


#or_glm(data = df, model = fit_glm, 
#       incr = list(extwarramt=0, flexdisc=-1000))

or_glm(data = df, model = fit_glm, 
       incr = list(gross = 10000))

summary(mylogit)
#sprintf("%.4f", exp(0.0000493))

confint.default(mylogit)


#use prediction on significant factors
#1. Flexible Discount
flexdisclogit <- glm(outcome ~ flexdisc , data = df, family = "binomial")
summary(flexdisclogit)
range(df$flexdisc)

mean(df$flexdisc)
median(df$flexdisc)

xflexdisc <- seq(0,50000,1)
#prediction peice
yflexdisc <- predict(flexdisclogit,list(flexdisc = xflexdisc), type = "response")
plot(df$flexdisc, df$outcome, pch=16, xlab = "Flex Disc ($)", ylab = "Outcome")
#lines(xflexdisc,yflexdisc)


#2. Ext Warranty Discount
extwarrlogit = glm(outcome ~ extwarramt , data = df, family = "binomial")
summary(flexdisclogit)
range(df$extwarramt)

mean(df$extwarramt)
median(df$extwarramt)

xextwarr <- seq(0,50000,1)
#prediction peice
yextwarr <- predict(extwarrlogit,list(extwarramt = xextwarr), type = "response")
plot(df$extwarramt, df$outcome, pch=16, xlab = "Ext Warr ($)", ylab = "Outcome")
#lines(xflexdisc,yflexdisc)

