# Libraries
library(pacman)
p_load(tidyverse)
p_load(ggpubr)
p_load(car)



#### 1a####
birthweight<- read.csv('birthweight.txt')
birthweight <- birthweight$birthweight
qqPlot(birthweight)
shapiro.test(birthweight)
print('Data is normal')

#### 1b####
n <- length(birthweight)
mu <- mean(birthweight)
sd <- sd(birthweight)
ci <- 1.645 * sd

lower <- mu - ci
upper <- mu + ci

print('Confidence interval')
print(lower)
print(upper)

#### 1c ####
t.test(birthweight, mu=2800, alternative = "greater")

#### 1d ####

#### 2a ####
n = 200
k = 140
p = k/n
q = 1-p

#### 2b ####
p - (2.576*sqrt(p * q/n))
p + (2.576*sqrt(p * q/n))

#### 2c ####


#### 3a+b ####
weather <- read.csv('weather.txt', sep='\t')

weather
plot(weather)
hist(weather$humidity)
hist(weather$temperature)

qqPlot(weather$humidity)
qqPlot(weather$temperature)
shapiro.test(weather$humidity)
shapiro.test(weather$temperature)

summary(weather)

#### 3c ####
n <- length(weather$humidity)
mu <- mean(weather$temperature)
sd <- sd(weather$temperature)
ci <- 1.645 * sd

lower <- mu - ci
upper <- mu + ci
lower
upper

#### 3d ####
TBD

#### 4a ####
# Testing homogeneity: the difference is a matter of design. In the test of independence, observational units are collected at random from a population and two categorical variables are observed for each unit. In the test of homogeneity, the data are collected by randomly sampling from each sub-group separately.

#### 4b ####
austen <- read.csv(file = 'austen.txt', sep = '\t')

chisq.test(austen)

#### 4c ####
