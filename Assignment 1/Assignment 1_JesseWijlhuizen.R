---
title: "Assignment 1"
author: "Jesse Wijlhuizen (13588818), group B"
date: "26 of September 2023"
output: pdf_document
---
  
setwd("~/MSc Data Science-PCmine/SSO/Assignment 1")
options(digits=3) 

## Exercise 1.1
birthweight=read.table("birthweight.txt",header=TRUE)
x=birthweight$birthweight;

####
##a)
####
summary(x)
sd(x)

##10%
quantile(x, 0.95)
quantile(x, 0.05)

##5%
quantile(x, 0.975)
quantile(x, 0.025)

##point estimate, does that mean the confidence interval as well?
hist(x)
qqnorm(x)
##although in general the plot is a flat line, there seems to be some problems at the end of the tails 

##shapiro wilk

####
##b) 
####
n=length(x)
m=mean(x);m

##2913.293

s=sd(x);print(s)
t=qt(0.95,df=n-1)
c(m-t*s/sqrt(n),m+t*s/sqrt(n)); c

##2829.202 & 2997.384 

####
##c)
####

t.test(x, mu=2800,alt="g")

##(left bound is the same, right differs --> that is why it is weird)
### 2829.202 & Inf


###d)
###
## An expert claims that the mean birthweight is bigger than 2800. H1, one-sided training --> while b is two-sided
  
## Exercise 1.2

###
###a)
###

n=200
p= 140/n
print(p)

##0.7

###
###b)
###
binom.test(140, n=200, p=0.7, conf.level=.99)
##0.6099163 0.7803014

##doen ook de formule van Matthijs

###
###c)
###

binom.test(140, n=200,p=0.75, conf.level=.99)
## P-value is 0.103. 

## Exercise 1.3

###
###a)
###  
weather=read.table("weather.txt",header=TRUE)
W_h=weather$humidity;
W_t=weather$temperature

##numerical
summary(W_h)
summary(W_t)

##graphical
par(mfrow=c(1,2)); hist(W_h); hist(W_t) # two histograms next to each other
qqnorm(W_h); qqnorm(W_t) # two QQ-plots next to each other
##boxplot

## W_h seems normal, W_t seems not --> add test for normality here.+ zorg dat we aandacht besteden aan mediaan etc omdat er dus iets niet normaal verdeeld is

###
###b)
###  

##See qqnorm above
## W_h seems normal, W_t seems not --> add test for normality here.

###
###c)
###  

alpha = 0.1


#t scores --. uit R halen?

t = qt(.1/2, df = 59); t

n <- length(weather$temperature); n
mu <- mean(weather$temperature)
sd <- sd(weather$temperature)
ci <- 1.67 * sd / sqrt(n)

lower <- mu - ci
upper <- mu + ci
lower
upper

## checken: 
t.test(weather$temperature, mu=mean(weather$temperature), conf.level=.90)
##47.48704 57.96296

##mean = 52.73
##CI = 47.49047 & 57.95953

##add Shapiro-Wilk test?

###
###d)
###  

sdW_h = sd(W_h);
EW_h = 0.02/2
alphaW_h=0.05
zW_h = qnorm(1-a/2)
print(zW_h)

NW_h=((zW_h^2)*(sdW_h^2))/(EW_h^2);
print(NW_h)
##2.408.723

## Exercise 1.4
austen=read.table("austen.txt",header=TRUE)

##A
##homogeneity

##B
##
x=as.data.frame(matrix(c(austen$Sense, austen$Emma, austen$Sand1),ncol=6,nrow=3, byrow=TRUE)); 
dimnames(x)=list(c("Sense","Emma", "Sand1"),c("a","an", "this", "that", "with", "without"))
print(x)

z=chisq.test(x); 
print(z)
##row = categories, J = samples (=column)
##X-squared = 14.274, df = 10, p-value = 0.1609
## so we cannot reject the null-hypothesis: the distribution amongst categories of row variable is the same for each column.

x=as.data.frame(matrix(c(austen$Sand1, austen$Sand2, austen$Emma, austen$Sense),ncol=6,nrow=4, byrow=TRUE)); 
dimnames(x)=list(c("Sand1","Sand2", "Emma", "Sense"),c("a","an", "this", "that", "with", "without"))
print(x)

z=chisq.test(x); 
print(z)

#####

x=as.data.frame(matrix(c(austen$Sand1, austen$Sand2),ncol=6,nrow=2, byrow=TRUE)); 
dimnames(x)=list(c("Sand1","Sand2"),c("a","an", "this", "that", "with", "without"))
print(x)

z=chisq.test(x); 
print(z)

##X-squared = 8.4993, df = 5, p-value = 0.1308

##drie tabellen

#austen_other <- austen %>% mutate(Austen = Sense + Emma + Sand1)

#cs_other <- chisq.test(austen_other %>% select(Sand2, Austen))
#cs_other

