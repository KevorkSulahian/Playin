library(ggplot2)
library(gridExtra)
library(FSA)
library(multcomp)
library(car)
library(fitdistrplus)
  
credit <- read.csv("Credit.csv", stringsAsFactors = F)
View(credit)

attach(credit)

ggplot(credit, aes(age, fill = default)) +geom_histogram(bins = 20)

t.test(age, mu = 49,alternative = 'two.sided')

countries <- read.csv("Countries.csv", stringsAsFactors = F)

attach(countries)

Summarize(data=countries, Unemployment ~ Region, digits = 2)
  
library(ggplot2)
ggplot(countries, aes(Region,Unemployment)) +geom_boxplot()

t.test(countries, Unemployment ~ Region)

Summarize(countries, Business.Freedom~Region, digits = 2)
