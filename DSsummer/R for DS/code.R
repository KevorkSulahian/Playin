my_numeric <- 42
my_char <- "uni"
my_logic <- F

num_vec <- c(1,2,3)
char_vec <- c("a","b","c")


# please 
numeric_vector <- c(1,2,3)
character_vector <- c("a","b","c")
boolean_vector <- c(T,F,T)

# pretty please

x = 42
x
my_apples <- 5
my_oranges <- 6
my_fruit <- my_apples + my_oranges
my_character <- "universe"
my_logical <- F


# FFS

days_vec <- c("M","T","W","T","F")
roullete_vector <- c(-24,-50,100,-350,10)
names(roullete_vector) <- days_vec
sum(roullete_vector)

roullete_vector > 0
roullete_vector[roullete_vector > 0]


# L2P

new_hope <- c(460,314)
empire_strikes <- c(290,247)
return_jedi <- c(309, 156)

box_office <- matrix(c(new_hope,empire_strikes,return_jedi),nrow = 3, byrow = T)


sex_vecotr <- c("Male","Female","Female","Male")

sex_factor <- as.factor(x = sex_vecotr)
sex_factor

data("mtcars")
##


# Definition of vectors
name <- c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune")
type <- c("Terrestrial planet", "Terrestrial planet", "Terrestrial planet",           "Terrestrial planet", "Gas giant", "Gas giant", "Gas giant", "Gas giant")
diameter <- c(0.382, 0.949, 1, 0.532, 11.209, 9.449, 4.007, 3.883)
rotation <- c(58.64, -243.02, 1, 1.03, 0.41, 0.43, -0.72, 0.67)
rings <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)

# Create a data frame from the vectors
planets_df <- data.frame(name, type, diameter, rotation, rings)

str(planets_df)

planets_df[planets_df$diameter < 1,]

a <- c(100,1,10)
order(a)
a[order(a)]

linkedin <- c(16,9,13,5,2,17,14)
facebook <- c(17,7,5,16,8,13,14)

which(linkedin > 15)

which(linkedin <= 5) 

which(linkedin > facebook)

mx <- matrix(c(linkedin,facebook), nrow=2, byrow=T)


medium = 'Linkedin'
num_views <- 14

if (medium == 'Linkedin') {
  print('YEET')
}
  
linkedin <- c(16, 9, 13, 5, NA, 17, 14)
facebook <- c(17, NA, 5, 16, 8, 13, 14)

mean(abs((facebook - linkedin)), na.rm = T)


func <- function(yo) {
  return( yo^2 )
}
func(12)

hello <- function() {
  return('how have the turntables')
}

nyc <- list(pop = 8405837,
            boroughs = c('Manhatan', 'bronx'),
            capital = F)
lapply(nyc, class)

sapply(nyc, class)

pioneers <- c("GAUSS:1777", "BAYES:1702", "PASCAL:1623", "PEARSON:1857")

split_math = str_split(string = pioneers,pattern = ':',simplify = T)

tolower(split_math[,1])

sapply(split_math, tolower)
  
temp <- list(c(3,7,9,6-1),c(6,9,12,13,5),c(4,8,3-1,-3),c(1,4,7,2,-2),c(5,7,9,4,2),c(-3,5,8,9,4),c(3,6,9,4,1))

sapply(temp, min)
sapply(temp, max)
lapply(temp, min)
lapply(temp, min)

extremes_avg <- function(x) {
  return((min(x) + max(x)) / 2)
}
sapply(temp, extremes_avg)


## Day 2

library(gapminder)
library(dplyr)

data(gapminder)

gapminder %>%
  filter(year == 1952, country == 'Syria')

gapminder %>%
  arrange(desc(continent))

gapminder %>%
  arrange(lifeExp)

gapminder %>%
  arrange(desc(lifeExp))

gapminder %>%
  filter(year == "1957") %>%
  arrange(pop)

gapminder %>%
  filter(year == 2007) %>%
  mutate(lifeExpMonths = lifeExp * 12) %>%
  arrange(desc(lifeExpMonths))

library(ggplot2)

gapminder_2007 <-gapminder %>%
  filter(year==2007)

ggplot(gapminder_2007, aes(gdpPercap, lifeExp, color = lifeExp)) + geom_point()

yeet <- gapminder %>%
  filter(year == 1952)

ggplot(yeet,aes(pop,gdpPercap, color= continent)) + geom_point() +
  scale_x_log10() + facet_wrap(~ continent)

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +  
  geom_point() +  
  scale_x_log10() +  
  facet_wrap(~ year)


gapminder %>%  
  summarize(medianLifeExp = median(lifeExp))


gapminder %>%
  group_by(continent) %>%
  filter(year == 1957) %>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))

by_year <- gapminder %>%  
  group_by(year) %>%  
  summarize(medianLifeExp = median(lifeExp),            
            maxGdpPercap = max(gdpPercap))


ggplot(by_year, aes(x = year, y = medianLifeExp)) +  
  geom_point() +  
  expand_limits(y = 0)

by_year_continent <- gapminder %>%  
  group_by(continent, year) %>%  
  summarize(medianGdpPercap = median(gdpPercap))


ggplot(by_year_continent, 
       aes(x = year, y = medianGdpPercap , color = continent)) +  
  geom_point() +  
  expand_limits(y = 0)
