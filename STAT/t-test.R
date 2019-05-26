data <- read.csv('screen_size-data.csv')

head(data,10)


qqnorm(data$Screen_size.in.cm.)
qqline(data$Screen_size.in.cm.,col="red")
# all on line, which means in normalized 

t.test(data$Screen_size.in.cm.,mu=10)
