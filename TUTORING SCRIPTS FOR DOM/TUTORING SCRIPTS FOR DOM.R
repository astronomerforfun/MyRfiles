#Quick Tutorial on mpg data show dplyr/ggplot

#subsetting data camp passwords.  How data is usually formatted in corporate world

library(ggplot2)
library(dplyr)


#Things to go over... '$' View/symbol/str/names/summary/class/vectorization/x and y axis

str(diamonds)
summary(diamonds)
class(diamonds)
diamonds <- as.data.frame(diamonds)
class(diamonds)

hist(diamonds$price)
hist(diamonds$carat)

#subsetting
newdf <- diamonds[diamonds$carat > 2,]
hist(newdf$carat)

#quick hist
hist(diamonds$price, col = "green")
#quick plot of price and cut https://www.basic-mathematics.com/box-and-whiskers-plot.html
plot(diamonds$cut, diamonds$price, col = "blue")
#quick bar plot in ggplot
ggplot(diamonds, aes(cut)) + geom_bar()

?ggplot
#quick plot price and carat
ggplot(diamonds, aes(diamonds$price, diamonds$carat)) + geom_point()

#ok.  How about adding another data point on the chart by color
#quick bar plot
ggplot(diamonds, aes(diamonds$price, diamonds$carat, colour = diamonds$cut)) + geom_point()
       
#appears that ideals are more expensive generally than fair's.  makes sense.  

#how about we subset data for those over ten thousand

#the arrow with a dash is the equivelant to = and represents equal to

under10 <- diamonds[diamonds$price < 5000,]

#same plot but using new df

ggplot(under10, aes(under10$price, under10$carat, colour = under10$cut)) + geom_point()

#hard to read.  Hmmmm...  Well is there another way?  Maybe facet_wrap by cut 

ggplot(under10, aes(under10$price, under10$carat, colour = under10$cut)) + geom_point() + facet_wrap(~cut)

#for future building a model using a simple algorithm.

### Use Mpg data to show model


randomprobofconversion <- seq(.01,.9,.01)
randomprobofconversion

randomconversionweek1 <- sample(randomprobofconversion, 30, replace = T)
randomconversionweek2 <- sample(randomprobofconversion, 30, replace = T)
cust_100_days_salesperson1 <- sample(seq(1,15, by = 1),30, replace=T)
cust_100_days_salesperson2 <- sample(seq(1,15, by = 1),30, replace=T)


conversion_salesperson1_100_days <- cust_100_days_salesperson1*randomconversionweek1
conversion_salesperson2_100_days <- cust_100_days_salesperson2*randomconversionweek2
df <- as.data.frame(cbind(cust_100_days_salesperson1, conversion_salesperson1_100_days,cust_100_days_salesperson2, conversion_salesperson2_100_days))


plot(df$cust_100_days, df$conversion_salesperson1_100_days)
plot(df$cust_100_days, df$conversion_salesperson2_100_days)
mean(df$conversion_salesperson1_100_days)
mean(df$conversion_salesperson2_100_days)
t.test(df$conversion_salesperson1_100_days, df$conversion_salesperson2_100_days)


##This is a format for running a randomization of a variable
sample_n <- 100
n_times <- 100000
mean_n_all <- rep(NA, n_times)

for(i in 1:n_times){
  sample <- sample(df$conversion_salesperson1_100_days, sample_n, replace = T)
  mean_n_all[i]<- mean(sample)
}
hist(monte)
abline(v = mean(df$conversion_salesperson2_100_days))




