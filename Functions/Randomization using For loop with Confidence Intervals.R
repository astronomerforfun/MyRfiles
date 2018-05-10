library(ggplot2)
library(dplyr)
library(reshape)
library(gapminder)


mean(mpg$hwy)
###Randomization#######################
#Create a sample size Vector
sample_size <- 30
#Create a repeat vector for iteration which is used in the for loop
n_rep <- 100000
#Empty Vector to store results
s_means <- rep(NA, n_rep)


#write for loop, take sample and then run mean on each
for(i in 1:n_rep){
  sample <- sample(mpg$hwy, sample_size)
  s_means[i] <- mean(sample)
}

hist(s_means)
abline(v = mean(mpg$hwy), col = "red")
hist(mpg$hwy)
abline(v = mean(mpg$hwy), col = "red")

####Running a mean with CI levels for mpg$hwy and mpg$cyl using for loop############

cyls <- unique(mpg$cyl)
n_groups <- length(cyls)
sample_mean <- rep(NA, n_groups)
cis <- matrix(nrow= n_groups, ncol=2)

for(i in 1:n_groups){
  #sample 

  #extract relevant data
  rows <- which(sample$cyl == cyls[i])
  observations <- mpg$hwy[rows]

  #Store Sample mean 
  sample_mean[i] <- mean(observations)

  #Construct CI
  stdev <- sd(observations)
  n <- length(observations)
  se_mean <- stdev/sqrt(n)
  
  
  #Store CI 
  cis[i,1] <- sample_mean[i] -2 * se_mean #formula for 
  cis[i,2] <- sample_mean[i]+2 * se_mean
}

cis

plot(mpg$cyl, mpg$hwy, col = "darkgrey")
points(x=cyls, y = sample_mean, col = "red")
segments(x0=cyls, x1 = cyls, y0=cis[,1], y1= cis[,2])

