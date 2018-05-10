# library(devtools)
# devtools::install_github("statswithr/statsr")

#Script which simulates a baseketball players shooting and then uses a function to
#calculate streaks. 




#create a vector of Hits and Misses, must be "H" or "M" for calc_streak function.
outcomes <- data.frame(c("H", "M"))
#create a simulation of 200 shots using dplyr's sample_n
test_shots <- sample_n(outcomes, 100000, replace = T, weight = c(.45, .55))

#look at table
table(test_shots)

#pass df to calc_streak()
streak <- calc_streak(test_shots)
#Look at table
table(streak)
barplot(streak$length)

#compare to another random sim
random <- sample_n(outcomes, 100000, replace = T)

table(random)
streak2 <- calc_streak(random)



#function
calc_streak = function(x){
  if (!is.atomic(x))
    x = x[,1]
  if (any(!x %in% c("H","M")))
    stop('Input should only contain hits ("H") and misses ("M")')
  y = rep(0,length(x))
  y[x == "H"] = 1
  y = c(0, y, 0)
  wz = which(y == 0)
  streak = diff(wz) - 1
  return(data.frame(length = streak))
}

x <- rep(NA, 1000)
for(i in 1:1000){
  sample <- sample_n(mpg, 1000, replace = T)
  x[i] <- mean(sample$hwy)
}


y <- sample_n(mpg, 1000, replace = T)
