library(dplyr)
library(ggplot2)


#Look at data
names(diamonds)
str(diamonds)

#create vector for factors
cut_vec <- unique(diamonds$cut)
#create group length for for loop
n_groups <- length(cut_vec)
#open vec
sample_mean <- rep(NA, n_groups)
#run for loop
for(i in 1:n_groups){
  rows <- which(diamonds$cut == cut_vec[i])
  observations <- diamonds$price[rows]
  sample_mean[i] <- mean(observations, na.rm = T)
}

plot(diamonds$cut, diamonds$price, col = "darkgrey")
points(x=cut_vec, y = sample_mean, col = "red")

#Look and see if the mean is truly different for Ideal vs Fair
Premium <- as.data.frame(diamonds[diamonds$cut == "Premium",])
Fair <- as.data.frame(diamonds[diamonds$cut == "Fair",])





wilcox.test(Premium$price~Fair$price)

t.test(Premium$price,Fair$price, var.equal = T)

#conclusion p value is very small, so reject Null.  The prices are truly different



