

###Diamonds Color Mean Price Analysis
color_vec <- unique(diamonds$color)
n_groups <- length(color_vec) #forloop
sample_mean <- rep(NA, n_groups)#empty vec

for(i in 1:n_groups){
  rows <- which(diamonds$color == color_vec[i])#row extraction
  observations <- diamonds$price[rows]#subset
  sample_mean[i] <- mean(observations)#run mean
}

colorf <- diamonds[diamonds$color == "F",]
colorj <- diamonds[diamonds$color == "J",]

plot(diamonds$color, diamonds$price)
points(x = color_vec, y = sample_mean)
t.test(colorf$price, colorj$price)
