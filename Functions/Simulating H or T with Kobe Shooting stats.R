####simulating binomial (fair coin)##########

outcomes <- c("H", "M")
# Simulate a fair coin 100 times with replacement (independent)
sim_fair_coin <- sample(outcomes, size = 100000, replace = TRUE)
# How many heads and tails in flipping the coin 100 times
table(sim_fair_coin)


coin <- calc_streak(sim_fair_coin)

barplot(table(coin))


Simulating the shots by Kobe Bryant
Simulating a basketball player who has independent shots uses the same mechanism that we use to simulate a coin flip. Simulate a single shot from an independent shooter with a shooting percentage of 45% (percentage of hits by Kobe).
set.seed(121213)
n = 50000
outcomes <- c("H", "M")
p <- 0.45
sim_basket <- sample(0:1, n, prob = c(1 - p, p), replace = TRUE)
s <- cumsum(sim_basket)
r <- s/(1:n)
lo <- max(c(0, p - 0.1))
hi <- min(c(1, p + 0.1))
plot(r, ylim = c(lo, hi), type = "l", main = "Number of hits made by the independent Shooter", 
     ylab = "Probability of a a hit")
lines(c(0, n), c(p, p))