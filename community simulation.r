
J <- 1000
init.1 <- 0.5*J
COM <- vector(length = J)
COM[1:init.1] <- 1
COM[init.1 + 1:J] <- 2
num.year <- 100
year <- 2

freq.1.vec <- vector(length = num.year)
freq.1.vec[1] <- init.1/J

for (i in 1:(J*(num.year-1))) {
  freq.1 <- sum(COM == 1)/J
  Pr.1 <- freq.1
  COM[ceiling(J*runif(1))] <- sample(c(1,2), 1, prob = c(Pr.1, 1 - Pr.1))
  
  if (i %% J == 0) {
    freq.1.vec[year] <- sum(COM == 1)/J
    year <- year +1
  }
  
}

plot(1:num.year, freq.1.vec, type = "l", xlab = "Time",
     ylab = "Frequency of species 1", ylim = c(0,1))
