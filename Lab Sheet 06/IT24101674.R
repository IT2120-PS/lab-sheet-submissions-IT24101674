setwd("C:\\Users\\User\\OneDrive\\Desktop\\SLIIT Modules\\PS\\LabSheet 06")
getwd()

#01

#Given: n = 50, p = 0.85, X ~ Binomial(n = 50, p = 0.85)
#X ∼ Binomial(50,0.85)

n <- 50
p <- 0.85

prob_X <- 1 - pbinom(46, size=n, prob=p)
prob_X

#02
#Random variable(X): Number of calls received in 1hr.

#Distribution of X: X ~ Poisson(12)


lamda <- 12

probability_X <- dpois(15, lamda)
probability_X
