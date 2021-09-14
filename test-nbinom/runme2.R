n = 100000
lambda = 14 ## 14 gives sd = 0.1,  7 gives sd=0.2,  28 gives sd=0.05
delta = inla.pc.rgamma(n, lambda=lambda)
alpha = rgamma(n, 1/delta, 1/delta)
hist(alpha, n=300, prob=T)
