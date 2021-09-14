lambda = 7
n = 10^6
delta = inla.pc.rgamma(n, lambda = lambda)
s = rgamma(n, shape = 1/delta,  rate = 1/delta)
hist(s,  n = 100)
