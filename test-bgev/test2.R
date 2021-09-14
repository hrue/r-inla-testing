y = 2
sigma = 2
xi = 0.2
mu = 1

dens = function(y, mu, sigma, xi) {
    tx = (1+xi*(y-mu)/sigma)^(-1/xi)
    -log(sigma) + (xi+1)*log(tx) -tx
}

xi = 1
mu = seq(0, 5, len=100)
plot(mu, dens(y, mu, sigma, xi))


xis = seq(0.001, 1,  len=100)
plot(log(xis),  dens(y, mu, sigma, xis), type="l")

s = seq(0.01, 10,  len=100)
plot(log(s),  dens(y, mu, sigma=s, xi), type="l")



    
