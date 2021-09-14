theta = -log(0.95)
sigma = 1
marg.prec = 2*theta/sigma^2

nrep=10
n = 100
N = n * nrep

locations = cumsum(sample(c(1, 5, 100), n, replace=TRUE))

x = numeric(N)
for(r in 0:(nrep-1)) {
    k = r*n
    x[k] = rnorm(1, mean=0, sd = sqrt(1/marg.prec))

    for(i in 2:n) {
        delta = locations[i] - locations[i-1]
        x[i + k] = x[i-1 + k] * exp(-theta * delta) +
            rnorm(1, mean=0,  sd = sqrt(1/marg.prec * (1-exp(-2*theta*delta))))
    }
}

replicate = rep(1:nrep,  each = n)

for(r in 1:nrep) {
    k = (r-1)*n+1
    dev.new()
    plot(locations, x[k:(k+n-1)], type="l")
}


y=x

formula = y ~ -1 + f(locations, model="ou", values=locations, replicate =replicate)
r = inla(formula, data = data.frame(y, locations, replicate), control.family = list(initial=10, fixed=FALSE), verbose=TRUE)


