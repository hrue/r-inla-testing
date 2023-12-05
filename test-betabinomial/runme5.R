rho = 0.1
n = 30
z = rnorm(n, sd=0.2)
Ntrials = sample(10:20, n, replace=TRUE)
eta = 1 + z
p.eta = exp(eta)/(1+exp(eta))
a = p.eta * (1-rho)/rho
b = (p.eta * rho - p.eta - rho + 1)/rho
p = rbeta(n, a, b)
y = rbinom(n, Ntrials, p)
s = 1

jw.diag <- function(x) {
    n <- length(x)
    A <- matrix(NA, nrow = n, ncol = n)
    diag(A) <- x
    return (A)
}

formula = y ~ 1 + z
data = data.frame(y, z)
r = inla(formula,
         data = data,
         family = "betabinomial",
         Ntrials=Ntrials, verbose=TRUE)

formula = jw.diag(y) ~ 1 + z
data = data.frame(y, z)
rr = inla(formula,
         data = data,
         family = rep("betabinomial", length(y)),
         Ntrials=Ntrials,
         verbose=TRUE)

summary(r)
summary(rr)

