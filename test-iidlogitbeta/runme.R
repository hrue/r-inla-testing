inla.my.update(b=T)

n=100L
a=5
b=1

p = rbeta(n, a, b)
Ntrials= sample(1L:10L, n, replace=TRUE)
y = rbinom(n, Ntrials, p)

idx = c(1L:n, 1L:n)

formula = Y ~ -1 + f(idx, model="iid", hyper = list(prec = list(initial = -10, fixed=TRUE)))

Y = matrix(NA, 2L*n, 2L)
Y[1L:n, 1L] = y
Y[1L:n + n, 2L] = 0  ## any number will do
Ntrials = c(Ntrials, rep(NA, n))

r = inla(formula, data = list(Y=Y, idx=idx),
        family = c("binomial", "iidlogitbeta"),
        control.data = list(list(), list(hyper = list(a = list(initial = log(a)),  b = list(initial = log(b))))), 
        verbose=TRUE,
        Ntrials = Ntrials)
