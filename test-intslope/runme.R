set.seed(123)
n = 5
idx = 1:n
sub = c(1, 1, 2, 2, 3)
stra = c(1, 2, 1, 2, 1)
z = 10 + 1:n
strata = sample(c("a", "b"), n, replace=TRUE)
y = rnorm(n)
r = inla(y ~ -1 + f(idx, model = "intslope",
                    args.intslope = list(subject = sub, strata = stra, covariates = z)), 
         data = list(y=y, idx=idx, sub=sub, stra=stra, z=z), 
         verbose=TRUE)
