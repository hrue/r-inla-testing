n = 100
s = 0.01
lambda = 1
x = rexp(n, rate = lambda)
y = 1 + x + rnorm(n, sd=s)

y[1] = NA   ## just to check the marginal for the first exp...
idx = 1:n
Y = matrix(NA, 2*n, 2)
Y[idx, 1] = y
Y[idx+n, 2] = 0 ## any any other numerical dummy value
intercept = c(rep(1, n), rep(0, n))
idx2 = c(idx, idx)
formula = Y ~ -1 + intercept + 
    f(idx2, model="iid", hyper = list(prec = list(initial = -5, fixed=TRUE)))

r = inla(formula, data = list(Y=Y, idx2 = idx2, intercept=intercept),
        verbose=TRUE,
        family = c("gaussian", "iidgamma"),
        control.family = list(
            list(hyper = list(
                     prec = list(
                         initial = log(1/s^2),
                         fixed=TRUE
                     )
                 )
                 ), 
            list(hyper = list(
                     shape = list(
                         initial = 1,
                         fixed=TRUE
                     ),
                     rate = list(
                         initial = log(lambda),
                         fixed = FALSE
                     )
                 )
                 )
        ),
        control.predictor = list(compute = TRUE, initial = 10)
        )

