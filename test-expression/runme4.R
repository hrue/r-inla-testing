n=100
y = rnorm(n)
loggamma = "expression:
            return(THETA0 + exp(THETA1));"

hyper.new = list(prec = list(prior = loggamma))
r.new = inla(y ~ f(idx),  data = data.frame(y, idx=1:n), 
        control.family = list(hyper = hyper.new), keep=TRUE, verbose=T)
