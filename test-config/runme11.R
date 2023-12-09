make.sym <- function(Q) {
    d <- diag(Q)
    Q <- Q + t(Q)
    diag(Q) <- d
    return (Q)
}

n = 100
y = rpois(n, exp(0.3 * scale(arima.sim(n, model = list(ar = 0.9)))))
jdx = 1:n
x = rnorm(n)
log.prec.init <- 0.123

formula = y ~ 1 + x + f(jdx, model="ar1",
                        hyper = list(prec = list(initial = log.prec.init, fixed = T)))
r = inla(formula, data =data.frame(y, jdx, x),
         family = "poisson", 
         control.fixed = list(prec = 1, prec.intercept = 2), 
         control.inla = list(int.strategy = "eb"), 
         control.compute = list(config = TRUE))

## check prior sd for 'jdx'
head(cbind(sqrt(diag(solve(make.sym(r$misc$configs$config[[1]]$Qprior))))[1:n],
           sqrt(1/exp(log.prec.init))))

## check posterior sd for 'jdx'
head(cbind(sqrt(diag(solve(make.sym(r$misc$configs$config[[1]]$Q))))[1:n],
           r$summary.random$jdx$sd))

## check posterior sd for the linear predictor
AA <- rbind(r$misc$configs$pA %*% r$misc$configs$A,
            r$misc$config$A)

head(cbind(sqrt(diag(AA %*% make.sym(r$misc$configs$config[[1]]$Qinv) %*% t(AA))),
           r$summary.linear.predictor$sd))


