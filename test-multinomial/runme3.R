## comparing multinomial with logistic regression

n <- 3000
x <- rnorm(n)
eta <- 1 + 0.2 * x
p <- 1/(1+exp(-eta))
y <- rbinom(n, prob = p, size = 1)
y[n] <- NA

r <- inla(y ~ 1 + x,
          control.fixed = list(prec = 0, prec.intercept = 0), 
          data = data.frame(y, x),
          family = "binomial",
          control.predictor = list(compute = TRUE, link = 1))

yy <- numeric(2*n)
xx <- numeric(2*n)
kk <- numeric(2*n)
intercept <- numeric(2*n)

for(i in 1:n) {
    ii <- 2*(i-1)
    yy[ii + 1] <- (y[i] == 0) 
    yy[ii + 2] <- (y[i] == 1)
    xx[ii + 1:2] <- c(0, x[i])
    kk[ii + 1:2] <- i
    intercept[ii + 1:2] <- c(0, 1)
}

## for predicting y[n], we need to remove the group spesific intercept when we use the plug-in
## estimate below
kk[c(2*n-1, 2*n)] <- NA

rr <- inla(yy ~ -1 + intercept + xx + f(kk, model = "iid",
                            hyper = list(prec = list(initial = -16,
                                                     fixed = TRUE))), 
           control.fixed = list(prec = 0), 
           data = data.frame(yy, xx, kk, intercept), 
           family = "poisson",
           control.predictor = list(compute = TRUE, link = 1))
summary(r)
summary(rr)

## predictions, using simple plug-in. better to use 'inla.posterior.sample' but it is more
## involved
p11 <- r$summary.fitted.values[n,"mode"]
p12 <- 1/(1+exp(-r$summary.linear.predictor[n,"mode"]))
p21 <- rr$summary.fitted.values[2*n, "mode"] /
    sum(rr$summary.fitted.values[c(2*n-1, 2*n), "mode"])
p22 <- exp(rr$summary.linear.predictor[2*n, "mode"]) /
    sum(exp(rr$summary.linear.predictor[c(2*n-1, 2*n), "mode"]))

round(dig = 3, c(binomial = c(p11, p12), poisson = c(p21, p22)))
