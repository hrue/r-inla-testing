n <- 300
idx <- 1:n
x <- arima.sim(n, mode = list(ar = 0.9))
s <- 0.1
y <- 0 + x + rnorm(n, sd = s)

r <- inla(y ~ -1 + f(idx, model = "ar1"),
          data = data.frame(y, idx),
          control.predictor = list(compute = TRUE), 
          control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))))

## leave out blocks of size at the time
m <- 20
k <- n %/% m
y.pred <- numeric(n)

for(i in 1:k) {

    y.new <- y
    y.new[1:m + (i-1)*m] <- NA

    r.block <- inla(y ~ -1 + f(idx, model = "ar1"),
                    data = data.frame(y = y.new, idx),
                    control.predictor = list(compute = TRUE), 
                    control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))), 
                    ## this is the trick. you can also set restart to FALSE in case you think
                    ## the hyperparameters does not change. 
                    control.mode = list(result = r, restart = TRUE))
    ii <- which(is.na(y.new))
    y.pred[ii] <- r.block$summary.linear.predictor$mean[ii]
    print(ii)
}
plot(sort(y), sort(y.pred))
abline(a = 0, b = 1)

