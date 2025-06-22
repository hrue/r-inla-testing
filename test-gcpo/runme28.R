n <- 20
rho <- 0.9
s <- 1
x <- scale(arima.sim(n, model = list(ar = rho)))
y <- x + rnorm(n, sd = s)

S <- toeplitz(rho^(0:(n-1)))
QQ <- solve(S) + 1/s^2 * diag(n)
SS <- solve(QQ)
CC <- SS
for(i in 1:n) for(j in 1:n) CC[i, j] <- SS[i, j]/sqrt(SS[i, i] * SS[j, j])

if (FALSE) {
    y[seq(2, n, by = 2)] <- NA
    friends <- vector('list', n)
    for(i in seq(2, n-1)) {
        friends[[i]] <- c(i-1, i+1)
    }
}

r <- inla(y ~ -1 + f(idx, model = "ar1", 
                     hyper = list(prec = list(initial = 0,
                                              fixed = TRUE),
                                  rho = list(initial = inla.models()$latent$ar1$hyper$theta2$to.theta(rho), 
                                             fixed = TRUE))),
          data = data.frame(y, idx = 1:n), 
          family = "gaussian",
          control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                         fixed = TRUE))),
          control.inla = list(int.strategy = "eb"), 
          control.compute = list(control.gcpo = list(enable = TRUE,
                                                     friends = friends, 
                                                     num.level.sets = 3,
                                                     verbose = TRUE)),
          verbose = TRUE,
          safe = FALSE, 
          num.threads = "1:1")
r$gcpo$groups
