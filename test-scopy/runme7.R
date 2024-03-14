if (!FALSE) {
    inla.setOption(smtp = 'taucs', safe = FALSE, num.threads = "1:1")
}

INLA:::inla.my.update(b = T)

N <- 200
s <- 0.1
x <- 1:N
eta <- 1 + x / N + sin(x * 0.1) * exp(-2*x/N) 
y <- eta + rnorm(N, sd = s)
m <- 11

Y <- matrix(NA, N + 1, 2)
Y[1:N, 1] <- y
Y[N+1, 2] <- mean(y)
r1 <- inla(Y ~ -1 +
              ## this model will just define the 'overall level',
              ## but with one value for each i.
              ## We need this as as can then scale this one with
              ## the spline. We add a point with
              ## the second likelihood to lock-it in place
              f(idx,
                model = "rw1",
                scale.model = TRUE,
                constr = FALSE,
                values = 1:N,
                hyper = list(prec = list(initial = 15,
                                         fixed = TRUE))) +
              ## the 'overall level' is scaled by a spline
              f(idx.scopy, scopy = "idx",
                hyper = list(mean = list(param = c(1, 0.1)),
                             slope = list(initial = 0, fixed = TRUE, param = c(0, 0.1)),
                             spline = list(initial = 0, fixed = TRUE, param = c(0, 20))), 
                control.scopy = list(covariate = x, n = m)), 
          ##
          data = list(idx = c(rep(NA, N), 1), 
                      idx.scopy = c(1:N, NA), 
                      x = c(x,1), 
                      m = 2),
          family = c("gaussian", "gaussian"), 
          control.family = list(
              list(hyper =
                       list(prec = list(
                                initial = log(1/s^2),
                                fixed = TRUE))),
              list(hyper =
                       list(prec = list(
                                initial = 15, 
                                fixed = TRUE)))), 
          control.compute = list(config = TRUE, residuals = TRUE))

idx.start <- which(rownames(r1$summary.hyperpar) == "Beta0 for idx.scopy (scopy mean)")
theta <- r1$mode$theta
mean.value <- theta[idx.start]
theta[idx.start] <- 0 ## now the slope

r2 <- inla(Y ~ -1 +
              ## this model will just define the 'overall level',
              ## but with one value for each i.
              ## We need this as as can then scale this one with
              ## the spline. We add a point with
              ## the second likelihood to lock-it in place
              f(idx,
                model = "rw1",
                scale.model = TRUE,
                constr = FALSE,
                values = 1:N,
                hyper = list(prec = list(initial = 15,
                                         fixed = TRUE))) +
              ## the 'overall level' is scaled by a spline
              f(idx.scopy, scopy = "idx",
                hyper = list(mean = list(initial = mean.value, fixed = TRUE), 
                             slope = list(param = c(0, 0.1)),
                             spline = list(param = c(0, 20))), 
                control.scopy = list(covariate = x, n = m)), 
          ##
          data = list(idx = c(rep(NA, N), 1), 
                      idx.scopy = c(1:N, NA), 
                      x = c(x,1), 
                      m = 2),
          family = c("gaussian", "gaussian"), 
          control.family = list(
              list(hyper =
                       list(prec = list(
                                initial = log(1/s^2),
                                fixed = TRUE))),
              list(hyper =
                       list(prec = list(
                                initial = 15, 
                                fixed = TRUE)))), 
          control.compute = list(config = TRUE, residuals = TRUE),
          control.mode = list(result = r1, restart = TRUE))

theta <- r2$mode$theta
slope.value <- theta[idx.start]

theta.before <- theta[seq_len(idx.start-1)]
theta.spline <- rep(0, m-2)
theta.after <- theta[idx.start + seq_len(length(theta) - idx.start)]
r2$mode$theta <- c(theta.before, theta.spline, theta.after)

r3 <- inla(Y ~ -1 +
              ## this model will just define the 'overall level',
              ## but with one value for each i.
              ## We need this as as can then scale this one with
              ## the spline. We add a point with
              ## the second likelihood to lock-it in place
              f(idx,
                model = "rw1",
                scale.model = TRUE,
                constr = FALSE,
                values = 1:N,
                hyper = list(prec = list(initial = 15,
                                         fixed = TRUE))) +
              ## the 'overall level' is scaled by a spline
              f(idx.scopy, scopy = "idx",
                hyper = list(mean = list(initial = mean.value, fixed = TRUE), 
                             slope = list(initial = slope.value, fixed = TRUE), 
                             spline = list(param = c(0, 20))), 
                control.scopy = list(covariate = x, n = m)), 
          ##
          data = list(idx = c(rep(NA, N), 1), 
                      idx.scopy = c(1:N, NA), 
                      x = c(x,1), 
                      m = m),
          family = c("gaussian", "gaussian"), 
          control.family = list(
              list(hyper =
                       list(prec = list(
                                initial = log(1/s^2),
                                fixed = TRUE))),
              list(hyper =
                       list(prec = list(
                                initial = 15, 
                                fixed = TRUE)))), 
          control.compute = list(config = TRUE, residuals = TRUE),
          control.mode = list(result = r2, restart = TRUE))



plot(x, y)
s.mean <- mean(r1$summary.random$idx$mean)

beta <- inla.scopy.summary(r1, "idx.scopy", range = c(1, N))
lines(beta$x, s.mean * (beta$mean), lwd = 3, col = "blue")

s.mean <- mean(r2$summary.random$idx$mean)
beta <- inla.scopy.summary(r2, "idx.scopy", range = c(1, N), mean.value = mean.value)
lines(beta$x, s.mean * (beta$mean), lwd = 3, col = "red")

s.mean <- mean(r3$summary.random$idx$mean)
beta <- inla.scopy.summary(r3, "idx.scopy", range = c(1, N),
                           mean.value = mean.value, slope.value = slope.value)
lines(beta$x, s.mean * (beta$mean), lwd = 3, col = "black")
