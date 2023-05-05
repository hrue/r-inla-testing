INLA:::inla.my.update(b = TRUE)
N <- 15
n <- 300
s <- .2
phi <- 0.9
x <- scale(arima.sim(n, model = list(ar = phi)))
beta <- sqrt(1:n)
beta <- beta / max(beta)

eta <- beta * x
y <- c(1 + x + rnorm(n, sd = s),
       2 + beta * x + rnorm(n, sd = s))
       
       

idx <- c(1:n, rep(NA, n))
idxx <- c(rep(NA, n), 1:n)
intercept <- as.factor(rep(1:2, each = n))

to.theta <- function(x)  log((1 + x)/(1 - x))

r <- inla(
    y ~ -1 +
        intercept + 
        f(idx, model = "ar1", values = 1:n, 
          hyper = list(prec = list(initial = 0, fixed = TRUE),
                       rho = list(initial =  to.theta(phi), fixed = TRUE))) + 
        f(idxx, scopy = "idx", control.scopy = list(n = N,
                                                    prec.betas = 10, 
                                                    covariate = 1:n)), 
    data = data.frame(y, idx, idxx, intercept), 
    control.family = list(
        hyper = list(
            prec = list(initial = log(1/s^2),
                        fixed = TRUE))),
    verbose = TRUE,
    safe = FALSE)

beta.est <- r$summary.hyperpar[1:N, "mean"]
plot(1:n, beta, type = "l",  col = "blue", lwd = 3, ylim = c(0, 1))
fun <- splinefun(seq(1, n, len = length(beta.est)), beta.est)
lines(1:n, fun(1:n), col = "red", lwd = 5)

