n <- 10
x <- rnorm(n)
X <- cbind(x)
beta_true = c(2)
 
eta_true = X%*%beta_true
y.orig <- y <- rpois(n, lambda = exp(eta_true))
 
W <- diag(n)
formula =  y ~ -1 + x
 
lc = inla.make.lincombs(x = X[,1])
 
mod_inla = inla(formula, data = data.frame(y = y.orig, X),
                family = "poisson",
                lincomb = lc,
                keep = T, 
                control.inla = list(lincomb.derived.correlation.matrix = TRUE)
                )
mat = mod_inla$misc$lincomb.derived.covariance.matrix
 
plot(mod_inla$summary.linear.predictor$mean)
lines(mod_inla$summary.lincomb.derived$mean)
cbind(mod_inla$summary.linear.predictor$sd, sqrt(diag(mat)))

