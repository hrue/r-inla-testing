

N = 50
x1 <- rnorm(N, 0, 1)
x2 <- rnorm(N, 0, 0.5)
x3 <- rnorm(N, 0, 2)
eta  <- 1+x1+x2+x3
y<- rpois(N, lambda = exp(eta))
selection = list(Predictor=1:10)
f <- y ~ 1 + x1+x2+x3
dat <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
mod <- inla(f,
            data=dat,
            family="nbinomial",
            control.compute=list(config=TRUE),
            selection = selection)
