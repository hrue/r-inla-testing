n = 600
N <- 2*n
x = rnorm(n, sd = 1)
a <- 0.5
eta1 = 2 + x
eta2 = 2 + a*x
quantile1 = 0.8
quantile2 = 0.2
lambda1 = qgamma(1.0 - quantile1, shape = exp(eta1)+1.0, rate=1)
lambda2 = qgamma(1.0 - quantile2, shape = exp(eta2)+1.0, rate=1)
y1 = rpois(n, lambda1)
y2 = rpois(n, lambda2)
Y <- matrix(NA, N, 2)
Y[1:n, 1] <- y1
Y[n+1:n, 2] <- y2
idx1 <- c(1:n, rep(NA, n))
idx2 <- c(rep(NA, n), 1:n)

r = inla(Y ~ 1 + f(idx1, model = "iid",
                   hyper = list(prec = list(prior = "pc.prec",
                                            param = c(1, 0.01)))) +
             f(idx2, copy = "idx1",
               hyper = list(beta = list(fixed = FALSE))), 
         family = rep("poisson", 2), 
         control.family = list(
             list(control.link = list(
                     model = "quantile",
                     quantile = quantile1)),
             list(control.link = list(
                     model = "quantile",
                     quantile = quantile2))), 
         data = list(Y = Y, idx1 = idx1, idx2 = idx2),
         inla.mode = "experimental",
         control.inla = list(cmin = 0), ## needed in the beginning
         control.compute = list(dic = TRUE, waic = TRUE), 
         verbose = TRUE)
summary(r)

rr = inla(Y ~ 1 + f(idx1, model = "iid",
                   hyper = list(prec = list(prior = "pc.prec",
                                            param = c(1, 0.01)))) +
             f(idx2, model =  "iid", 
                   hyper = list(prec = list(prior = "pc.prec",
                                            param = c(1, 0.01)))), 
         family = rep("poisson", 2), 
         control.family = list(
             list(control.link = list(
                     model = "quantile",
                     quantile = quantile1)),
             list(control.link = list(
                     model = "quantile",
                     quantile = quantile2))), 
         data = list(Y = Y, idx1 = idx1, idx2 = idx2),
         inla.mode = "experimental",
         control.inla = list(cmin = 0), ## needed in the beginning
         control.compute = list(dic = TRUE, waic = TRUE), 
         verbose = TRUE)
summary(r)
