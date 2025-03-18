n <- 3000
x <- rnorm(n,  sd = 0.2)
eta <- 1 + x
E <- runif(n, min = 0.1, max=10)

mu <- E * exp(eta)
size <- 3
prob <- size/(size + mu)
Y <- rnbinom(n, size, mu=mu)

r <- inla(Y ~ 1 + x,
         data = data.frame(Y, x, E),
         family = "nbinomial",
         E=E,
         ##control.predictor = list(link = 1), 
         verbose = T)
summary(r)
plot(E * r$summary.fitted.values$mean, Y)
abline(a=0,b=1, lwd = 3, col = "blue")
