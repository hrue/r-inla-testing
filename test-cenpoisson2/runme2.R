n <- 300
a <- 0
b <- 1
x <- rnorm(n, sd = 0.3)
eta = a + b*x
low = sample(c(0, 1, 4, Inf), n, replace = TRUE) 
high <- low + sample(c(0, 1, 2, Inf), n, replace = TRUE) 

E = sample(1:10, n, replace=TRUE)
lambda = E*exp(eta)
y = rpois(n, lambda = lambda)

censored = which(y >= low & y <= high)
y[censored] = low[censored]
y[1] <- NA
y[2] <- NA
Y <- inla.mdata(cbind(y, low, high))
YY <- c(NA, 0, rep(NA, n-2))
Ydata <- list(Y = Y, YY = YY)

r = inla(Ydata  ~ 1 + x, 
         family = c("cenpoisson2", "gaussian"), 
         data = list(Ydata = Ydata, x = x), 
         E=E,
         debug = T,
         num.threads = "1:1",
         safe = FALSE, 
         control.predictor = list(link = 1), 
         verbose = TRUE)
summary(r)
