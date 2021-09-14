n <- 5
na <- rep(NA, n)
Y1 <- inla.mdata(c(1:n, na, na),  1)
Y2 <- inla.surv(c(na, 1:n, na), 1)
Y3 <- c(na, na, 1:n)

intercept <- as.factor(c(rep(1, n), rep(2, n), rep(3, n)))
data <- list(YY = list(Y1, Y2, Y3),
             intercept = intercept)

r <- inla(YY ~ -1 + intercept, data = data,
          control.inla = list(diagonal = 1),
          family = c("nmix", "exponentialsurv", "poisson"))
