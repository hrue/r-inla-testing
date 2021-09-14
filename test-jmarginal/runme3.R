set.seed(123)
n <- 8
data <- data.frame(y = 1+rnorm(n), idx = 1:n)
r <- inla(y ~ -1 + f(idx, model = "rw2", scale.model = TRUE,
                     constr = FALSE, initial = 0, fixed = TRUE),
          data = data,
          control.family = list(hyper = list(prec = list(initial = 2, fixed = TRUE))), 
          selection = list(idx = 1:n),
          control.compute = list(config = TRUE))

constr <- list(A = matrix(1, ncol = n, nrow = 1), e = 10)
x <- inla.rjmarginal(10, r, constr = constr)

A <- matrix(rnorm(n^2), n, n)
b <- inla.tjmarginal(r, A)
b.marg <- inla.1djmarginal(b)





