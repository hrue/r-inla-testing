n <- 300
N <- 4*n
y <- rpois(n, exp(2))

idx <- 1:n
idx2 <- 1:n

r <- inla(y ~ 1 + f(idx, vb.correct = T, initial = log(1), fixed = T) + f(idx2, copy = "idx"),
          data = data.frame(y, idx, idx2),
          control.inla = list(control.vb = list(f.enable.limit = c(N, N, N, N)),
                              int.strategy = "eb"), 
          family = "poisson",
          verbose = TRUE)
rr <- inla(y ~ 1 + f(idx, vb.correct = T, initial = log(1/4), fixed = T), 
           data = data.frame(y, idx, idx2),
           control.inla = list(control.vb = list(f.enable.limit = c(N, N, N, N)),
                               int.strategy = "eb"), 
           family = "poisson",
           verbose = TRUE)
