Y <- matrix(NA, 3, 3)
diag(Y) <- 1:3
r <- inla(Y ~ 1,
          data = list(Y = Y),
          family = rep("poisson", 3),
          verbose = TRUE)

