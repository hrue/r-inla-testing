INLA:::inla.my.update(b = T)

mat <- matrix(c("a*2", "-3*b", ".2*x",
                "0.213 * d", "e * -2.34",  "   2.2 ",
                "x", "", "-1"),
              nrow = 3, ncol = 3)

n <- 100
y <- rnorm(n)

r <- inla(y ~ 1,
          data = list(y = y),
          family = "sem",
          control.family = list(control.sem = list(B = mat)),
          safe = FALSE,
          verbose = TRUE,
          num.threads = "1")

