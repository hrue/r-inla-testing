n <- 10
x <- rnorm(n)
y <- 1 + x + rt(n, df = 3)

r <- inla(y ~ 1+ x, 
          data = data.frame(y, x, idx = 1:n),
          family = "t",
          control.family = list(hyper = list(dof = list(initial = -4))), 
          verbose=TRUE)

