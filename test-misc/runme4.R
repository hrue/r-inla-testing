n <- 20
x <- rnorm(n)
eta <- 1 + 0.2 * x
y <- rnorm(n, mean = eta, sd = 1)

inla.setOption(inla.call = NULL)
r <- inla(y ~ 1 + x,
          data = data.frame(y, x),
          ##family = "t",
          ##control.family = list(hyper = list(dof = list(initial = 10, fixed = TRUE))), 
          control.inla = list(cmin = 0, control.vb = list(strategy = "variance")), 
          inla.mode = "compact")

rr <- inla(y ~ 1 + x,
           data = data.frame(y, x),
           ##family = "t",
           ##control.family = list(hyper = list(dof = list(initial = 10, fixed = TRUE))), 
           control.inla = list(cmin = 0, strategy = "gaussian"), 
           inla.mode = "classic")

rrr <- inla(y ~ 1 + x,
            data = data.frame(y, x),
            ##family = "t",
            ##control.family = list(hyper = list(dof = list(initial = 10, fixed = TRUE))), 
            control.inla = list(cmin = 0, strategy = "laplace"), 
            inla.mode = "classic")

inla.setOption(inla.call = "inla.mkl.work")
nr <- inla(y ~ 1 + x,
          data = data.frame(y, x),
          ##family = "t",
          ##control.family = list(hyper = list(dof = list(initial = 10, fixed = TRUE))), 
          control.inla = list(cmin = 0, control.vb = list(strategy = "variance")), 
          inla.mode = "compact")

nrr <- inla(y ~ 1 + x,
           data = data.frame(y, x),
           ##family = "t",
           ##control.family = list(hyper = list(dof = list(initial = 10, fixed = TRUE))), 
           control.inla = list(cmin = 0, strategy = "gaussian"), 
           inla.mode = "classic")

nrrr <- inla(y ~ 1 + x,
            data = data.frame(y, x),
            ##family = "t",
            ##control.family = list(hyper = list(dof = list(initial = 10, fixed = TRUE))), 
            control.inla = list(cmin = 0, strategy = "laplace"), 
            inla.mode = "classic")

r$mlik - rr$mlik
rr$mlik - rrr$mlik
