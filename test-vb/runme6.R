n <- 20
x <- rnorm(n, sd = 0.3)
y <- rpois(n, exp(-1 + x))

r <- inla(y ~ 1 + x, 
          family = "poisson", 
          control.inla = list(int.strategy = "eb"), 
          data = data.frame(y, idx = 1:n, x))
## this gives
## Node[0] delta[-0.000] dx/sd[-0.000] (x-mode)/sd[0.000]"                                              
## Node[3] delta[-0.000] dx/sd[-0.000] (x-mode)/sd[0.000]"                                              
## Node[6] delta[-0.000] dx/sd[-0.000] (x-mode)/sd[0.000]"                                              
## Node[50] delta[0.000] dx/sd[0.000] (x-mode)/sd[-0.044]"                                              
## Node[51] delta[0.000] dx/sd[0.000] (x-mode)/sd[-0.005]"                                              

## which is intercept, 1 (=0), 4 (=3), 7 (=6), then 'x' and the intercept.
## the inla-log-output using 0-based indexing.

rr <- inla(y ~ 1 + x, 
           family = "poisson", 
           control.inla = list(int.strategy = "eb"), 
           data = data.frame(y, idx = 1:n, x))
