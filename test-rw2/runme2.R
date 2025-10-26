n <- 20
y <- 10*sin(seq(0, 2*pi, len = n)) + rnorm(n)

r <- inla(y ~ -1 + f(idx, model = "rw2", cyclic = TRUE,
                     values = 1:n, constr = FALSE), 
          data = data.frame(y, idx = 1:n),
          family = "stdnormal")

plot(r$summary.random$idx$mean)

## this will not work, as the model is defined on 1:n via 'values' and idx contains values not
## in that set
r <- inla(y ~ -1 + f(idx, model = "rw2", cyclic = TRUE,
                     values = 1:n, constr = FALSE), 
          data = data.frame(y, idx = n + 1:n),
          family = "stdnormal")

## this will work, as the 'values are taken from 'idx' and the values for where the RW2 is
## defined is taken from 'idx',  as unique(sort(idx))
r <- inla(y ~ -1 + f(idx, model = "rw2", cyclic = TRUE,
                     constr = FALSE), 
          data = data.frame(y, idx = n + 1:n),
          family = "stdnormal")
inla.dev.new()
plot(r$summary.random$idx$mean)



