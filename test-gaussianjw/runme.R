INLA:::inla.my.update()
inla.setOption(inla.call = "inla.mkl.work")
inla.setOption(inla.mode = "experimental")

n <- 300
x <- rnorm(n, sd = 0.5)
eta <- 1 + x
p <- 1/(1 + exp(-eta))
df <- sample(10:100, n, replace = TRUE)
size <- df
va <- p * (1.0 - p) / size
v <- rchisq(n, df = df) * va / df
pahat <- rnorm(n, mean = p, sd = sqrt(v))
Y <- inla.mdata(pahat, v, size, df)

r <- inla( Y ~ 1 + x,
          data = list(Y = Y, x = x),
          family = "gaussianjw",
          control.inla = list(cmin = 0), 
          verbose = TRUE)
summary(r)

