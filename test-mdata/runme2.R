n <- 100
m <- 10

yy <- numeric(n+m)
yy[(n+1):(n+m)] <- rnorm(m)
yy[1:n] <- NA

x <- rnorm(n, sd = 0.5)
eta <- 1 + x
p <- 1/(1 + exp(-eta))
df <- sample(10:100, n, replace = TRUE)
size <- df
va <- p * (1.0 - p) / size
v <- rchisq(n, df = df) * va / df
phat <- rnorm(n, mean = p, sd = sqrt(v))
mNA <- rep(NA, m)

Y <- inla.mdata(cbind(c(phat, 1:m), c(v, mNA)),
                cbind(c(size, 1:m), c(df, 1:m)))

YY <- list(Y = Y, yy = yy)
r <- inla(YY ~ 1 + x,
           data = list(Y = Y, x = c(x, 1:m)), 
           family = c("gaussianjw", "gaussian"), 
           control.inla = list(cmin = 0))
summary(r)

