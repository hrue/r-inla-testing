m1 <- 20
m2 <- 30
n <- m1 * m2
s <- 0.1

make.vec <- function(i, n) {
    idx <- rep(0, n)
    idx[i] <- 1
    return (idx)
}

x1 <- rnorm(m1)
x2 <- rnorm(m2)

all.lc <- c()
all.lc.mean <- c()
y <- c()
for(i in 1:m1) {
    for(j in 1:m2) {
        y <- c(y, 0 + x1[i] + x2[j] + rnorm(1, sd = s))
        
        lc <- inla.make.lincomb(i1 = make.vec(i, m1),
                                i2 = make.vec(j, m2))
        names(lc) <- paste0("x1:", i, ",x2:", j)
        all.lc <- c(all.lc, lc)
        all.lc.mean <- c(all.lc.mean, x1[i] + x2[j])
    }
}

hyper <- list(prec = list(initial = 0,
                          fixed = TRUE))

r <- inla(y ~ -1 +
              f(i1, hyper = hyper) +
              f(i2, hyper = hyper),
          data = data.frame(y,
                            i1 = rep(1:m1, each = m2),
                            i2 = rep(1:m2, m1)),
          control.family = list(
              hyper = list(prec = list(initial = log(1/s^2),
                                       fixed = TRUE))), 
          lincomb = all.lc)

plot(r$summary.lincomb.derived$mean, all.lc.mean)
abline(a=0,b=1)
