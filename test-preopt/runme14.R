set.seed(123)
n <- 10^5
y <- rnorm(n)
inla.setOption(num.threads = "1:1")

ii <- sample(1:1000, n, replace = TRUE)
idx <- c()
for (i in 1:n) {
    idx <- c(idx, rep(ii[i], each = ii[i]))
    if (length(idx) >n)
        break
}
idx <- idx[1:n]
rr <- inla(y ~ 1 + f(idx),
          data = data.frame(y, idx),
          control.fixed = list(prec.intercept = 1),
          inla.mode = "experimental",
          inla.call = "inla.mkl.work",
          safe = FALSE, 
          verbose = TRUE)
r <- inla(y ~ 1 + f(idx),
          data = data.frame(y, idx),
          control.fixed = list(prec.intercept = 1),
          inla.mode = "experimental",
          safe = FALSE, 
          verbose = TRUE)

r$mlik - rr$mlik
r$cpu
rr$cpu
