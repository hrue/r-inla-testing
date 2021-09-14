##set.seed(123)
n <- 1163
m <- 1184
x <- rnorm(n)
xx <- rnorm(n)
xxx <- rnorm(n)
xx[(n %/% 2):n] <- 0
x[n:(n %/% 2)] <- 0
s <- 0.1
y <- 1 + x + xx + xxx + rnorm(n, sd = s)
A <- matrix(runif(n*m), m, n)
for(i in 1:nrow(A)) {
    while(sum(mask <- sample(0:1, ncol(A), prob = c(1, 5/n), replace = TRUE)) == 0) {
        ## do nothing
    }
    A[i, ] <- mask * A[i, ]
    A[i, ] <- A[i, ]/sum(A[i, ])
}
stopifnot(all(rowSums(A != 0) >0))
A <- inla.as.sparse(A)

y <- A %*% cbind(1+x+xx+xxx) + rnorm(m, sd = 0.1)

r <- inla(y ~ 1 + x + xx + xxx,
          data = list(y = y, x = x, xx = xx, xxx = xxx, A = A),
          family = "t", 
          verbose = TRUE, keep = T, 
          control.compute = list(smtp = 'pardiso'), 
          control.inla = list(cmin = 0), 
          control.predictor = list(A = A, hyper = list(prec = list(initial = 14))), 
          control.fixed = list(prec.intercept = 1, prec = 1), 
          control.family = list(hyper = list(prec = list(initial = 4, fixed = FALSE),
                                             dof = list(initial = 6, fixed = TRUE))), 
          inla.call = "inla.mkl.work", inla.arg = "-v -t4:1 -b -P")
##r <- inla.rerun(r)
##stop("XXXXXXXXX")

rr <- inla(y ~ 1 + x + xx + xxx,
           data = list(y = y, x = x, xx = xx, xxx = xxx),
           family = "t", 
           verbose = TRUE,
           control.compute = list(smtp = 'pardiso'), 
           control.inla = list(cmin = 0), 
           control.predictor = list(A = A, hyper = list(prec = list(initial = 14))), 
           control.fixed = list(prec.intercept = 1, prec = 1), 
           control.family = list(hyper = list(prec = list(initial = 4, fixed = FALSE),
                                              dof = list(initial = 6, fixed = TRUE))), 
           inla.call = "inla.mkl.work", inla.arg = "-v -t4:1 -b")
##rr <- inla.rerun(rr)

r$summary.fixed - rr$summary.fixed
r$mlik - rr$mlik
plot(r$mode$x,  rr$mode$x)
abline(a = 0, b = 1)
r$mode$theta - rr$mode$theta
