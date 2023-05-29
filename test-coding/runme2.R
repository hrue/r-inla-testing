n <- 1000
m <- round(sqrt(n))
y <- rnorm(n)
nc <- 10
x <- rnorm(n)
xx <- rnorm(n)
x3 <- rnorm(n)
x4 <- rnorm(n)
A <- matrix(rnorm(n*nc), nc, n)
diag(A) <- diag(A) + 1
e <- rnorm(nc)
constr <- list(A = A, e = e)
idx <- (sample(1:m, n, replace = TRUE))

inla.setOption(num.threads = "2:1", inla.mode = "compact", safe = FALSE)

res <- c(0, 0)
for(i in 1:100) {
    r <- inla(y ~ 1 + x*xx*x3*x4 + f(idx, extraconstr = constr, values = 1:n, 
                         hyper = list(prec = list(initial = 0, fixed = TRUE))), 
              data = data.frame(y, idx, x, xx, x3, x4), 
              control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE))), 
              inla.call = INLA:::inla.call.builtin(), 
              control.fixed = list(prec.intercept = 1, prec = 1), 
              control.inla = list(cmin = 0),
              control.expert = list(disable.gaussian.check = TRUE))

    rr <- inla(y ~ 1 + x*xx*x3*x4 + f(idx, extraconstr = constr, values = 1:n, 
                          hyper = list(prec = list(initial = 0, fixed = TRUE))), 
               data = data.frame(y, idx, x, xx, x3, x4), 
               control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE))), 
               inla.call = "inla.mkl.work", 
               control.fixed = list(prec.intercept = 1, prec = 1), 
               control.inla = list(cmin = 0), 
               control.expert = list(disable.gaussian.check = TRUE))
    
    err <- mean(abs(unlist(c(r$summary.random$idx[, 2:6] - rr$summary.random$idx[, 2:6]))))

    a <- r$logfile[grep("stage1", r$logfile)]
    time.r <- as.numeric(strsplit(a," +")[[1]][5])

    a <- rr$logfile[grep("stage1", rr$logfile)]
    time.rr <- as.numeric(strsplit(a," +")[[1]][5])

    res <- res + c(time.r, time.rr)
    print(round(dig = 4, c(res/i, err*1e9)))
}
