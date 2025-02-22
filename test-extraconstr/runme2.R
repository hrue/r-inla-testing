INLA:::inla.my.update()
inla.setOption(num.threads = "8:2")

n <- 1000
idx <- 1:n
eta <- sin(idx/2)
y <- eta + rnorm(n, sd = 1)

m <- n %/% 2L
A <- inla.as.sparse(matrix(rnorm(n*m) * rbinom(n*m, size = 1, prob = 0.5), m, n))
e <- rep(0, m)
extraconstr <- list(A = A, e = e)

##Sys.setenv(INLA_TRACE = "GMRFLib_solve_llt_sparse_matrix")

tim <- numeric(3)

r <- inla(y ~ 1 + f(idx, model = "rw2", scale.model = TRUE,
                          initial = 0, fixed = !TRUE, 
                          constr = TRUE, 
                          extraconstr = extraconstr), 
                family = "stdnormal", 
                data = data.frame(idx, y),
                verbose = F, 
                inla.call = "inla.mkl")

x <- rnorm(n)
xx <- rnorm(n)
xxx <- rnorm(n)
r <- inla(y ~ 1 + x + xx + xxx +
              f(idx, model = "rw2", scale.model = TRUE,
                initial = 0, fixed = !TRUE, 
                constr = !TRUE, 
                extraconstr = extraconstr), 
          family = "stdnormal", 
          data = data.frame(idx, y, x, xx, xxx),
          verbose = !F, 
          inla.call = "inla.mkl")

rr <- inla(y ~ 1 + x + xx + xxx +
               f(idx, model = "rw2", scale.model = TRUE,
                 initial = 0, fixed = !TRUE, 
                 constr = !TRUE, 
                 extraconstr = extraconstr), 
           family = "stdnormal", 
           data = data.frame(idx, y, x, xx, xxx),
           verbose = !F, safe = F, keep = FALSE, 
           inla.call = "inla.mkl.work")

a <- rr$logfile[grep("solve_llt", rr$logfile)]
a <- strsplit(a[grep("Leave, ", a)], " ")
tim[1] <- sum(unlist(lapply(a, function(x) as.numeric(x[length(x)-1]))))
print(tim[1])

r$cpu.used
rr$cpu.used
r$mlik - rr$mlik
