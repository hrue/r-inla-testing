vals <- 1:5
data <- list(y = 1:5, x = c(NA, 12:15), idx = c(NA, 2:5), widx = c(1, 0, 3, 4, 5)/10, iidx = 5:1,
             cidx = c(1, 2, 3, 3, 3), vals = vals,
             xx = (1:5)/10)
r <- inla(y ~ 1 + x +
              f(idx, widx, model = "rw1", values = vals, 
                hyper = list(prec = list(initial = 0, fixed = TRUE))) +
              f(iidx, model = "rw1", values = vals,
                hyper = list(prec = list(initial = log(10.0), fixed = TRUE))) +
              f(cidx, copy = "idx"), 
              ##f(xx, model = "clinear", range = c(1, 2)), 
          control.fixed = list(prec.intercept = 1.1, prec = 10.1),
          data = data, 
          verbose = TRUE, 
          inla.call = "inla.valgrind",
          inla.arg = "-v -t1:1 -b -P")

n <- 17
A <- matrix(0, n, n)
m <- length(vals)
for(i in 1:m) {

    j <- data$idx[i]
    if (!is.na(j) && data$widx[j] != 0) A[i, j] <- 1

    j <- data$iidx[i]
    if (!is.na(j)) A[i, m + j] <- 1

    j <- data$cidx[i]
    if (!is.na(j)) A[i, 2*m + j] <- 1

    j <- 3*m + 1
    A[i, j] <- 1
    
    j <- 3*m + 2
    if (!is.na(data$x[i]) && data$x[i] != 0) A[i, j] <- 1
}
g <- inla.read.graph(inla.as.sparse(t(A) %*% A))
g

n <- 17
A <- matrix(0, m, n)
m <- length(vals)
for(i in 1:m) {

    j <- data$idx[i]
    if (!is.na(j) && data$widx[j] != 0) A[i, j] <- data$widx[j]

    j <- data$iidx[i]
    if (!is.na(j)) A[i, m + j] <- 1

    j <- data$cidx[i]
    if (!is.na(j)) A[i, 2*m + j] <- 1

    j <- 3*m + 1
    A[i, j] <- 1
    
    j <- 3*m + 2
    if (!is.na(data$x[i]) && data$x[i] != 0) A[i, j] <- data$x[i]
}

inla.as.sparse(t(A) %*% A)

