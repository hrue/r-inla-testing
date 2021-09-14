n <- 10
QQ <- matrix(rnorm(n^2), n, n)
QQ <- QQ %*% t(QQ)
d <- diag(runif(n))
Q <- QQ + d

eps <- 1E-4

ij <- sample(1:n, 2, replace = FALSE)
i <- ij[1]
j <- ij[2]

Q <- QQ + d
b <- rnorm(n)
bb <- b
bb[j] <- bb[j] + eps
print(list(approx = (solve(Q, bb)[i] - solve(Q, b)[i])/eps, 
           exact = solve(Q)[i, j]))
dd <- d
dd[j, j] <- dd[j, j] + eps
print(list(approx = (solve(QQ+dd)[i, i] - solve(Q)[i, i])/eps,
           exact = -(solve(Q)[i, j])^2))
