## this testing-code for the spde3-interface testing random matrices

n = 100
m = 9  ## this is 'n3' in the spesification
p = 5  

srm = function(n, m=n, symmetric = TRUE)
{
    if (symmetric) stopifnot(n == m)
    M = matrix(rnorm(n*m), n, m)
    to.zero = matrix(runif(n*m), n, m)
    d = diag(M)
    M[to.zero < 0.75] = 0
    diag(M) = d
    if (symmetric) {
        M = M %*% t(M)
    }
    return(INLA:::inla.as.sparse(M))
}

M0 = srm(n, symmetric = TRUE)
M1 = srm(n, symmetric = FALSE)
M2 = srm(n, symmetric = TRUE)
M3 = srm(m, n, symmetric = FALSE)

B0 = matrix(rnorm(n*(p+1)), n, p+1)
B1 = matrix(rnorm(n*(p+1)), n, p+1)
B2 = matrix(rnorm(n*(p+1)), n, p+1)
B3 = matrix(rnorm(m*(p+1)), m, p+1)
theta = matrix(rnorm(p, sd=0.3), p, 1)

phi0 = c(B0[, 1] + B0[, -1] %*% theta)
phi1 = c(B1[, 1] + B1[, -1] %*% theta)
phi2 = c(B2[, 1] + B2[, -1] %*% theta)
phi3 = c(B3[, 1] + B3[, -1] %*% theta)

D0 = diag(exp(phi0))
D1 = diag(exp(phi1))
D2 = diag(phi2)
D3 = diag(exp(phi3))

Q = D0 %*% (
        D1 %*% M0 %*% D1 +
        D2 %*% D1 %*% M1 +
        t(M1) %*% D1 %*% D2 +
        M2 +
        t(M3) %*% D3 %*% M3)  %*% D0
Q = INLA:::inla.as.sparse(Q)
## make it numerical symmetric as well
Q = (Q + t(Q))/2.0

for (mat in c("M0", "M1", "M2", "M3", "B0", "B1", "B2", "B3")) {
    tmp = INLA:::inla.write.fmesher.file(get(mat),  mat)
}
tmp = INLA:::inla.write.fmesher.file(matrix(theta, 1, length(theta)),  "theta")

system(paste(INLA:::inla.call.builtin(), "-m testit"))
Qspde3 = INLA:::inla.as.sparse(INLA:::inla.read.fmesher.file("Q"))

print(paste("ERROR1 = ", max(abs(Q - Qspde3)/mean(abs(Q)))))
print(paste("ERROR2 = ", max(abs(Q - Qspde3))))

