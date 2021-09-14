n = 5
M = 200
N = M + n
p = 2
pac = c(0.5, 0.25)
phi = inla.ar.pacf2phi(pac)

print(pac)
print(phi)

L = matrix(0, N, N)
diag(L) = 1.0
for(i in (p+1):N) {
    for(j in max(1, i-p):(i-1)) {
        L[i, j] = -phi[i-j]
    }
}
S = solve(t(L) %*% L)
prec = 1/S[M, M]
S = S[M:(M+p-1), M:(M+p-1), drop=F]
S = S/S[1, 1]
Q.m = solve(S)

m = 2*p+1
L = matrix(0, m, m)
for(i in (p+1):m) {
    L[i, i]=1
    for(j in max(1, i-p):(i-1)) {
        L[i, j] = -phi[i-j]
    }
}
Q = t(L) %*% L
Q.scaled = Q/prec

Q.marg = matrix(0, nrow(Q), ncol(Q))
for(i in 1:p) {
    for(j in 1:p) {
        Q.marg[i, j] = Q.m[i, j]
    }
}

Q.all = Q.scaled + Q.marg
S.all = solve(Q.all)

