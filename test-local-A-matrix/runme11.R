g = inla.read.graph(system.file("demodata/germany.graph", package="INLA"))
source(system.file("demodata/Bym-map.R", package="INLA"))

n <- g$n
m <- 15
s <- 0.1

X <- matrix(rnorm(n*m), n, m)
y <- rowSums(X) + rnorm(n, sd = s)

A <- inla.as.sparse(diag(X[, 1]))
for(i in 2:m)
    A <- cbind(A, inla.as.sparse(diag(X[, i])))

r <- inla(y ~ -1 +
              f(idx.na,
                model = "besag",
                scale.model = TRUE,
                constr = FALSE,
                nrep = m,
                graph = g, 
                A.local = A,
                values = 1:n,
                hyper = list(prec = list(prior = "pc.prec",
                                         param = c(0.5, 0.01)))), 
          family = "normal",
          control.family = list(hyper = list(prec = list(initial = log(1/s^2),
                                                         fixed = TRUE))), 
          data = list(n = n, m = m, y = y, X = X, idx.na = rep(NA, n)),
          verbose = TRUE)

for(i in 1:m) {
    inla.dev.new()
    Bym.map(r$summary.random$idx.na$mean[ (i-1) * n + 1:n])
    title(paste("coeff ",  i))
}
