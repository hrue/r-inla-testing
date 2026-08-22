## Test Poisson log-normal likelihood
## Compares:
##   Model 1: Poisson + per-observation iid random effect (reference)
##   Model 2: poissonlognormal likelihood across nquad = 10, 25, 50

library(INLA)

pln_summary <- function(m, label) {
    cat(sprintf("  %-20s intercept=%.4f  prec=%.4f  DIC=%.1f\n",
                label,
                m$summary.fixed[1, "mean"],
                inla.mmarginal(m$marginals.hyperpar[[1]]),
                m$dic$dic))
}

## ============================================================
## Large-rate data  (beta0=5, sigma=1, mean(y) ~ 231)
## ============================================================
cat("=== Large-rate data (beta0=5, sigma=1) ===\n")
set.seed(42)
n          <- 500
beta0_true <- 5.0
sigma_true <- 0.25
prec_true  <- 1 / sigma_true^2

u <- rnorm(n, 0, sigma_true)
y <- rpois(n, exp(beta0_true + u))
cat(sprintf("  n=%d  mean(y)=%.1f  fraction zero=%.3f\n\n",
            n, mean(y), mean(y == 0)))

cat("  True: intercept=5.0  prec=1.0\n\n")

hyper = list(prec = list(
                 initial = 4,
                 prior = "pc.prec", 
                 param = c(0.5, 0.01), 
                 fixed = FALSE))


## Poisson + iid (reference)
idx <- 1:n
m1 <- inla(y ~ 1 + f(idx, model = "iid"),
           family          = "poisson",
           data            = data.frame(y = y, idx = idx),
           control.compute = list(dic = TRUE))
pln_summary(m1, "Poisson+iid")

## poissonlognormal across nquad values
nq <- 15
cat("nq ", nq, "\n")
    m <- inla(y ~ 1,
              family          = "poisson",
              control.family = list(control.mix = list(
                                        model = "gaussian",
                                        integrator = "quadrature", 
                                        npoints = nq, 
                                        hyper = hyper)),
              safe = FALSE, 
              verbose = TRUE, 
              control.inla = list(cmin = 0), 
              data            = data.frame(y = y),
              control.compute = list(dic = TRUE))
    pln_summary(m, sprintf("PLN nquad=%d", nq))

stop("XXXXXXXXXX")

## poissonlognormal across nquad values
for (nq in c(11L, 15L)) {
    cat("nq ", nq, "\n")
    m <- inla(y ~ 1,
              family          = "poisson",
              control.family = list(control.mix = list(
                                        model = "gaussian",
                                        integrator = "quadrature", 
                                        npoints = nq, 
                                        hyper = hyper)),
              control.inla = list(cmin = 0), 
              data            = data.frame(y = y),
              control.compute = list(dic = TRUE))
    pln_summary(m, sprintf("PLN nquad=%d", nq))
}

## ============================================================
## Sparse data  (beta0=-3.5, sigma=2, ~89% zeros)
## ============================================================
cat("\n=== Sparse data (beta0=-3.5, sigma=2) ===\n")
set.seed(123)
n_s          <- 10000
beta0_sparse <- -3.5
sigma_sparse <- 2.0
prec_sparse  <- 1 / sigma_sparse^2

u_s <- rnorm(n_s, 0, sigma_sparse)
y_s <- rpois(n_s, exp(beta0_sparse + u_s))
cat(sprintf("  n=%d  mean(y)=%.4f  fraction zero=%.3f\n\n",
            n_s, mean(y_s), mean(y_s == 0)))

cat(sprintf("  True: intercept=%.1f  prec=%.4f\n\n", beta0_sparse, prec_sparse))

## Poisson + iid (reference)
idx_s <- 1:n_s
m1s <- inla(y ~ 1 + f(idx, model = "iid"),
            family          = "poisson",
            data            = data.frame(y = y_s, idx = idx_s),
            control.compute = list(dic = TRUE),
            safe            = TRUE)
pln_summary(m1s, "Poisson+iid")

## poissonlognormal across nquad values
for (nq in c(11L, 15L, 25L)) {
    cat("nq ", nq, "\n")
    m <- inla(y ~ 1,
              family          = "poisson",
              control.family = list(control.mix = list(
                                        model = "gaussian",
                                        integrator = "quadrature", 
                                        npoints = nq, 
                                        hyper = hyper)),
              control.inla = list(cmin = 0), 
              data            = data.frame(y = y_s),
              control.compute = list(dic = TRUE))
    pln_summary(m, sprintf("PLN nquad=%d", nq))
}

cat("\n=== Done ===\n")
