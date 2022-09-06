INLA:::inla.my.update()
set.seed(1234)
n <- 5
A <- matrix(rnorm(n^2), n, n)
x <- rnorm(n, sd = 0.2)
eta <-  A %*% (1 + x)
##eta <-  1 + x
y <- rpois(n, exp(eta))

make.vec <- function(i, n) {
    idx <- rep(0, n)
    idx[i] <- 1
    return (idx)
}

inla.setOption(inla.call = "inla.mkl.work",
               inla.mode = "experimental")

B <- matrix(rnorm(n^2), n, n)
lcs <- c()
for(i in 1:n) {
    lc <- inla.make.lincomb(APredictor = B[i, ],
                            Predictor = B[, i])
    names(lc) <- paste0("APtest:", i)
    lcs <- c(lcs, lc)
}
r <- inla(y  ~ 1 + x,
          data = data.frame(y, x),
          family = "poisson",
          lincomb = lcs, 
          safe = FALSE,
          verbose = TRUE, 
          control.fixed = list(correlation.matrix = TRUE), 
          control.compute = list(config = TRUE),
          control.predictor = list(A = A), 
          control.lincomb = list(verbose = TRUE), 
          control.inla = list(control.vb = list(enable = FALSE)))

inla.setOption(inla.mode = "classic")
rr <- inla(y  ~ 1 + x,
          data = data.frame(y, x),
          family = "poisson",
          lincomb = lcs, 
          safe = FALSE,
          verbose = TRUE, 
          control.fixed = list(correlation.matrix = TRUE), 
          control.compute = list(config = TRUE),
          control.predictor = list(A = A), 
          control.lincomb = list(verbose = TRUE), 
          control.inla = list(strategy = "gaussian"))

round(dig = 4, r$misc$lincomb.derived.correlation.matrix - rr$misc$lincomb.derived.correlation.matrix)

round(dig = 4, as.numeric(unlist(r$summary.lincomb.derived)- unlist(rr$summary.lincomb.derived)))


