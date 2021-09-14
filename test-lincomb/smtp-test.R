n = 30
nlc = 5

lc = c()
for(i in 1:nlc) {
    nw = nlc
    iw = sample(1:n, nw)
    w = numeric(n)
    w[iw] = runif(nw)
    llc = inla.make.lincomb(x = w)
    names(llc) = paste0("lc", i)
    lc = c(lc,  llc)
}
formula = y ~ -1 + f(x, model="ar1", constr=T) + f(z, constr=T) + f(zz, constr=T)
stopifnot((n %% 5 == 0) && (n %% 10 == 0))
y = rnorm(n)
x = sample(1:n, n, replace=FALSE)
z = rep(1:5, each = n %/% 5)
zz = rep(1:10, each = n %/% 10)

rr = NULL
for (inla.call in c("inla", "inla.work", "inla.valgrind")) {
    for (smtp in c("band",  "taucs")) {
        if (inla.call != "inla" || smtp != "band") {
            r = inla(formula,
                     data = data.frame(y, x, z, zz),
                     family = "t", 
                     lincomb = lc,
                     control.family = list(hyper = list(prec = list(initial = 0, fixed=TRUE),
                                                        dof = list(initial = 20, fixed=TRUE))), 
                     control.inla = list(lincomb.derived.correlation.matrix=TRUE, strategy = "laplace"), 
                     control.fixed = list(prec.intercept = 1), 
                     verbose=FALSE, 
                     inla.call = inla.call,
                     num.threads = 4, 
                     control.compute = list(smtp = smtp))
            if (is.null(rr)) {
                rr = r
                if (FALSE) {
                    print(round(r$misc$lincomb.derived.correlation.matrix, digits = 6))
                    print(round(r$summary.lincomb.derived[, c("0.025quant", "0.975quant")],
                                digits=6))
                }
            } else {
                print(round(r$misc$lincomb.derived.correlation.matrix -
                            rr$misc$lincomb.derived.correlation.matrix, 
                            digits = 6))
                print(round(r$summary.lincomb.derived[, c("0.025quant", "0.975quant")] -
                            rr$summary.lincomb.derived[, c("0.025quant", "0.975quant")],
                            digits=6))
                print(head(round(r$summary.random$x[, c("0.025quant", "0.975quant")] -
                                 rr$summary.random$x[, c("0.025quant", "0.975quant")],
                                 digits=6)))
            }
        }
    }
}
rr = NULL


