n <- 100000
x <- rnorm(n)
xx <- rnorm(n)
y <- rpois(n, exp(0 + x + xx))
inla.setOption(num.threads = "1:1", safe = FALSE, verbose = F)
for (m in c("compact")) {
    inla.setOption(inla.mode = m)
    r <- inla(y ~ 1 + x*xx, 
              family = "nbinomial", 
              data = data.frame(y, idx = 1:n, x, xx),
              control.compute = list(cpo = TRUE, po = TRUE, waic = TRUE, dic = TRUE), 
              control.fixed = list(prec.intercept = 1, prec = 1))
    rr <- inla(y ~ 1 + x*xx, 
               family = "nbinomial", 
               data = data.frame(y, idx = 1:n, x, xx),
               control.compute = list(cpo = TRUE, po = TRUE, waic = TRUE, dic = TRUE), 
               control.fixed = list(prec.intercept = 1, prec = 1), 
               inla.call = "inla.mkl.work")##, verbose = T, keep = T)
    print(c(r$logfile[grep("stage1", r$logfile)], rr$logfile[grep("stage1", rr$logfile)]))
}

mean(abs(r$waic$local.waic - rr$waic$local.waic))
mean(abs(r$dic$local.dic - rr$dic$local.dic))
mean(abs(r$cpo$cpo - rr$cpo$cpo))
mean(abs(r$cpo$pit - rr$cpo$pit))
mean(abs(r$po$po - rr$po$po))
mean(abs(r$summary.linear.predictor$mean - rr$summary.linear.predictor$mean))
mean(abs(r$summary.fitted.values$mean - rr$summary.fitted.values$mean))
mean(abs(as.numeric(r$internal.summary.hyperpar - rr$internal.summary.hyperpar)))



