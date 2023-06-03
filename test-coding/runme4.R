n <- 100000
x <- rnorm(n)
xx <- rnorm(n)
y <- 1 + x + xx + x*xx + rnorm(n, sd = 0.01)
inla.setOption(num.threads = "1:1")
##inla.setOption(smtp = "taucs")
inla.setOption(smtp = "pardiso")

##for (m in c("compact", "classic")) {
for(times in 1:2) {
    for (m in c("compact")) {
        inla.setOption(inla.mode = m)
        r <- inla(y ~ 1 + x*xx, 
                  data = data.frame(y, idx = 1:n, x, xx),
                  control.fixed = list(prec.intercept = 1, prec = 1), 
                  family = "t", 
                  control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE),
                                                     dof = list(initial = 3, fixed = TRUE))), 
                  control.inla = list(cmin = 0, int.strategy = "eb"), 
                  control.compute = list(internal.opt = FALSE, cpo = TRUE, dic = TRUE, po = TRUE), 
                  control.expert = list(disable.gaussian.check = TRUE))
        rr <- inla(y ~ 1 + x*xx, 
                   data = data.frame(y, idx = 1:n, x, xx),
                   control.fixed = list(prec.intercept = 1, prec = 1), 
                   family = "t", 
                   control.family = list(hyper = list(prec = list(initial = 0, fixed = TRUE), 
                                                      dof = list(initial = 3, fixed = TRUE))), 
                   inla.call = "inla.mkl.work", 
                   control.inla = list(cmin = 0, int.strategy = "eb"), 
                   control.compute = list(internal.opt = FALSE, cpo = TRUE, dic = TRUE, po = TRUE), 
                   control.expert = list(disable.gaussian.check = TRUE))

        print(list(##err.cpo = mean(abs(r$cpo$cpo - rr$cpo$cpo)),
                   ##err.pit = mean(abs(r$cpo$pit - rr$cpo$pit)),
                   err.dic = mean(abs(r$dic$local.dic - rr$dic$local.dic))))
            ##err.po = mean(abs(r$po$po - rr$po$po))))
        
        print(c(r$logfile[grep("stage1", r$logfile)], rr$logfile[grep("stage1", rr$logfile)]))
    }
}
