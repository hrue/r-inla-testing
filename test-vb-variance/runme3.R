n <- 100
x <- rnorm(n)
y <- rpois(n, lambda = exp(-2 + 0.5 * x))

Sys.setenv(INLA_TRACE = "GMRFLib_ai_vb_correct_mean_preopt,GMRFLib_ai_vb_correct_variance_preopt")
r <- inla(y~1+x+f(idx),
          data = data.frame(y, x, idx = 1:n),
          family = "poisson",
          control.inla = list(control.vb = list(strategy = "variance", f.enable.limit = c(300, 300))),
          inla.call = "inla.mkl",
          verbose = T, 
          num.threads = "1:1")

rr <- inla(y~1+x+f(idx),
          data = data.frame(y, x, idx = 1:n),
          family = "poisson",
          control.inla = list(control.vb = list(strategy = "variance", f.enable.limit = c(300, 300))),
          inla.call = "inla.mkl.work", 
          verbose = T, 
          num.threads = "1:1")

r$summary.fixed - rr$summary.fixed
