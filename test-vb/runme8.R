library(INLA)
lm <- inla.list.models()
m = get("inla.models", inla.get.inlaEnv())
m$latent$rw2$min.diff = NULL
assign("inla.models", m, inla.get.inlaEnv())

m <- 3
data(Tokyo)
Tokyo <- cbind(rep(Tokyo$y, m), rep(Tokyo$n, m), rep(Tokyo$time, m))
colnames(Tokyo) <- c("y", "n", "time")
Tokyo[, "time"] <- 1:nrow(Tokyo)
Tokyo <- as.data.frame(Tokyo)

formula = y ~ -1 + f(time, model="rw2", cyclic=TRUE, 
                     ## fix the precision here so you can see the effect better
                     hyper = list(prec = list(initial = -5, fixed = !FALSE)), 
                     scale.model = TRUE,
                     vb.correct = 1:nrow(Tokyo))

Sys.setenv(INLA_TRACE = "GMRFLib_QM")
r = inla(formula,
         family="binomial",
         Ntrials=n,
         data=Tokyo,
         inla.call = "inla.mkl.work",
         num.threads = "4:1",
         control.inla = list(control.vb = list(enable = TRUE)), 
         safe = FALSE, keep = FALSE, 
         verbose = !TRUE)

rr = inla(formula,
          family="binomial",
          Ntrials=n,
          data=Tokyo,
          num.threads = "4:1",
          control.inla = list(control.vb = list(enable = TRUE)),
          verbose = !TRUE, 
          inla.call = INLA:::inla.call.builtin())

print(max(abs(r$summary.linear.predictor$mean - rr$summary.linear.predictor$mean)))

