INLA:::inla.my.update(b = TRUE)

n <- 2
A <- matrix(rnorm(n^2), n, n)
off <- rnorm(n)
y <- rnorm(n)
inla.setOption(inla.mode = mode, safe = FALSE)

for (mode in c("classic", "compact")) {
    print(mode)

    r = inla(y ~ 1,
             control.family = list(hyper = list(prec = list(initial = 20, fixed = TRUE))), 
             data = list(y = y,  off = off, A = A),
             control.fixed = list(prec.intercept = 0), 
             control.compute = list(config=TRUE, return.marginals.predictor = TRUE),
             control.inla = list(control.vb = list(enable = FALSE)), 
             control.predictor = list(A=A, compute = TRUE), offset = off)
    print(r$logfile[grep("Mode[.]", r$logfile)])
    rr = inla(y ~ 1 + offset(off), 
              control.family = list(hyper = list(prec = list(initial = 20, fixed = TRUE))), 
              data = list(y = y,  off = off, A = A),
              control.fixed = list(prec.intercept = 0),
              control.inla = list(control.vb = list(enable = FALSE)), 
              control.predictor = list(compute = TRUE), 
              control.compute = list(config=TRUE, return.marginals.predictor = TRUE))
    print(rr$logfile[grep("Mode[.]", rr$logfile)])

    rrr = inla(y ~ 1 + offset(off), 
              control.family = list(hyper = list(prec = list(initial = 20, fixed = TRUE))), 
              data = list(y = y,  off = off, A = A),
              control.fixed = list(prec.intercept = 0),
              control.inla = list(control.vb = list(enable = FALSE)), 
              control.predictor = list(A = A, compute = TRUE), 
              control.compute = list(config=TRUE, return.marginals.predictor = TRUE),
              offset = off)
    print(rrr$logfile[grep("Mode[.]", rrr$logfile)])
    
    print("r$lp$mean")
    print(round(dig = 3, r$summary.linear.predictor$mean))
    print("rr$lp$mean")
    print(round(dig = 3, rr$summary.linear.predictor$mean))
    print("rrr$lp$mean")
    print(round(dig = 3, rrr$summary.linear.predictor$mean))

    print("summary.fixed")
    print(round(dig = 3, r$summary.fixed[, "mean"]))
    print(round(dig = 3, rr$summary.fixed[, "mean"]))
    print(round(dig = 3, rrr$summary.fixed[, "mean"]))

    print("config.mean")
    print(round(dig = 3, r$misc$configs$config[[1]]$mean))
    print(round(dig = 3, rr$misc$configs$config[[1]]$mean))
    print(round(dig = 3, rrr$misc$configs$config[[1]]$mean))

    print("mode$x")
    print(round(dig = 3, r$mode$x))
    print(round(dig = 3, rr$mode$x))
    print(round(dig = 3, rrr$mode$x))

    if (inla.getOption('inla.mode') == "compact") {
        print("r APredictor Predictor")
        print(round(dig = 3, c(r$misc$configs$config[[1]]$APredictor[, "mean"],
                               r$misc$configs$config[[1]]$Predictor[, "mean"])))
        print("rr APredictor Predictor")
        print(round(dig = 3, c(rr$misc$configs$config[[1]]$APredictor[, "mean"],
                               rr$misc$configs$config[[1]]$Predictor[, "mean"])))
        print("rrr APredictor Predictor")
        print(round(dig = 3, c(rrr$misc$configs$config[[1]]$APredictor[, "mean"],
                               rrr$misc$configs$config[[1]]$Predictor[, "mean"])))
    }
    xx <- inla.posterior.sample(10^4, r)
    print("r sample")
    print(round(dig = 3, rowMeans(inla.posterior.sample.eval(function() c(APredictor, Predictor), xx))))
    
    print("rr sample")
    xx <- inla.posterior.sample(10^4, rr)
    print(round(dig = 3, rowMeans(inla.posterior.sample.eval(function() c(Predictor), xx))))

    print("rrr sample")
    xx <- inla.posterior.sample(10^4, rrr)
    print(round(dig = 3, rowMeans(inla.posterior.sample.eval(function() c(APredictor, Predictor), xx))))
}
