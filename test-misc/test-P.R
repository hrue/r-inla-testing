INLA:::inla.my.update(b = TRUE)
y <- -4:5
inla.setOption(num.threads = 1)
r <- inla(y ~ 1 + offset(y),
          data = data.frame(y), 
          control.family = list(hyper = list(prec = list(initial = 2*log(1/length(y)),
                                                         fixed = TRUE))),
          control.compute = list(return.marginals.predictor = TRUE), 
          control.predictor = list(cdf = 0), 
          verbose = TRUE)

n <- length(y)
for(i in 1:n) {
    p <- inla.pmarginal(0, r$marginals.linear.predictor[[i]])
    print(c(i, p, r$summary.linear.predictor$"0 cdf"[i]))
}
