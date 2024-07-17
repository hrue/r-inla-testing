n <- 30
y <- rpois(n, exp(2))
idx <- 1:n
for (emerg in c(0.001,  25)) {
    cat("emergency = ", emerg, "\n")
    r <- inla(y ~ 1 + f(idx, vb.correct = T, initial = log(1), fixed = T), 
              data = data.frame(y, idx),
              control.inla = list(control.vb = list(emergency = emerg), 
                                  int.strategy = "eb"), 
              family = "poisson")
    if (length(grep("max_correction = ", r$logfile)) > 0) {
        cat("VB ABORTED\n")
    } else {
        cat("VB OK\n")
    }
}
