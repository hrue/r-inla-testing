param <- c(10, 0.5, 1, 0)
r <- exp(seq(-3, 4, by = 0.01))
lty <- 1
plot(r, inla.prw2.drange(r, param), ylim=c(0, 0.1), type = "l", lty = lty)
for (h in 1/2^(1:8)) {
    param[3] <- h
    lty <- lty+1
    lines(r, inla.prw2.drange(r, param), lty = lty)
}

    
    
