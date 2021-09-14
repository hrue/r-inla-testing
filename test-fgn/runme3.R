## plot the acf for the approximation with the truth

library(FGN)

for(H in seq(0.7, 0.95, by=0.05)) {
    lagmax = 1500
    r = acvfFGN(H, lagmax)

    inla.dev.new()
    plot(0:lagmax, r, ylim=c(0, 1), type="l", lwd=5, col="red",
         main = paste0("H= ", round(H, dig=3)))

    for(k in c(3, 4)) {
        cof = inla.fgn(H, k)
        nn = lagmax + 1
        S = rep(0, nn)
        for(i in 1:k) {
            S = S + cof[1, (k+1)+i] * cof[1, 1+i]^(0:(nn-1))
        }
        if (k == 3) {
            lines(0:lagmax, S, lwd=3, col="blue", lty=1)
        } else {
            lines(0:lagmax, S, lwd=3, col="green", lty=2)
        }
    }
}
