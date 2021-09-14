# 21-03-29, Ludger Starke, MDC Berlin, B.U.F.F.

rm(list = ls())

# secure alternative to dchisq(x, df, ncp, log = TRUE) -------------------------
# not vectorized to help translation to C
# use only for df <= 100 and x < 250^2
dnchisq.BesselI.log <- function(x, df, ncp) {

  log.likelihood <- rep(0, length(ncp))
  
  for (i in 1:length(ncp)) {
  
      if (ncp[i] == 0) {
        # besselI is not defined for x = 0
        log.likelihood[i] <- dchisq(x, df, 0, TRUE)
      } else if (sqrt(ncp[i]*x) < 8*10^4) {
        # the cutoff is due to besselI
        # see xlrg_IJ at https://github.com/atks/Rmath/blob/master/bessel.h
        #
        # alternative form of pdf is used
        # https://en.wikipedia.org/wiki/Noncentral_chi-squared_distribution
        #
        # note for C implementation with Rmath: 
        # besselI(..., ..., TRUE) -> bessel_i(..., ..., 2)

        log.likelihood[i] <- -log(2) - (x + ncp[i])/2 +
                          (df/4 - 1/2)*log(x/ncp[i]) + sqrt(x*ncp[i]) +
                          log(besselI(sqrt(x*ncp[i]), df/2 - 1, TRUE))
      } else
        # Approximation of Fraser (is perfect on right tail)
        log.likelihood[i] = -1/2*log(8*pi*x) - 
                            1/2*((sqrt(x) - sqrt(ncp[i])) - (df - 1)/4*(log(x) - log(ncp[i]))/(sqrt(x) - sqrt(ncp[i])))^2 + 
                            log(1 - (df - 1)/(2*(sqrt(x) - sqrt(ncp[i])))*(1/sqrt(x) - 1/2*(log(x) - log(ncp[i]))/(sqrt(x) - sqrt(ncp[i])))) 
  }
  
  return(log.likelihood)
  
}


## plots -----------------------------------------------------------------------

# pdf("dnchisqBesselI_borderCases_logLikelihood.pdf", width=8, height=6)
par(mar = c(4.5, 4.5, 4.5, 2))

df <- 3
x <- 50^2

par(mfrow=c(2,2))

# overview
ncp <- seq(0, 2*sqrt(x), length.out = 10^4)^2

plot(sqrt(ncp), dchisq(x, df, ncp, log = TRUE), type = "l", lwd = 2.5, col = "grey40",
     xlab = "sqrt(ncp)", ylab = "log likelihood", cex.lab = 1.2, cex.axis = 1.1, lty = 1,
     main = '\noverview')

lines(sqrt(ncp), dnchisq.BesselI.log(x, df, ncp), type = "l", col = "tomato2", lwd = 2.5, lty = 2)


# ncp -> 0
ncp <- seq(0, 0.1, length.out = 10^4)^2

dchisq.values <- dchisq(x, df, ncp, log = TRUE) 
plot(sqrt(ncp), dchisq.values, type = "l", lwd = 2.5, col = "grey40",
     xlab = "sqrt(ncp)", ylab = "log likelihood", cex.lab = 1.2, cex.axis = 1.1, lty = 1,
     main = '\nncp -> 0')

lines(sqrt(ncp), dnchisq.BesselI.log(x, df, ncp), type = "l", col = "tomato2", lwd = 2.5, lty = 2)

legend(x = 0, y = max(dchisq.values) - 0.04*(max(dchisq.values) - min(dchisq.values)),
       legend = c("dchisq(..., log = TRUE)", "dnchisq.BesselI.log"),
       lwd = c(2.5, 2.5), lty = c(1, 2), col = c("grey40", "tomato2"),
       cex = 0.9)

# breakdown at ncp << x
ncp <- seq(11.55, 11.7, length.out = 10^4)^2

bessel.values <- dnchisq.BesselI.log(x, df, ncp)

plot(sqrt(ncp), dchisq(x, df, ncp, log = TRUE), type = "l", lwd = 2.5, col = "grey40",
     xlab = "sqrt(ncp)", ylab = "log likelihood", cex.lab = 1.2, cex.axis = 1.1, lty = 1,
     ylim = range(bessel.values), main = '\nbreakdown at ncp << x')

lines(sqrt(ncp), bessel.values, type = "l", col = "tomato2", lwd = 2.5, lty = 2)


# breakdown at ncp >> x
ncp <- seq(88.2, 88.4, length.out = 10^4)^2

bessel.values <- dnchisq.BesselI.log(x, df, ncp)

plot(sqrt(ncp), dchisq(x, df, ncp, log = TRUE), type = "l", lwd = 2.5, col = "grey40",
     xlab = "sqrt(ncp)", ylab = "log likelihood", cex.lab = 1.2, cex.axis = 1.1, lty = 1,
     ylim = range(bessel.values), main = '\nbreakdown at ncp >> x')

lines(sqrt(ncp), bessel.values, type = "l", col = "tomato2", lwd = 2.5, lty = 2)

mtext(paste("LogL: df = ", as.character(df), ",  sqrt(x) = ", as.character(sqrt(x)), sep = ""),
      side = 3, line = -2, outer = TRUE,
      font = 2, cex = 1.1)

# dev.off()

