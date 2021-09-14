n <- 512
phi <- 0.9
x <- arima.sim(n, model = list(ar = phi))
x <- as.numeric(scale(x))

per <- ((1/n)*abs(fft(x)^2))[2:(n/2)]
r <- inla(log(per) ~ 1 + f(i, model = "rw2", scale.model = TRUE),
          family = "logperiodogram", 
          data = data.frame(per, i = 1:length(per)),
          verbose = TRUE)

i <- (2:(n/2))/(n/2)/2
log.f <- 1/(1 - 2*phi * cos(i * 2 * pi) + phi^2)

plot(i, r$summary.random$i$mean)
lines(i, log(log.f), lwd = 3, col = "blue")

