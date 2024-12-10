m <- 3
n <- 2000
N <- (m+1)*n
lp.scale <- rep(c(NA, 1:m), each = n)

off <- runif(N)

eta <- lp.scale
eta[is.na(eta)] <- 1
y <- rpois(N, lambda = exp(off + eta))
Y <- matrix(NA, N, m+1)
for(i in 1:(m+1)) {
    idx <- (i-1)*n + 1:n
    Y[idx, i] <- y[idx]
}
r <- inla(Y ~ 1 + offset(off), 
          family = rep("poisson", m+1), 
          data = list(Y = Y, lp.scale = lp.scale, off = off), 
          lp.scale = lp.scale,
          control.lp.scale = list(
              hyper =  list(theta1 = list(param = c(1, 10)),
                            theta2 = list(param = c(2, 20)))), 
          num.threads = 8, 
          inla.mode = "experimental", 
          verbose = TRUE)
summary(r)
c(1, r$mode$theta) * r$summary.fixed[1,"mean"]


