m<- inla.models()
m$likelihood$poisson$link <- c(m$likelihood$poisson$link, "identity")
assign("inla.models",  m, envir = inla.get.inlaEnv())
stopifnot(any(inla.models()$likelihood$poisson$link == "identity"))

n <- 10000
x <- rnorm(n)
y <- rpois(n, 10 + x)

r <- inla(y ~ offset(off) + 1 + x,
          data = data.frame(y, off = 10, x),
          family = "poisson",
          control.family = list(control.link = list(model = "identity")))
summary(r)




          
