inla.setOption(INLAjoint.features=TRUE)

nm <- 99
n <- 999
y <- rnorm(n)

tref.1a <- Sys.time()
wd <- "model.files"
unlink(wd, recursive = TRUE)
off <- matrix(rnorm(n*nm), n, nm)
theta <- matrix(runif(nm, min = -0.2, max = 0.2), 1, nm)

r <- inla(y ~ 1,
          data = list(y = y, offset = off), 
          offset = off, 
          verbose = !TRUE, 
          working.directory = wd, 
          family = "normal",
          control.mode = list(theta = theta,
                              ## add this?  x = ..., 
                              fixed = TRUE,
                              restart = FALSE), 
          control.compute = list(return.marginals = FALSE), 
          control.inla = list(compute.initial.values = FALSE), 
          quantiles = c(), 
          inla.call = "",
          keep = TRUE,
          safe = FALSE)

tref.1a <- Sys.time() - tref.1a

tref.1b <- Sys.time()
r <- INLA:::inla.run.many(nm, wd, num.threads = "4:1", cleanup = !TRUE, verbose = !TRUE)
tref.1b <- Sys.time() - tref.1b

print(tref.1a + tref.1b)
print(tref.1a)
print(tref.1b)
