n <- 25
s <- 0.01
y <- sin(1:n/5) + rnorm(n, sd = s)

lc1 <- inla.make.lincomb('(Intercept)' = 1, time = 1)
names(lc1) <- "lc1"
lc2 <- inla.make.lincomb('(Intercept)' = 1, time = c(0, 1))
names(lc2) <- "lc2"
lc3 <- inla.make.lincomb('(Intercept)' = 1, time = c(0, 0, 1))
names(lc3) <- "lc3"
r <- inla(y ~ 1 + f(time, model = "rw2", scale.model = TRUE,
                    constr = FALSE, 
                    hyper = list(prec = list(initial = 6, fixed = TRUE))), 
          data = data.frame(y, time = 1:n),
          lincomb = c(lc1, lc2, lc3), 
          control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed = TRUE))), 
          control.predictor = list(compute = TRUE),
          control.fixed = list(prec.intercept = 1),
          control.inla = list(int.strategy = "eb"), 
          inla.mode = "compact",
          keep = TRUE,
          verbose = TRUE, 
          num.threads = "1:1", 
          control.compute = list(smtp = "pardiso"))
          ##control.compute = list(smtp = "taucs")) 
summary(r)

round(dig = 8, matrix(unlist(r$summary.lincomb.derived[1:3, c("mean", "sd")]), 3, 2))
round(dig = 8, matrix(unlist(r$summary.linear.predictor[1:3, c("mean", "sd")]), 3, 2))
