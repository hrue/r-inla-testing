set.seed(1234)
n <- 10
x <- rnorm(n, sd = 0.2)
eta <-  -1 + x
y <- rpois(n, exp(eta))

inla.setOption(inla.call = "inla.mkl.work",
               inla.mode = "experimental")

r <- inla(y  ~ 1 + x,
          data = data.frame(y, x),
          family = "poisson",
          control.compute = list(config = TRUE),
          control.inla = list(control.vb = list(enable = FALSE)))

rr <- inla(y  ~ 1 + x,
          data = data.frame(y, x),
          family = "poisson",
          control.compute = list(config = TRUE),
          control.inla = list(control.vb = list(enable = TRUE,
                                                strategy = "mean")))

rrr <- inla(y  ~ 1 + x,
          data = data.frame(y, x),
          family = "poisson",
          control.compute = list(config = TRUE),
          control.inla = list(control.vb = list(enable = TRUE,
                                                strategy = "variance")),
          verbose = TRUE)

A <- rbind(cbind(mean = r$misc$configs$config[[1]]$mean,
            imean = r$misc$configs$config[[1]]$improved.mean,
            mode = r$mode$x[-(1:n)]), 
      cbind(mean = rr$misc$configs$config[[1]]$mean,
            imean = rr$misc$configs$config[[1]]$improved.mean,
            mode = rr$mode$x[-(1:n)]), 
      cbind(mean = rrr$misc$configs$config[[1]]$mean,
            imean = rrr$misc$configs$config[[1]]$improved.mean,
            mode = rrr$mode$x[-(1:n)]))
nr <- nrow(A) %/% 3L
rownames(A) <- rep(c("GAUSSIAN", "VB[MEAN]", "VB[VARIANCE]"), each = nr)
round(dig = 4, A)

B <- rbind(sqrt(diag(r$misc$configs$config[[1]]$Qinv)),
           sqrt(diag(rr$misc$configs$config[[1]]$Qinv)),
           sqrt(diag(rrr$misc$configs$config[[1]]$Qinv)))
rownames(B) <- rep(c("GAUSSIAN", "VB[MEAN]", "VB[VARIANCE]"), each = 1)
colnames(B) <- paste0("x:", 1:ncol(B))
round(dig = 4, B)
           
