INLA:::inla.my.update(b = T)
n <- 100
x <- rnorm(n)
xx <- rnorm(n)
xxx <- rnorm(n)
y <- rnorm(n)

r <- inla(y ~ 1 + x*xx*xxx,
          data = data.frame(y, x, xx, xxx),
          control.compute = list(smtp = "stiles"),
          control.stiles = list(
              block.size = -1,
              tile.size = -1,
              tile.type = "semisparse",
              param = rep(-1, 32)),
          verbose = TRUE)

              


