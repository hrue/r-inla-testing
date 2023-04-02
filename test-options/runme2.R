n <- 100
x <- rnorm(n)
y <- rpois(n, exp(0 + x + rnorm(n)))
idx <- 1:n

r <- inla(y ~ 1 + x + f(idx),
          data = data.frame(y, x, idx), 
          family = "poisson",
          verbose = TRUE, 
          control.expert = list(dot.product.gain = TRUE))

## look for the line in the output (at the end)
## Dot-product gain: 0.000 seconds, 0.000001 seconds/fn-call
