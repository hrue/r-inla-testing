n <- 5
x <- seq(2, by = 2, length = n)
xx <- seq(3, by = 2, length = n)
y <- 1 + x + xx + rnorm(n, sd = 0.1)

n <- 15
x <- rnorm(n)
xx <- rnorm(n)
y <- 1 + x + xx + rnorm(n, sd = 0.1)

r <- inla(y ~ 1 + x + xx,
          data = data.frame(y, x, xx),
          family = "t", 
          verbose = TRUE, 
          control.fixed = list(prec.intercept = 1, prec = 1), 
          control.family = list(hyper = list(prec = list(initial = log(10), fixed = FALSE),
                                             dof = list(initial = 6, fixed = TRUE))), 
          inla.call = "inla.valgrind", inla.arg = "-v -t1:1 -b -P")

A <- cbind(1, x, xx)
round(dig = 3, A)
round(dig = 3, t(A))
round(dig = 3, t(A) %*% A)


### > A <- cbind(1, x, xx)
### 
### > round(dig = 3, A)
###         x xx
### [1,] 1  2  3
### [2,] 1  4  5
### [3,] 1  6  7
### [4,] 1  8  9
### [5,] 1 10 11
### 
### > round(dig = 3, t(A))
###    [,1] [,2] [,3] [,4] [,5]
###       1    1    1    1    1
### x     2    4    6    8   10
### xx    3    5    7    9   11
### 
### > round(dig = 3, t(A) %*% A)
###         x  xx
###     5  30  35
### x  30 220 250
### xx 35 250 285



### > round(dig = 3, A)
###        x
### [1,] 1 2
### [2,] 1 4
### [3,] 1 6

### > round(dig = 3, t(A))
###   [,1] [,2] [,3]
###      1    1    1
### x    2    4    6
### 
### > round(dig = 3, t(A) %*% A)
###       x
###    3 12
### x 12 56
