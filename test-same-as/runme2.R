inla.my.update()
inla.setOption("inla.call", "inla.work")


n = 100
m = n-2
y = sin((1:n)*0.2) + rnorm(n, sd=0.1)

formula = Y ~ f(i, model="iid", initial=-10, fixed=T) +
              f(j, w, copy="i", fixed=FALSE) +
              f(k, copy="i", same.as = "j") +
              f(l, model ="iid") -1

Y = matrix(NA, n+m, 2)
Y[1:n,     1] = y
Y[1:m + n, 2] = 0

i = c(1:n, 3:n)             # x_t
j = c(rep(NA,n), 3:n -1)    # x_t-1
jj = c(rep(NA,n), 3:n -1)    # x_t-1
w = c(rep(NA,n), rep(-2,m)) # weights for j
k = c(rep(NA,n), 3:n -2)    # x_t-2
kk = c(rep(NA,n), 3:n -2)    # x_t-2
l = c(rep(NA,n), 1:m)       # v_t

r = inla(formula, data = data.frame(i,j,w,k,l,Y,jj,kk),
         family = rep("gaussian", 2),
         control.data = list(list(), list(initial=10, fixed=T)),
        verbose=T)
