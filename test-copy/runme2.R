n = 1000
N = 4*n
time = 1:n
x = sin(time/n*4*pi)
age = rep(1:4, each=n)

s <- 0.01
y = numeric(N)
y[ which(age==1) ] = x 
y[ which(age==2) ] = 2*x 
y[ which(age==3) ] = -x 
y[ which(age==4) ] = -2*x 

y <- y + rnorm(N, sd = s)

formula = y ~ -1 + f(i, model="rw2", scale.model = TRUE) +
    f(j, copy="i", hyper = list(beta = list(fixed=FALSE))) + 
    f(k, copy="i", hyper = list(beta = list(fixed=FALSE))) + 
    f(l, copy="i", hyper = list(beta = list(fixed=FALSE))) 

i = rep(NA, N)
i[which(age == 1)] = time
j = rep(NA, N)
j[which(age == 2)] = time
k = rep(NA, N)
k[which(age == 3)] = time
l = rep(NA, N)
l[which(age == 4)] = time

r = inla(formula,  data = data.frame(y, i, j, k, l),  verbose=TRUE,
        family = "gaussian",
        control.family =  list(hyper = list(prec = list(initial = log(1/s^2),
                                                        fixed = TRUE))), 
        control.predictor = list(compute=TRUE))

