## A simple model
n = 100
a = rnorm(n)
b = rnorm(n)
idx = 1:n

y = rnorm(n) + a + b 
formula = y ~ 1 + a + b + f(idx, model="iid")

## assume we want to compute the posterior for
##
##  2 * beta_a + 3 * beta_b + idx[1] - idx[2]
##
## which we specify as follows (and giving it a unique name)

lc1 = inla.make.lincomb(a=2, b=3, idx = c(1,-1,rep(NA,n-2)))
names(lc1) = "lc1"

## strictly speaking, it is sufficient to use `idx = c(1,-1)', as the
## remaining idx's are not used in any case.

r = inla(formula, data = data.frame(a,b,y),
        ##
        ## add the linear combinations here
        lincomb = lc1,

        ##
        ## force noise variance to be essiatially zero
        ##
        control.family = list(initial=10, fixed=TRUE))

## to verify the result, we can compare the mean but the variance and
## marginal cannot be computed from the simpler marginals alone.

lc1.1 = 2 * r$summary.fixed["a", "mean"] + 3 * r$summary.fixed["b", "mean"] +
    r$summary.random$idx$mean[1] - r$summary.random$idx$mean[2]

lc1.2= r$summary.lincomb.derived["lincomb.lc1.derived", "mean"]
print(c(lc1.1 = lc1.1, lc1.2 = lc1.2))

## the marginals are available as `r$marginals.lincomb$...'.

## we can derive the lincomb above more accurately at a higher cost,
## by adding a new node to the graph, using derived.only=FALSE. Thus
## the lincomb can be derived using the simplified or full laplace
## approximation. For the Gaussian case here, they are all the same...

r = inla(formula, data = data.frame(a,b,y),
        lincomb = lc1,
        control.family = list(initial=10, fixed=TRUE))

lc1.3= r$summary.lincomb["lincomb.lc1", "mean"]
print(c(lc1.1 = lc1.1, lc1.2 = lc1.2, lc1.3 = lc1.3))

## let us compute the posterior for
##
##  linear.predictor[2] + intercept 
##
## which we specify as follows (and giving it a unique name).
##
## NOTE: The linear predictor has a predefined name = "Predictor", and
## the intercept is named "(Intercept)" (as lm() does, don't blame
## me!).

lc2 = inla.make.lincomb(Predictor = c(NA,1, rep(NA,n-2)), "(Intercept)" = 1)
names(lc2) = "lc2"

## same here, `Predictor = c(NA,1)' is sufficient. We need to spesify
## them all the way until the last !is.na().

## to use more than one linear combination, we can spesify more than
## one using the c() operator
all.lc = c(lc1,lc2)

r = inla(formula, data = data.frame(a,b,y),
        lincomb = all.lc,
        control.predictor = list(compute=TRUE),
        control.family = list(initial=10, fixed=TRUE),
        control.inla = list(lincomb.derived.correlation.matrix=TRUE),
        keep = TRUE,
        safe = FALSE, 
        verbose = T)

lc2.1 = r$summary.linear.predictor$mean[2] + r$summary.fixed["(Intercept)", "mean"]
lc2.2 = r$summary.lincomb["lincomb.lc2.derived", "mean"]

print(c(lc2.1, lc2.1))

## There is an another function which is handy for specifying many
## linear combinations at once, that is inla.make.lincombs() [note the
## plural 's'].  Here each 'column' define one linear combination
##
## let wa and wb be vectors, and we want to compute the marginals for
##
## beta_a * wa[i] + beta_b * wb[i], for i=1..m. this is done
## conveniently as follows

m = 10
wa = runif(m)
wb = runif(m)
lc.many = inla.make.lincombs(a = wa, b=wb)

## we can give them names as well, but there are also default names, like

print(names(lc.many))

## if we want to join in the previous ones, as well, we can add them
## using c()
r = inla(formula, data = data.frame(a,b,y),
    lincomb = c(lc.many, all.lc),
    control.family = list(initial=10, fixed=TRUE))

print(r$summary.lincomb.derived[, "mean"])


## if 'wa' and 'wb' is defined in a matrix, like
A = cbind(a=wa, b=wb)

lc.many = inla.make.lincombs(a = A[, "a"],  b = A[, "b"])

## we can join this trick with "(Intercept) = wi" using (do not ask
## why it is like this: read the code instead. If someone wants to fix
## it, please do!)
wi = runif(m)
lc.most = inla.make.lincombs(a = A[, "a"],  b = A[, "b"], "(Intercept)" = wi)


## terms like 'idx' above, can be added as idx = IDX into
## inla.make.lincombs(), where IDX is a matrix. Again, each column of
## the arguments define one linear combination. 



