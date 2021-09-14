set.seed(123)

ec = NULL
ec = list(A=matrix(1:20, 2, 10), e=c(-10, 100))
formula = y ~ -1 + f(idx, model="linear") + f(jdx, model="ar1", values=1:10, extraconstr = ec) + f(kdx, model="iid") + x

n = 1
y = rnorm(n)
idx = rep(1, n)
jdx = 1:n
kdx = 1
x = rnorm(n)

y[] = NA
r.pre = inla(formula, data =data.frame(y, idx, jdx, x),
        control.predictor = list(initial = 2),
        control.compute = list(config = FALSE))
r = inla(formula, data =data.frame(y, idx, jdx, x),
        control.predictor = list(initial = 2), 
        control.compute = list(q=TRUE, graph=TRUE, config=FALSE), 
        ##control.inla=list(reordering = "reverseidentity", int.strategy="eb"), 
        control.inla=list(reordering = "amdc", int.strategy="grid"), 
        control.mode = list(result = r.pre,  restart=FALSE), 
        verbose=TRUE)

