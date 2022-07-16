library(INLA)
inla.setOption(inla.mode = "experimental")

data(Germany)
g = system.file("demodata/germany.graph", package="INLA")
source(system.file("demodata/Bym-map.R", package="INLA"))
summary(Germany)

Germany = cbind(Germany,region.struct=Germany$region)

formula3 = Y ~ f(region.struct,model="besag",graph=g) +
               f(region,model="iid") + f(x, model="rw2")

n <- 544
A <- matrix(rnorm(n^2) * rbinom(n^2, prob = 0.01,  size = 1), n, n)
diag(A) <- 1
result3 =  inla(formula3,family="poisson",
                data=Germany,
                E=E,
                verbose = TRUE, 
                num.threads = "4:1", 
                keep = TRUE, 
                inla.call = "inla.mkl.work",
                control.predictor = list(A = A), 
                control.inla = list(int.strategy = "eb"))


