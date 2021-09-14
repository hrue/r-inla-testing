# generate R code for type IV interaction model in general using

## set time effect assuming an RW1¨
time.dim = 10
Q.rw1 = INLA:::inla.rw(time.dim, order=1, scale.model=TRUE, sparse=TRUE)

## set the spatial effect using one of three graph files
## load the desired graph file 
## a) Ohio reduced graph with 10 regions
## graph = "ohioReduced.txt"
## b) Ohio complete graph with 88 regions
graph = "ohioFull.txt"
## c) Germany graph with 544 regions
graph = "germany.txt"

g = inla.read.graph(graph)
Q.space = INLA:::inla.pc.bym.Q(g)
space.dim = dim(Q.space)[1]

# Assumed ordering:
## Time \otimes Space, firs runs the space index and then time. 

# set the indices
interact.dim = time.dim*space.dim
idx.time = rep(1:time.dim, each=space.dim)
idx.space = rep(1:space.dim, time.dim)
idx.inter = 1:interact.dim

# define the structure matrix for the interaction
Q.inter = Q.rw1 %x% Q.space

# Specifying constraints needed for Type IV Interaction
interact.constr <- matrix(0, time.dim+space.dim-1, interact.dim)
# first go through space and set constraints for time
for(i in 1:space.dim){
    interact.constr[i, seq(1, time.dim * space.dim, by=space.dim) + (i-1)] = 1
}
# go through time and set constraints for space
for(i in 1:(time.dim-1)){
  interact.constr[i+space.dim, 1:space.dim + (i-1)*space.dim] = 1
}

constr.inter <- list(A = interact.constr, e = rep(0, time.dim+space.dim-1))
rankdef.inter = time.dim + space.dim - 1

d <- 0.01
# set up the Type IV model 
formula.constr = y ~ 1 + 
    f(idx.time, model="rw1", constr=FALSE, rankdef=1, scale.model=TRUE,
      diagonal = d, 
    hyper = list(theta = list(prior = "pc.prec", param = c(3, .05),
                              initial = 1, fixed = FALSE))) +
    f(idx.space, model="besag", graph=Q.space, constr=FALSE, rankdef=1,
      diagonal = d, 
    hyper = list(theta = list(prior = "pc.prec", param = c(3, .05),
                              initial = 1, fixed = FALSE)))+
    f(idx.inter, model="generic0", Cmatrix = Q.inter,
      diagonal = d, 
      rankdef=rankdef.inter,
      hyper = list(theta = list(prior = "pc.prec", param = c(3, .05),
                              initial = 1, fixed = FALSE)))

# generate data
eff <- idx.time - mean(idx.time)
eff2 = .1*(idx.space - mean(idx.space))
prec.sc <- exp(rnorm(interact.dim, sd=2))
y <- 1 + eff + eff2 + rnorm(interact.dim, sd=1/sqrt(prec.sc))

# call inla
data = list(y=y, 
            idx.time=idx.time, 
            idx.space=idx.space, 
            idx.inter=idx.inter, 
            prec.sc=prec.sc)

r <- inla(formula.constr, family="gaussian",
          control.family = list(
              hyper = list(prec = list(initial = 0, fixed = FALSE))),
          control.fixed = list(prec.intercept = 1), 
          data = data,
          scale = prec.sc, 
          control.predictor = list(compute=TRUE),
          control.compute = list(config=TRUE),
          verbose = TRUE, keep = TRUE, 
          inla.call = "inla.mkl.work", 
          num.threads = "3:2",
          blas.num.threads = 2)

