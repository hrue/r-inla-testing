library(mvtnorm)
n = 10000
idx = 1:n
nstrata = 50
strata = sample(1:nstrata, n, replace=TRUE)
nsubject = n %/% nstrata
subject = sample(1:nsubject, n, replace=TRUE)
z = rnorm(n)
gam = c(1, 1 + rnorm(nstrata-1, sd = 0.2))

rho = 0.5
Sigma = matrix(c(1/1, NA, NA, 1/2), 2, 2)
Sigma[1,2] = Sigma[2,1] = rho*sqrt(Sigma[1,1]*Sigma[2,2])

aa = rmvnorm(nsubject, sigma=Sigma)
alpha = aa[,1]
beta = aa[,2]
s = 0.01
y = gam[strata] * (alpha[subject] + z * beta[subject]) + rnorm(n, s = 0.01)
r = inla(y ~ -1 + f(idx, model = "intslope",
                    args.intslope = list(subject = subject, strata = strata, covariates = z),
                    hyper = list("gamma1" = list(fixed=FALSE),
                                 "gamma2" = list(fixed=FALSE),
                                 "gamma3" = list(fixed=FALSE),
                                 "gamma4" = list(fixed=FALSE),
                                 "gamma5" = list(fixed=FALSE),
                                 "gamma6" = list(fixed=FALSE),
                                 "gamma7" = list(fixed=FALSE),
                                 "gamma8" = list(fixed=FALSE),
                                 "gamma9" = list(fixed=FALSE),
                                 "gamma10" = list(fixed=FALSE),
                                 "gamma11" = list(fixed=FALSE),
                                 "gamma12" = list(fixed=FALSE),
                                 "gamma13" = list(fixed=FALSE),
                                 "gamma14" = list(fixed=FALSE),
                                 "gamma15" = list(fixed=FALSE),
                                 "gamma16" = list(fixed=FALSE),
                                 "gamma17" = list(fixed=FALSE),
                                 "gamma18" = list(fixed=FALSE),
                                 "gamma19" = list(fixed=FALSE),
                                 "gamma20" = list(fixed=FALSE),
                                 "gamma21" = list(fixed=FALSE),
                                 "gamma22" = list(fixed=FALSE),
                                 "gamma23" = list(fixed=FALSE),
                                 "gamma24" = list(fixed=FALSE),
                                 "gamma25" = list(fixed=FALSE),
                                 "gamma26" = list(fixed=FALSE),
                                 "gamma27" = list(fixed=FALSE),
                                 "gamma28" = list(fixed=FALSE),
                                 "gamma29" = list(fixed=FALSE),
                                 "gamma30" = list(fixed=FALSE),
                                 "gamma31" = list(fixed=FALSE),
                                 "gamma32" = list(fixed=FALSE),
                                 "gamma33" = list(fixed=FALSE),
                                 "gamma34" = list(fixed=FALSE),
                                 "gamma35" = list(fixed=FALSE),
                                 "gamma36" = list(fixed=FALSE),
                                 "gamma37" = list(fixed=FALSE),
                                 "gamma38" = list(fixed=FALSE),
                                 "gamma39" = list(fixed=FALSE),
                                 "gamma40" = list(fixed=FALSE),
                                 "gamma41" = list(fixed=FALSE),
                                 "gamma42" = list(fixed=FALSE),
                                 "gamma43" = list(fixed=FALSE),
                                 "gamma44" = list(fixed=FALSE),
                                 "gamma45" = list(fixed=FALSE),
                                 "gamma46" = list(fixed=FALSE),
                                 "gamma47" = list(fixed=FALSE),
                                 "gamma48" = list(fixed=FALSE),
                                 "gamma49" = list(fixed=FALSE),
                                 "gamma50" = list(fixed=FALSE))), 
         control.inla = list(int.strategy = "eb", force.diagonal = TRUE), 
         data = list(y=y, idx=idx, subject=subject, strata=strata, z=z), 
         control.family = list(hyper = list(prec = list(initial = log(1/s^2), fixed=TRUE))), 
         verbose=TRUE)

plot(r$summary.hyperpar$mean[-c(1,2,3)], gam)
