require(INLA)
nsites <- 200
idx = 1:nsites
Ntrials <- rep(20,nsites)
b <- rnorm(n=1)
f <- 1/( 1 + exp(-(b)))
z <- rbinom(n=nsites,size=Ntrials,prob=f)
                    


res <- inla(formula=z~1, 
            family="binomial",
            Ntrials=Ntrials,
            data = data.frame(z=z, idx=idx),
            control.data = list(link="logit",
              hyper = list(prec = list(prior="loggamma",param=c(0.1,0.1)))),
            control.fixed= list(prec.intercept = 1),
            control.compute = list(hyperpar=FALSE,return.marginals=FALSE,mlik=TRUE),
            control.inla=list(strategy = "laplace", npoints = 100),
            verbose=TRUE)
res$mlik

b
