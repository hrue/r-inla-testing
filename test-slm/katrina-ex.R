#
#Example to show dependence on rho.min with a spatial probit model
#
library(INLA)

zero.variance = list(prec=list(initial = 10, fixed=TRUE))

#PACKAGE SPATIALPROBIT
#Here I use the katrina dataset from spatialprobit

library(spatialprobit)
data(Katrina)

h = 0.01

#And index for slm model
Katrina$idx<-1:nrow(Katrina)

#Model matrix for SLM models
mm <- model.matrix(y1~ 1+flood_depth+log_medinc+small_size+large_size+
   low_status_customers+high_status_customers+owntype_sole_proprietor+
   owntype_national_chain,
   Katrina)

# (a) 0-3 months time horizon
# LeSage et al. (2011) use k=11 nearest neighbors in this case
nb <- knn2nb(knearneigh(cbind(Katrina$lat, Katrina$long), k=11))
listw <- nb2listw(nb, style="W")
W1 <- as(as_dgRMatrix_listw(listw), "CsparseMatrix")


#Compute eigenvalues for SLM model (as in Havard's code)
e1 = eigen(W1)$values
re.idx1 = which(abs(Im(e1)) < 1e-6)
rho.max1 = 1/max(Re(e1[re.idx1]))
rho.min1 = 1/min(Re(e1[re.idx1]))
rho1 = mean(c(rho.min1, rho.max1))

#
#Variance-covarinace matrix for beta coeffients' prior
#
betaprec1<-.0001
#Standard regression model
Q.beta1 = diag(1,ncol(mm))
Q.beta1 = betaprec1*Q.beta1 %*% t(Q.beta1)

rho1.range = c(0, rho.max1)
rho2.range = c(-1, rho.max1)

#Model 1: rho.min = -1
m1 = inla(y1~ -1 +
        f(idx, model="slm",
        args.slm = list(
                rho.min = rho1.range[1], 
                rho.max = rho1.range[2],
                W = W1,
                X = mm,
                Q.beta = Q.beta1),
        hyper = list(
                prec = list(
                        initial=log(1),#This is fixed for identifiability reasons
                        fixed=TRUE),
#                        prior = "loggamma",
#                        param = c(1, 0.1)),
                rho = list(
#                       initial=log(0.3820/(1-0.3820)),
#                       fixed=TRUE))),
                        prior = "logitbeta",
                        param = c(1, 1)))),
#                        prior = "gaussian",
#                        param = c(0, 0.2)))),
        data = Katrina,
        family = "binomial",
        control.family = list(link = "probit"), 
        control.compute=list(dic=TRUE),
        control.inla = list(h = h), 
        verbose=FALSE)
m1 = inla.hyperpar(m1, dz=0.25, diff.logdens = 10)

#Model 1: rho.min = 0
m2 = inla(y1~ -1 +
        f(idx, model="slm",
        args.slm = list(
                rho.min = rho2.range[1],
                rho.max = rho2.range[2], 
                W = W1,
                X = mm,
                Q.beta = Q.beta1),
        hyper = list(
                prec = list(
                        initial=log(1),#This is fixed for identifiability reasons
                        fixed=TRUE),
#                        prior = "loggamma",
#                        param = c(1, 0.1)),
                rho = list(
#                       initial=log(0.3820/(1-0.3820)),
#                       fixed=TRUE))),
                        prior = "logitbeta",
                        param = c(1, 1)))),
#                        prior = "gaussian",
#                        param = c(0, 0.2)))),
        data = Katrina,
        family = "binomial",
        control.family = list(link = "probit"), 
        control.compute=list(dic=TRUE),
        control.inla = list(h = h), 
        verbose=FALSE)
m2 = inla.hyperpar(m2, dz=0.25, diff.logdens = 10)

#Summary of beta 
m1coef<-m1$summary.random$idx[-(1:673), 2:3]
rownames(m1coef)<-colnames(mm)
m1coef

m2coef<-m2$summary.random$idx[-(1:673), 2:3]
rownames(m2coef)<-colnames(mm)
m2coef

#Rho // hrue: cannot compare them like this, as they refer to
#different scale
##m1$summary.hyperpar[,1:2]
##m2$summary.hyperpar[,1:2]

## convert both marginals for rho into a common scale (rho.min, rho.max)=c(0, 1)

## -1, rho.max1
mn=rho1.range[1]
mx=rho1.range[2]
rho1 = inla.tmarginal(function(x) mn + (mx-mn)*x,  m1$marginals.hyperpar[[1]])
## 0,  rho.max1
rho2 = inla.tmarginal(function(x) mn + (mx-mn)*x,  m2$marginals.hyperpar[[1]])

plot(rho1, type="l")
lines(rho2, lwd=2)

