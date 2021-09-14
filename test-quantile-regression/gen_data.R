##############################################################
## Functions
##############################################################
ilogit <- function(x){ 1/(1+exp(-x)) }
logit <-function(x){ log(x/(1-x)) }

##############################################################
## Kumaraswamy distribution
##############################################################
rku <- function(n, q, kappa, delta){
   u <- runif(n)
   y <- ( 1- (1-u)^(log(1 - exp(-1/delta))/log(1-q)) )^{-delta*log(kappa)}
   return(y)
}

##############################################################
## JSB distribution
##############################################################
rsb <- function(n, q, kappa, delta){
   u <- runif(n)
   A <- kappa*exp( (pnorm(u)-pnorm(q))/delta )/( 1 - kappa )
   y <- 1/(1/A + 1)
   return(y)
}

##############################################################
## Generate Data
##############################################################
set.seed(19832)
n <- 1000 #sample size
q <- 0.5 #quantile of interest
x <- rnorm(n) #covariate
kappa <- ilogit(1+x)
delta <- 1

y.k <- rku(n,q,kappa,delta)
y.s <- rsb(n,q,kappa,delta)

