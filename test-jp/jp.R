jp = function(theta) {
    ## you have to check and KNOW the order of the hyperparameters, as their names are not
    ## passed along... so you need to check this upfront.
    param.1 = c(1, 0.1)  ## params for the precision for the observations
    param.2 = c(1, 0.01) ## params for the precision of the iid term

    ## return the sum of all log-priors on the theta-scale. the 'theta[1]' is the log
    ## jacobian...
    lprior = (dgamma(exp(theta[1]), shape = param.1[1], rate = param.1[2], log=TRUE) +
              theta[1] +
              dgamma(exp(theta[2]), shape = param.2[1], rate = param.2[2], log=TRUE) +
              theta[2])

    print(paste("call 'jp' with theta: ", theta, " return ", lprior, sep="",  collapse=" "))
    return (lprior)
}
