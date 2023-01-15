prior.function = function(log_precision) {
    a = 1;
    b = 0.1;
    precision = exp(log_precision);
    logdens = log(b^a) - lgamma(a) + (a-1)*log_precision - b*precision;
    log_jacobian = log_precision;
    return(logdens + log_jacobian)
}

lprec = seq(-10, 10, len=1000)
prior.table = paste(c("table:", cbind(lprec, prior.function(lprec))),sep = "", collapse = " ")
n = 100
y = rnorm(n)

r = inla(y~1,
         data = data.frame(y),
         control.family = list(
             hyper = list(
                 prec = list(
                     prior = prior.table))))

