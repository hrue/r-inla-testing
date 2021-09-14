dw = function(lambda, y, alpha, variant = 0, log=TRUE) {
    if (variant == 0) {
        ##return (y^(alpha-1)*lambda*exp(-lambda*y^alpha))
        return(log((y^(alpha-1)*lambda*exp(-lambda*y^alpha))))
    } else {
        ##return (y^(alpha-1)*lambda^alpha*exp(-(lambda*y)^alpha))
        return (log(y^(alpha-1)*lambda^alpha*exp(-(lambda*y)^alpha)))
    }
}

eta = seq(-3, 3, by = 0.01)
lambda = exp(eta)
alpha = 1.4
y = 2
ld = dw(exp(eta), y=1, alpha, variant=1)
plot(eta, ld, type="l")
all(diff(ld, diff=2) < 0)
