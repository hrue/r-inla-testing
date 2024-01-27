pgev = function(y, xi) {
    qalpha <- 0
    alpha <- 0.5
    beta <- 0.5
    sbeta <- 1

    l1.xi <- (-log(alpha))^(-xi)
    l2.xi <- (-log(1-beta/2))^(-xi)
    l3.xi <- (-log(beta/2))^(-xi)

    pfrechet = function(y) {
        aux1 <- (y - qalpha) / (sbeta * (l2.xi - l3.xi)^(-1)) + l1.xi
        return (exp(-pmax(0, aux1^(-1/xi))))
    }

    return (pfrechet(y))
}
