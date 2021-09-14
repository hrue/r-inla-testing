
pZ <- function(z, rho, df) {
    rrho <- sqrt(rho)
    zz <- sqrt(z)
    if(rho == 0.0) {
        ##return (pnorm(1/sqrt(2)*(zz -df) - (df-1)/2.0 / zz))
        return (pnorm(sqrt(2)*(zz -rrho)))
    } else {
        return (pnorm((zz -df) - (df-1)/2.0 * log(zz/rrho)/(zz-rrho)))
    }
}

dZ <- function(z, rho, df) {
    h <- 1e-4
    return (log((pZ(z+h, rho, df) - pZ(z-h, rho, df)) / (2.0 * h)))
}

z <- 200.1
rho <- 0.00001
df <- 100

print(c(pchisq(z, df = df, ncp = rho),  pZ(z, rho, df)))
print(c(dchisq(z, df = df, ncp = rho, log = TRUE),  dZ(z, rho, df)))


