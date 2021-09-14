library(INLA)

## the default value of lambda in the PC prior
lambda.default = inla.models()$link$sn$hyper$theta$param

## this gives this historgram for the skewness parameter.
## whic is the cube root of what normally is the skewness:
## see inla.doc("sn", section="link")

xx = seq(-5, 5,  by = 0.001)
plot(xx, inla.pc.dsn(xx, lambda = lambda.default),
     type = "l",  lwd=3)
