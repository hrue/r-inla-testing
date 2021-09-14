library(INLA)

ratsdata <- structure(list(t = c(8, 15, 22, 29, 36), tbar = 22, N = 30, T = 5, 
    Omega = structure(c(200, 0, 0, 0.2), .Dim = c(2L, 2L)), mean = c(0, 
    0), prec = structure(c(1e-06, 0, 0, 1e-06), .Dim = c(2L, 
    2L)), y = structure(c(151, 145, 147, 155, 135, 159, 141, 
    159, 177, 134, 160, 143, 154, 171, 163, 160, 142, 156, 157, 
    152, 154, 139, 146, 157, 132, 160, 169, 157, 137, 153, 199, 
    199, 214, 200, 188, 210, 189, 201, 236, 182, 208, 188, 200, 
    221, 216, 207, 187, 203, 212, 203, 205, 190, 191, 211, 185, 
    207, 216, 205, 180, 200, 246, 249, 263, 237, 230, 252, 231, 
    248, 285, 220, 261, 220, 244, 270, 242, 248, 234, 243, 259, 
    246, 253, 225, 229, 250, 237, 257, 261, 248, 219, 244, 283, 
    293, 312, 272, 280, 298, 275, 297, 350, 260, 313, 273, 289, 
    326, 281, 288, 280, 283, 307, 286, 298, 267, 272, 285, 286, 
    303, 295, 289, 258, 286, 320, 354, 328, 297, 323, 331, 305, 
    338, 376, 296, 352, 314, 325, 358, 312, 324, 316, 317, 336, 
    321, 334, 302, 302, 323, 331, 345, 333, 316, 291, 324), .Dim = c(30L, 
    5L))), .Names = c("t", "tbar", "N", "T", "Omega", "mean", 
"prec", "y"))

inla_data <- data.frame(
  y = as.vector(ratsdata$y),
  tc = rep(ratsdata$t - mean(ratsdata$t), each=ratsdata$N), ## using centered t
  i1 = rep(1:ratsdata$N,ratsdata$T),                        ## rat index
  i2 = ratsdata$N + rep(1:ratsdata$N,ratsdata$T),           ## "copy-rat" index
  intercept = 1                                             ## explicit intercept 
)
inla_formula <- 
  y ~ -1 + intercept + tc +
  f(i1, model="iid2d", n=2*30,
    hyper = list(theta1 = list(param = c(3, 200, .2, 0)))) +
  f(i2, tc, copy="i1")
 


p.rats <- inla.cd(debug=TRUE,
    formula = inla_formula,
    family = "gaussian",
    data = inla_data,
    split.by = "i1",
    control.predictor = list(compute=TRUE),
        control.family = 
        list(hyper = list(theta = list(prior="loggamma", param=c(.001,.001)))),
    control.fixed = list(prec.intercept = 1E-6, prec = 1E-6),
    control.inla = list(h=0.03)
)

p2.rats <- inla.cut(debug=TRUE,
    formula = inla_formula,
    family = "gaussian",
    data = inla_data,
    split.by = "i1",
    control.predictor = list(compute=TRUE),
        control.family = 
        list(hyper = list(theta = list(prior="loggamma", param=c(.001,.001)))),
    control.fixed = list(prec.intercept = 1E-6, prec = 1E-6),
    control.inla = list(h=0.03))

