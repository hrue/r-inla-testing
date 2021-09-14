load("toenail")
toenail$patientID = as.numeric(as.character(toenail$patientID))
toenail$treatment = as.numeric(toenail$treatment) -1 ## 0 or 1
toenail$outcome = as.numeric(toenail$outcome)-1

model = outcome ~ 1 + treatment*time + f(patientID, model="iid", param = c(0.01, 0.01))

r = inla(model, data = toenail,
    ##family = "binomial", 
    family = "gaussian", control.family = list(hyper = list(prec = list(initial = -4, fixed=TRUE))), 
    control.fixed = list(prec.intercept = 1E-2, prec = 1E-2),
    control.compute = list(config=TRUE),
    control.predictor = list(compute=TRUE), 
    control.inla = list(
        int.strategy = "eb"), 
    verbose=FALSE)

rr = inla(model, data = toenail,
    ##family = "binomial", 
    family = "gaussian", control.family = list(hyper = list(prec = list(initial = -4, fixed=TRUE))), 
    control.fixed = list(prec.intercept = 1E-2, prec = 1E-2),
    control.compute = list(config=TRUE),
    control.predictor = list(compute=TRUE), 
    control.inla = list(
        correct = TRUE,
        correct.factor = 10.0,
        correct.verbose = TRUE,
        int.strategy = "eb"), 
    verbose=FALSE, keep=T)

summary(r)
summary(rr)

