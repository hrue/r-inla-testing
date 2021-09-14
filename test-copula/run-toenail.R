load("toenail")
toenail$patientID = as.numeric(as.character(toenail$patientID))
toenail$treatment = as.numeric(toenail$treatment) -1 ## 0 or 1
toenail$outcome = as.numeric(toenail$outcome)-1
hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))
model = outcome ~ 1 + treatment*time + f(patientID, model="iid", hyper = hyper)
r = (inla(model, data = toenail, family = "binomial", 
          control.fixed = list(prec.intercept = 1E-4, prec = 1E-4),
          control.compute = list(config=TRUE),
          control.predictor = list(compute=TRUE), 
          control.inla = list(
              strategy = "simplified.laplace",
              force.diagonal = TRUE, 
              interpolator = "gridsum", 
              int.strategy = "grid",
              diff.logdens = 10,
              dz = 0.5, 
              correct = FALSE), 
          verbose=FALSE))
##r = inla.hyperpar(r, diff.logdens = 20, dz = 0.5)

model = outcome ~ 1 + treatment*time + f(patientID, model="iid", hyper = hyper)
rr = (inla(model, data = toenail, family = "binomial", 
           control.fixed = list(prec.intercept = 1E-4, prec = 1E-4),
           control.compute = list(config=TRUE),
           control.predictor = list(compute=TRUE), 
           control.mode = list(result = r,  restart=TRUE), 
           control.inla = list(
               strategy = "simplified.laplace",
               force.diagonal = TRUE, 
               interpolator = "gridsum", 
               int.strategy = "grid",
               diff.logdens = 10,
               dz = 0.5,
               correct = TRUE,
               correction.factor = 1), 
           verbose=FALSE))
##rr = inla.hyperpar(rr)
