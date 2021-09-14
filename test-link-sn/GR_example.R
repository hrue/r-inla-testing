hosp = read.table("hospital.txt", header = TRUE)

hyperp <- list(prec = list(prior = "pc.prec", param = c(1, 0.01)))
formula <- hospital ~ loginc + distance + dropout + college +
                      f(group, model="iid", hyper=hyperp)
fit <- inla(formula,data=hosp,family="binomial",
            control.fixed = list(prec.intercept = 10,
                                 prec = 10), 
            control.family = list(
                control.link = list(
                    model = "sn",
                    hyper = list(alpha = list(
                                     initial = 5, 
                                     fixed = FALSE, 
                                     param = 1)))), 
            verbose=TRUE)
plot(fit,  plot.prior = TRUE)

