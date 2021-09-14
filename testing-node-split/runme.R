data(Epil)
my.center = function(x)
    c(scale(x, center = TRUE, scale = FALSE))

## make centered covariates
Epil$CTrt    = my.center(Epil$Trt)
Epil$ClBase4 = my.center(log(Epil$Base/4))
Epil$CV4     = my.center(Epil$V4)
Epil$ClAge   = my.center(log(Epil$Age))
Epil$CBT     = my.center(Epil$Trt*Epil$ClBase4)
Epil$intercept = 1

formula = y ~ -1 + intercept + ClBase4 + CTrt + CBT+ ClAge + CV4 +
          f(Ind, model="iid") + f(rand, model="iid")

result = inla(formula,family="poisson", data = Epil)

## split by each individual
source("node-split.R")
res = node.split(formula, data = Epil, split.by = "Ind",
    family = "poisson",
    control.inla = list(strategy = "gaussian", int.strategy = "eb"))

for(i in 1:length(res)) {
    dev.new()
    par(mfrow=(c(2, 2)))
    kk = nrow(res[[i]]$within.group.linpred)
    for (k in seq_len(kk)) {
        x = res[[i]]$within.group.linpred[k, ]
        y = res[[i]]$between.group.linpred[k, ]
        xylim = range(c(x, y))
        plot(x, y, pch = k, xlim = xylim, ylim = xylim, 
             main = paste("split by", res[[i]]$split.by, "=", res[[i]]$split.value),
             xlab = "within group linpred",
             ylab = "between group linpred")
        abline(v = mean(x))
        abline(h = mean(y))
    }
}
