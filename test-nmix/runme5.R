# libraries
library(INLA)
library(unmarked)

# data
data(mallard)
mallard.umf <- unmarkedFramePCount(y = mallard.y, siteCovs = mallard.site,
                                   obsCovs = mallard.obs)
mallard.umf[1:6, ]
length <- mallard.site[ , "length"]
elev <- mallard.site[ , "elev"]
forest <- mallard.site[ , "forest"]
mean.ivel <- rowMeans(mallard.obs$ivel, na.rm = T)
mean.ivel[is.na(mean.ivel)] <- mean(mean.ivel, na.rm = T)
mean.date <- rowMeans(mallard.obs$date, na.rm = T)
mean.date.sq <- mean.date^2
mallard.inla.df <- data.frame(y1 = mallard.y[ , "y.1"],
                              y2 = mallard.y[ , "y.2"],
                              y3 = mallard.y[ , "y.3"],
                              length, elev, forest, mean.ivel,
                              mean.date, mean.date.sq)
round(head(mallard.inla.df), 3) 
counts.and.count.covs <- inla.mdata(mallard.y, 1, length, elev, forest)

# inla result, CRASHES
out.inla.2 <- inla(counts.and.count.covs ~ 1 + mean.ivel + mean.date + mean.date.sq,
                      data = list(counts.and.count.covs = counts.and.count.covs,
                                  mean.ivel = mallard.inla.df$mean.ivel,
                                  mean.date = mallard.inla.df$mean.date,
                                  mean.date.sq = mallard.inla.df$mean.date.sq),
                   family = "nmixnb", 
                   control.inla = list(cmin = 0))
## if we want to rerun from fitted model without 'cmin=0',  we can this:
out.inla.2$control.inla$cmin <- -Inf
out.inla.2 <- inla.rerun(out.inla.2)

summary(out.inla.2, digits = 3)
