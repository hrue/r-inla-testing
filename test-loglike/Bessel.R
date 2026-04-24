
Sys.unsetenv("PKG_LIBS")
system("R CMD SHLIB bessel-demo2.c")

library(INLA) #
library(spdep)
library(MASS)
library(tidyverse)
# source("D:/CodigosC/CodigosR/IC/ic_bayes/funcoes_uteis.R")
setwd("D:/CodigosC/CodigosR/IC/ic_bayes/INLA/Sim/Bessel")
set.seed(1234)


n = 188
real_kap = c(0.3, 1.6, 0.5)
real_lam <- c(3, 0.5, 1.5)
x1 <- rbinom(n,1,0.5)
x2 <- runif(n,-1,1)
X <- as.matrix(tibble(rep(1,n),x1,x2))
V <- as.matrix(tibble(rep(1,n),x1,x2))

dados = bbreg::simdata_bes(real_kap,
                           real_lam,
                           X,V,
                           link.mean = "logit",
                           link.precision = "log") # bbreg is the package for bessel
dados = unlist(dados)
hist(dados)

df_inla <- list(
  Y = I(inla.mdata(cbind(dados, x1, x2))),
  x1 = x1,
  x2 = x2
)

caminho_dll <- file.path(getwd(), "bessel-demo2.dll")
cloglike <- inla.cloglike.define(model = "inla_cloglike_bessel", 
                                 shlib = caminho_dll)

formula = Y ~ x1 + x2
r = inla(formula,
         data = df_inla, 
         family = "cloglike",
         control.family = list(cloglike = cloglike),
         verbose = TRUE
)
summary(r)
