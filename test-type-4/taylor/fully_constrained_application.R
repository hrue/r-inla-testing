INLA:::inla.my.update()
INLA:::inla.my.update()
inla.setOption(inla.call = "remote")


# Constraints project - The big model we want to fit for Malawi
# Author: Taylor Okonek

# load necessary packages
library(tidyverse)
library(rgdal)
library(SUMMER)
library(INLA)

# paths to the binomial dataframe, the graph file, and the shapefile
data_dir <- "binom_df_simul.csv"
shape_dir <- "gadm36_MWI_shp/gadm36_MWI_1.shp"

# Read in data ------------------------------------------------------------
binom_df <- read.csv(data_dir)

# rearrange so that the ordering precedent is: time, age, space
binom_df <- binom_df %>%
  arrange(time, region, age) # seems to arrange it by time, age, space even though it's in a different order

# read in map
geo <- readOGR(shape_dir, verbose = FALSE)
# subset to exclude Likoma
geo <- geo[geo$NAME_1 != "Likoma",]
Amat <- getAmat(geo, geo$REGNAME)

# Create data for INLA ----------------------------------------------------

# create INLA data
inla_dat <- list(y = binom_df$y,
                 Ntrials = binom_df$n,
                 idx = as.numeric(binom_df$region),
                 age = binom_df$age + 1, # add 1 because it ranges from 0 - 11, but we need them to start at 1
                 time = factor(binom_df$time) %>% as.numeric(),
                 urban = binom_df$urban)


# Create precision matrices and constraints for all interaction terms --------

# create ICAR matrix from Amat
k.theta <- Amat * -1
diag(k.theta) <- (rowSums(k.theta) * -1)
rowSums(k.theta) # double check that rows and cols sum to zero
colSums(k.theta)

# get all the basics we need for the kronecker products
inla.rw = utils::getFromNamespace("inla.rw", 
                                  "INLA")
# get unscaled to use in bym2 for time
RW_prec <- inla.rw(n = length(unique(binom_df$time)), order = 1, scale.model = FALSE, 
                   sparse = TRUE)
scaled_RW_prec <- inla.rw(n = length(unique(binom_df$time)), order = 1, scale.model = TRUE, 
                   sparse = TRUE)
# now get RW_prec for age
RW_prec_age <- inla.rw(n = length(unique(binom_df$age)), order = 1, scale.model = FALSE, 
                   sparse = TRUE)
scaled_RW_prec_age <- inla.rw(n = length(unique(binom_df$age)), order = 1, scale.model = TRUE, 
                              sparse = TRUE)
ICAR_prec <- INLA::inla.scale.model(k.theta, constr = list(A = matrix(1, 
                                                                      1, dim(k.theta)[1]), e = 0))

N = length(unique(binom_df$region))
S = length(unique(binom_df$time))
Age_num = length(unique(binom_df$age))

# function for getting constraints from a precision matrix (using eigenvectors)
get_constraints <- function(mat) {
  test <- eigen(mat)
  # get eigenvectors that correspond to zero eigenvalues for constraint matrix
  zero_eigen_ids <- which(test$values < 0.00001)
  tmp <- t(test$vectors[, zero_eigen_ids]) 
  constr <- list(A = tmp, e = rep(0, dim(tmp)[1]))
  return(constr)
}

## Time X Space
R.time.space <- scaled_RW_prec %x% ICAR_prec
constr.time.space <- get_constraints(R.time.space)

# get variable for time x space interaction
temp <- expand.grid(unique(binom_df$region), unique(binom_df$time))
temp$time.space <- 1:nrow(temp)
colnames(temp) <- c("region", "time", "time.space")
inla_dat$time.space <- left_join(binom_df, temp) %>% dplyr::select(time.space) %>% unlist() %>% unname()

## Time X Age
R.time.age <- scaled_RW_prec %x% scaled_RW_prec_age
constr.time.age <- get_constraints(R.time.age)

# get variable for time x age interaction
temp <- expand.grid(unique(binom_df$age), unique(binom_df$time))
temp$time.age <- 1:nrow(temp)
colnames(temp) <- c("age", "time", "time.age")
inla_dat$time.age <- left_join(binom_df, temp) %>% dplyr::select(time.age) %>% unlist() %>% unname()

## Age X Space
R.age.space <- scaled_RW_prec_age %x% ICAR_prec
constr.age.space <- get_constraints(R.age.space)

# get variable for age x space interaction
temp <- expand.grid(unique(binom_df$region), unique(binom_df$age))
temp$age.space <- 1:nrow(temp)
colnames(temp) <- c("region", "age",  "age.space")
inla_dat$age.space <- left_join(binom_df, temp) %>% dplyr::select(age.space) %>% unlist() %>% unname()

# Kronecker product between RW1_time x RW1_age x ICAR
R.time.age.space <- scaled_RW_prec %x% scaled_RW_prec_age %x% ICAR_prec
constr.time.age.space <- get_constraints(R.time.age.space)

# get variable for time x age x space interaction
temp <- expand.grid(unique(binom_df$region), unique(binom_df$age), unique(binom_df$time))
temp$time.age.space <- 1:nrow(temp)
colnames(temp) <- c("region", "age","time" , "time.age.space")
inla_dat$time.age.space <- left_join(binom_df, temp) %>% dplyr::select(time.age.space) %>% unlist() %>% unname()


diag.value <- 1e-2

# Fit model ---------------------------------------------------------------

# create formula
formula <- y ~ 1 + urban +
  f(idx, model="bym2", graph=k.theta, scale.model = TRUE, constr = TRUE,
    rankdef = 1, diagonal = diag.value,
    hyper = list(
      phi = list(
        prior = "pc",
        param = c(0.5, 2/3)),
      prec = list(
        prior = "pc.prec",
        param = c(1, 0.01)))) +
  f(time, model = "bym2", graph=RW_prec, scale.model = TRUE, constr = TRUE, 
    rankdef = 1, diagonal = diag.value,
    hyper = list(
      phi = list(
        prior = "pc",
        param = c(0.5, 2/3)),
      prec = list(
        prior = "pc.prec",
        param = c(1, 0.01)))) +
  f(age, model = "bym2", graph=RW_prec_age, scale.model = TRUE, constr = TRUE, 
    rankdef = 1, diagonal = diag.value,
    hyper = list(
      phi = list(
        prior = "pc",
        param = c(0.5, 2/3)),
      prec = list(
        prior = "pc.prec",
        param = c(1, 0.01)))) +
  f(time.space, 
    model = "generic0", Cmatrix = R.time.space, extraconstr = constr.time.space,
    diagonal = diag.value, 
    rankdef = N + S - 1) +
  f(time.age,
    model = "generic0", Cmatrix = R.time.age, extraconstr = constr.time.age,
    diagonal = diag.value, 
    rankdef = Age_num + S - 1) +
  f(age.space,
    model = "generic0", Cmatrix = R.age.space, extraconstr = constr.age.space,
    diagonal = diag.value, 
    rankdef = Age_num + N - 1) +
  f(time.age.space,
    model = "generic0", Cmatrix = R.time.age.space, extraconstr = constr.time.age.space,
    diagonal = diag.value, 
    rankdef = N * Age_num + S * Age_num + N * S - N - S - Age_num + 1) 

ptm <- Sys.time()
mod <- inla(formula,
            data = inla_dat,
            family = "betabinomial",
            ##control.mode = list(theta = c(-3.3632, 3.8731, -2.8977,
            ##                              5.1300, -2.8627, 5.3662,
            ##                              -2.7191, 8.1767, 7.8420,
            ##                              8.9217, 10.3805),
            ##                    restart = TRUE), 
            Ntrials = Ntrials,
            control.fixed = list(prec.intercept = 1, prec = 1),
            control.compute = list(config = TRUE),
            inla.arg = "-v -b -t22:4 -P -B1", 
            verbose = TRUE)
            
print(Sys.time()-ptm)

