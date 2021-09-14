# packages
library(INLA)
library(INLAMSM) # from devtools::install_github("becarioprecario/INLAMSM")

# data
my_data <- readRDS("data.RDS")
W <- readRDS("W.RDS")

# number of severities
k <- 2 

# Define the sum-to-zero constraints
A <- kronecker(Diagonal(k, 1), Matrix(1, ncol = nrow(W), nrow = 1))
e <- rep(0, k)

# Define the spatial random effect
my_effect <- inla.IMCAR.model(k = k, W = W)


INLA:::inla.my.update()
INLA:::inla.my.update()



# Run the model
inla(
  formula = number_of_car_crashes ~ 0 + 
    intercept_severe + intercept_slight + 
    edge_betweenness_severe + edge_betweenness_slight + 
    CLASSIFICA_severe_motorway + CLASSIFICA_severe_primary_road +
    CLASSIFICA_slight_motorway + CLASSIFICA_slight_primary_road + 
    perc_lav_severe + perc_lav_slight + 
    dens_pop_severe + dens_pop_slight + 
    f(iid_severe) + f(iid_slight) + 
    f(
      idx,
      model = my_effect,
      extraconstr = list(A = as.matrix(A), e = e)
    ),
  family = "poisson",
  data = my_data,
  E = road_length * total_traffic,
  control.compute = list(dic = TRUE, waic = TRUE),
  twostage = FALSE, num.threads = "1:1", verbose = TRUE, keep = TRUE
)

inla.version()
