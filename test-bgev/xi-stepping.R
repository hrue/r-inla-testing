library(mvtnorm)
library(sf)
library(sp)
library(INLA)


# Some functions used in the script =============================================
qgev = function(p, μ, σ, ξ) {
  if (length(ξ) == 1) ξ = rep(ξ, length(p))
  μ - σ * ifelse(ξ == 0, log(-log(p)), (1 / ξ) * (1 - (- log(p)) ^ (-ξ)))
}

rgev = function(n, μ, σ, ξ) qgev(runif(n), μ, σ, ξ)

bgev2gev = function(q, s, ξ, α = .5, β = .25) {
  if (length(ξ) == 1) ξ = rep(ξ, max(length(q), length(s)))
  μ = q - s * ℓ(α, ξ) / (ℓ(1 - β / 2, ξ) - ℓ(β / 2, ξ))
  σ = s / (ℓ(1 - β / 2, ξ) - ℓ(β / 2, ξ)) * ifelse(ξ == 0, 1, ξ)
  list(μ = μ, σ = σ, ξ = ξ)
}

ℓ = function(x, ξ) ifelse(ξ == 0, -log(-log(x)), (-log(x)) ^ (-ξ) - 1)

set_hyper_priors = function(coeffs, precision = 10) {
  control.family = list(hyper = list(spread = list(
    initial = coeffs[1],
    prior = "loggamma",
    param = precision * c(exp(coeffs[1])^2, exp(coeffs[1])))))
  for (i in seq_along(coeffs[-1])) {
    control.family$hyper[[paste0("beta", i)]] = list(
      prior = "gaussian",
      initial = coeffs[i + 1],
      param = c(coeffs[i + 1], precision))
  }
  control.family
}

set_fixed_priors = function(coeffs, precision = 10) {
  control.fixed = list(mean = as.list(coeffs),
                       prec = as.list(rep(precision, length(coeffs))))
  names(control.fixed$mean) = c("intercept", paste0("q_", seq_along(coeffs[-1])))
  names(control.fixed$prec) = names(control.fixed$mean)
  control.fixed
}

# =================================================================================

n = 500
q_coeffs = c(4, 2, -1, .5, -2) # Intercept and regression coefficients for location
s_coeffs = c(log(8), .2, -.1) # Intercept and regression coefficients for spread
ξ = .05 # Value of tail parameter
n_q = length(q_coeffs) - 1
n_s = length(s_coeffs) - 1

# Simulate coordinates
proj = "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=km +no_defs"
set.seed(1)
coords = data.frame(x = runif(n) + 58, y = runif(n) + 60) %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(4326)) %>%
  st_transform(proj)

# Simulate covariates
set.seed(1)
X = cbind(1, matrix(rnorm(n * (n_q + n_s)), nrow = n))
colnames(X) = c("intercept", paste0("q_", 1:n_q), paste0("s_", 1:n_s))

# Calculate location and spread params
q = as.numeric(X[, 1:(1 + n_q)] %*% q_coeffs)
s = as.numeric(X[, c(1, (n_q + 2):(n_q + n_s + 1))] %*% s_coeffs)
s = exp(s)

# Add spatial gaussian field to location param
distances = as.matrix(st_distance(coords))
units(distances) = NULL
Σ = exp(- distances / 10)
set.seed(1)
q = q + as.numeric(mvtnorm::rmvnorm(1, mean = rep(0, n), sigma = Σ))

# Simulate observations from the latent field
gev_par = bgev2gev(q, s, ξ)
set.seed(1)
value = rgev(n, gev_par$μ, gev_par$σ, gev_par$ξ)
mdata = inla.mdata(value, X[, (n_q + 2):(n_q + n_s + 1)], matrix(0, n, 0))

# Set the priors
control.fixed = set_fixed_priors(q_coeffs, precision = 10)
control.family = set_hyper_priors(s_coeffs, precision = 10)

# Define the mesh for using an SPDE in INLA
boundary = inla.nonconvex.hull(points = st_coordinates(coords), convex = -.15)
mesh = inla.mesh.2d(st_coordinates(coords),
                    boundary = boundary,
                    max.edge = c(10, 15),
                    cutoff = 3,
                    offset = c(10, 50),
                    crs = sp::CRS(proj))
plot(mesh)

# Define the prior for the SPDE
spde = inla.spde2.pcmatern(alpha = 1.5, mesh, prior.range = c(40, .9), prior.sigma = c(2, .05))
# I think this is a reasonable choice for the SPDE priors, given that
# The actual range is somewhere around 30 and the SD is 1

stack = inla.stack(
  data = list(value = value),
  A = list(inla.spde.make.A(mesh, st_coordinates(coords)), 1),
  effects = list(matern_field = seq_len(mesh$n), as.data.frame(X)),
  tag = "estimation")

inla.setOption(smtp = 'taucs')
tail.high <- 0.001
control.family$hyper$tail <- list(param = c(7, 0, tail.high))
res1 = inla(
  formula = mdata ~ -1 + intercept + q_1 + q_2 + q_3 + q_4 + f(matern_field, model = spde),
  family = "bgev",
  verbose = FALSE,
  control.compute = list(config = TRUE),
  control.predictor = list(A = inla.stack.A(stack)),
  data = inla.stack.data(stack),
  control.inla = list(h = 1e-3, cmin = 0, b.strategy = "keep",
                      int.strategy = "eb", strategy = "gaussian"), 
  control.fixed = control.fixed,
  control.family = control.family)


## keep the tail param found in the previous run as the initial value in the next, we have to do
## some woodo since the interval change
from.theta <- inla.models()$likelihood$bgev$hyper$theta2$from.theta
to.theta <- inla.models()$likelihood$bgev$hyper$theta2$to.theta

for(new.tail.high in c(0.01, 0.05, 0.1, 0.15)) {
    tail.intern <- res1$mode$theta[2]
    tail <- from.theta(tail.intern, interval = c(0, tail.high))
    tail.high <- new.tail.high
    tail.intern <- to.theta(tail, interval = c(0, tail.high))
    res1$mode$theta[2] <- tail.intern
    res1$.args$control.family[[1]]$hyper$theta2$param <- c(7, 0, tail.high)
    res1 <- inla.rerun(res1)
}
res1$.args$control.inla$int.strategy <- "auto"
res1$.args$control.inla$strategy <- "adaptive"
res1 <- inla.rerun(res1)

## maybe do this in the end
if (FALSE) {
    ## then it can still crash if we do 
    res1$.args$control.inla$cmin <- -Inf
    res1 <- inla.rerun(res1)
}
if (FALSE) {
    ## an alternative is to do this, but I'm not sure its better (feels a little weird)
    res1$.args$control.inla$cmin <- 0
    res1$.args$control.inla$b.strategy <- "skip"
    res1 <- inla.rerun(res1)
}
