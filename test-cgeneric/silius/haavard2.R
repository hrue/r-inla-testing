library(INLA)
inla.setOption(inla.mode = "experimental")

system("make -B")

alpha_generic_model = function(y_s0,
                               n, # Total number of observations
                               dist_to_s0, # List containing dist_to_s0 for each s0
                               init,
                               log_lambda_prior,
                               log_kappa_prior,
                               s0_index = rep(1L, length(y_s0)), # Which s0 do the different y(s0)'s come from?
                               debug = FALSE,
                               logging = FALSE) {
    if (!is.integer(s0_index)) {
        if (sum(abs(s0_index - as.integer(s0_index))) > 0) stop("s0_index contains non-integers")
        s0_index = as.integer(s0_index)
    }
    s0_index = as.integer(factor(s0_index, levels = sort(unique(s0_index))))
    if (!is.list(dist_to_s0)) dist_to_s0 = list(dist_to_s0)
    stopifnot(length(dist_to_s0) == max(s0_index))
    stopifnot(length(y_s0) == length(s0_index))

    args = list(
        debug = debug,
        logging = as.integer(logging),
        model = "inla_cgeneric_alpha_model",
        shlib = "alpha.so",
        n = n,
        s0_index = s0_index,
        y_s0 = y_s0,
        init = init,
        log_lambda_prior = log_lambda_prior,
        log_kappa_prior = log_kappa_prior)
    for (i in seq_along(dist_to_s0)) args[[paste0("dist_to_s0:", i)]] = dist_to_s0[[i]]

    do.call(INLA::inla.cgeneric.define, args)
}

# -----------------------------------------------------------------------
# Test out the model y = α * y(s0) + ε
# -----------------------------------------------------------------------

# Choose a design for the simulation study
loc = as.matrix(expand.grid(x = seq(2, 6, by = 2), y = seq(2, 6, by = 2)))
n_loc = nrow(loc)
n_train = 5
σ_ε = .1
λ = 4.3
κ = 1.8
threshold = 4
verbose = FALSE

# Place s0
s0 = c(4.5, 5.5)

# Compute distances
dist = as.matrix(dist(loc))
dist_to_s0 = as.matrix(dist(rbind(s0, loc)))[-1, 1]

# Sample y_s0
set.seed(1)
y_s0 = threshold + rexp(n_train)

# Create y
set.seed(1)
y = rep(y_s0, each = length(n_loc)) * rep(exp(-(dist_to_s0 / λ)^κ), length(y_s0))
y = y + rnorm(length(y), sd = σ_ε)

# Create the alpha_model using rgeneric and cgeneric
init = c(log(λ), log(κ))
log_lambda_prior = c(log(λ), 1)
log_kappa_prior = c(log(κ), 1)
alpha_model = alpha_generic_model(
  y_s0 = y_s0,
  dist_to_s0 = dist_to_s0,
  n = length(y),
  init = log(c(λ, κ)),
  logging = TRUE,
  log_lambda_prior = c(log(λ), 1),
  log_kappa_prior = c(log(κ), 1))

# Create everything else needed for running INLA
formula = y ~ -1 + f(idx, model = alpha_model)

INLA:::inla.cgeneric.q(alpha_model)
stop("XXX" )

# Run INLA with one thread (fast)
res = inla(formula,
           data = data.frame(y = y, idx = seq_along(y)),
           num.threads = "3:2",
           verbose = verbose)

# Run INLA with two threads (slow)
res2 = inla(formula,
           data = data.frame(y = y, idx = seq_along(y)),
           num.threads = "6:1",
           verbose = verbose,
           inla.call = "remote")

res$cpu; res2$cpu
