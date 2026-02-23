#
# fit_models.R - MetricGraph SPDE test with INLA/inlabru
#
# Models:
#   M1: Spatial SPDE only (alpha=1)
#   M2: Spatial + covariates
#   M3: Spatial + covariates + AR1 on hour
#

library(MetricGraph)
library(INLA)
library(inlabru)
library(ggplot2)

# Load data
bundle <- readRDS("bundle.rds")
graph <- bundle$graph
dat <- bundle$data

# Add observations to graph
graph$clear_observations()
graph$add_observations(data = dat, normalized = TRUE, clear_obs = TRUE)

# Plot: Graph with observations
p1 <- graph$plot(data = "log_NO2", vertex_size = 0, edge_width = 0.3, data_size = 0.8) +
  scale_color_viridis_c(option = "plasma", name = "log(NO2)") +
  ggtitle("Dublin road network with NO2 observations") +
  theme_minimal()
print(p1)

# Build SPDE model (alpha=1 = exact GMRF, Matern nu=0.5)
spde <- graph_spde(graph, alpha = 1)

# Prepare data for inlabru
covars <- c("log_NO2", "time", "t2m_C", "si10", "residential_pct_100m",
            "industrial_pct_100m", "FRC", "elevation_m")
data_spde <- graph_data_spde(spde, loc_name = "loc", covariates = covars)
df <- data_spde$data

# M1: Spatial SPDE only
cmp1 <- log_NO2 ~ -1 + Intercept(1) + field(loc, model = spde)

fit1 <- bru(cmp1, data = df, family = "gaussian",
            options = list(control.compute = list(dic = TRUE, waic = TRUE),
                           verbose = TRUE, keep = TRUE))

summary(fit1)
spde_result1 <- spde_metric_graph_result(fit1, "field", spde)
summary(spde_result1)

# M2: Spatial + covariates
cmp2 <- log_NO2 ~ -1 + Intercept(1) +
  t2m_C(t2m_C, model = "linear") +
  si10(si10, model = "linear") +
  residential(residential_pct_100m, model = "linear") +
  industrial(industrial_pct_100m, model = "linear") +
  FRC(FRC, model = "linear") +
  field(loc, model = spde)

fit2 <- bru(cmp2, data = df, family = "gaussian",
            options = list(control.compute = list(dic = TRUE, waic = TRUE),
                           verbose = TRUE, keep = TRUE))

summary(fit2)
spde_result2 <- spde_metric_graph_result(fit2, "field", spde)
summary(spde_result2)

# M3: Spatial + covariates + AR1 on hour
cmp3 <- log_NO2 ~ -1 + Intercept(1) +
  t2m_C(t2m_C, model = "linear") +
  si10(si10, model = "linear") +
  residential(residential_pct_100m, model = "linear") +
  industrial(industrial_pct_100m, model = "linear") +
  FRC(FRC, model = "linear") +
  hour_ar1(time, model = "ar1") +
  field(loc, model = spde)

fit3 <- bru(cmp3, data = df, family = "gaussian",
            options = list(control.compute = list(dic = TRUE, waic = TRUE),
                           verbose = TRUE,  keep = TRUE))

summary(fit3)
spde_result3 <- spde_metric_graph_result(fit3, "field", spde)
summary(spde_result3)

# Plot
fitted_vals <- fit3$summary.fitted.values$mean[1:length(df$log_NO2)]

# Interactive map 
library(mapview)
graph$plot(data = "log_NO2", type = "mapview", vertex_size = 0)
