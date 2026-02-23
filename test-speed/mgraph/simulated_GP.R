library(MetricGraph)
library(INLA)
library(qs)
# Load graph and build mesh
graph <- qread("graph_mesh_pruned_100m_7FRC.qs")
# graph$build_mesh(h = 0.1)
# graph$compute_mesh_weights()

# --- Simulate Gaussian process at random locations ---
set.seed(123)
n <- 2000
sigma <- 1.5
range <- 0.5
alpha <- 1

# Random locations on graph (weighted by edge length)
edge_lengths <- graph$get_edge_lengths()
edge_probs <- edge_lengths / sum(edge_lengths)
edge_number <- sample(1:graph$nE, n, replace = TRUE, prob = edge_probs)
distance_on_edge <- runif(n)
PtE <- cbind(edge_number, distance_on_edge)

# Sample GP
u <- sample_spde(
  range = range,
  sigma = sigma,
  alpha = alpha,
  graph = graph,
  PtE = PtE
)

# Plot GP realization
graph$plot(X = u, X_loc = PtE, vertex_size = 0, edge_width = 0.3)

# --- Fit basic GP model with INLA ---                                                  
noise_sd <- 0.1                                                                         
y <- u + rnorm(n, 0, noise_sd)                                                          

# Add to graph                                                                          
graph$add_observations(data = data.frame(                                               
  y = y,                                                                                
  edge_number = edge_number,                                                            
  distance_on_edge = distance_on_edge                                                   
), normalized = TRUE, clear_obs = TRUE)                                                 

# Build SPDE model (no LGCP)                                                            
spde_model <- graph_spde(graph, alpha = 1)                                              

# Prepare data                                                                          
data_spde <- graph_data_spde(spde_model, name = "field")                                

# Build stack - Intercept added in effects, not data                                    
stk <- inla.stack(                                                                      
  data = data_spde[["data"]],                                                           
  A = data_spde[["basis"]],                                                             
  effects = c(                                                                          
    data_spde[["index"]],                                                               
    list(Intercept = 1)                                                                 
  )                                                                                     
)                                                                                       

# Fit                                                                                   
fit <- inla(                                                                            
    y ~ -1 + Intercept + f(field, model = spde_model),                                    
    family = "gaussian",                                                                  
    data = inla.stack.data(stk),                                                          
    control.predictor = list(A = inla.stack.A(stk)),
    keep = TRUE
)                                                                                       

summary(fit)                                                                            
spde_result <- spde_metric_graph_result(fit, "field", spde_model)                       
summary(spde_result)       

sessionInfo()
