# Zachary Ferris - zferris2022@my.fit.edu - April 22, 2025

# load libraries
library(INLA)
library(lattice)
library(Matrix)
library(foreach)
library(magrittr)
library(parallel)
library(sp)
library(INLA)
library(ggplot2)
library(ggmap)
library(gstat)
library(ncdf4)
library(raster)
library(sf)
library(spdep)
library(tidyverse)
library(rworldmap)
library(rworldxtra)
library(grDevices)
library(colorRamps)
library(RColorBrewer)
library(plyr)
library(fields)
library(corrplot)
library(dplyr)
library(terra)
library(patchwork)

# set paths
data_dir = './'
results_dir = 'Results/'


# read in the global coral bleaching database
bleaching_data = read.csv(file.path(data_dir, 'GCBD_BCO-DMO_101422.csv')) # thank you, Chelsey!


# read in predictor data
topography_data = read.csv(file.path(data_dir, 'GCBD_BCO-DMO_101422_topography_011725.csv'))
cloud_data = read.csv(file.path(data_dir, 'GCBD_clouds_020925.csv'))
hydrodynamic_data = read.csv(file.path(data_dir, 'GCBD_Hydrodynamics_30_021425.csv'))
par_data = read.csv(file.path(data_dir, 'GCBD_PAR_30_042125.csv'))
turbidity_data = read.csv(file.path(data_dir, 'GCBD_daily_Turbidity_020325.csv'))
heatwaveR_data = read.csv(file.path(data_dir, "GCBD_heatwaveR_14d_040725.csv")) # thank you, Andy!
tide_data = read.csv(file.path(data_dir, "GCBD_TidalRange_020725.csv"))
SWH_data = read.csv(file.path(data_dir, 'GCBD_SWH_021925.csv'))
SWH_data = SWH_data %>%
  dplyr::rename(
    Latitude_Degrees = Lat,
    Longitude_Degrees = Lon
  )
WP_data = read.csv(file.path(data_dir, 'GCBD_WP_021925.csv'))
WP_data = WP_data %>%
  dplyr::rename(
    Latitude_Degrees = Lat,
    Longitude_Degrees = Lon
  )
sst_data = read.csv(file.path(data_dir, 'GCBD_SST_30_042125.csv'))

# add topography data
data = merge(
  bleaching_data, 
  topography_data[, c('Latitude_Degrees', 'Longitude_Degrees', 'Depth', 'Rugosity', 'Aspect_Degrees', 'Slope_Degrees', 'Equatorial_Alignment', 'Bathymetric_Position_Index', 'AdjSD')], 
  by = c('Latitude_Degrees', 'Longitude_Degrees'), 
  all.x = TRUE
)

# add cloud data
data = merge(
  data,
  cloud_data[, c('Latitude_Degrees', 'Longitude_Degrees', 'Date_Month', 'Cloud_Cover')],
  by = c('Latitude_Degrees', 'Longitude_Degrees', 'Date_Month'),
  all.x = TRUE
)

# add water velocity and direction data
data = merge(
  data,
  hydrodynamic_data[, c('Latitude_Degrees', 'Longitude_Degrees', 'Date_Year', 'Date_Month', 'Date_Day', 'Water_Velocity', 'Water_Direction')],
  by = c('Latitude_Degrees', 'Longitude_Degrees', 'Date_Year', 'Date_Month', 'Date_Day'),
  all.x = TRUE
)


# add par data
data = merge(
  data,
  par_data[, c('Latitude_Degrees', 'Longitude_Degrees', 'Date', 'PAR_mean', 'PAR_Rate_of_Change')],
  by = c('Latitude_Degrees', 'Longitude_Degrees', 'Date'),
  all.x = TRUE
)


# add turbidity data
data =  merge(
  data,
  turbidity_data[, c('Latitude_Degrees', 'Longitude_Degrees', 'Date', 'Turbidity_mean')],
  by = c('Latitude_Degrees', 'Longitude_Degrees', 'Date'),
  all.x = TRUE
)


# add heatwaveR data
data = merge(
  data,
  heatwaveR_data[, c('Latitude_Degrees', 'Longitude_Degrees', 'Date', 'Heatwave_Intensity_Mean', 'Heatwave_Intensity_Max', 'Heatwave_Intensity_Var', 'Heatwave_Intensity_Cumulative', 'Heatwave_Onset_Rate', 'Heatwave_Duration', 'Heatwave_Decline_Rate', 'Has_Event', 'Prior_Heatwave_Num', 'Prior_Heatwave_Exposure_Days', 'Prior_Heatwave_Exposure_Intensity', 'Days_Since_Last_Heatwave')],
  by = c('Latitude_Degrees', 'Longitude_Degrees', 'Date'),
  all.x = TRUE
)


# add tide data
data = merge(
  data,
  tide_data[, c('Latitude_Degrees', 'Longitude_Degrees', 'Tidal_Range')],
  by = c('Latitude_Degrees', 'Longitude_Degrees'),
  all.x = TRUE
)


# add significant wave height data
data = merge(
  data,
  SWH_data[, c('Latitude_Degrees', 'Longitude_Degrees', 'Date', 'SWH_mean', 'SWH_sd')],
  by = c('Latitude_Degrees', 'Longitude_Degrees', 'Date'),
  all.x = TRUE
)


# add wave period data
data = merge(
  data,
  WP_data[, c('Latitude_Degrees', 'Longitude_Degrees', 'Date', 'WP_mean', 'WP_sd')],
  by = c('Latitude_Degrees', 'Longitude_Degrees', 'Date'),
  all.x = TRUE
)

# add sst data
data = merge(
  data,
  sst_data[, c('Latitude_Degrees', 'Longitude_Degrees', 'Date', 'SST_mean', 'SST_sd')],
  by = c('Latitude_Degrees', 'Longitude_Degrees', 'Date'),
  all.x = TRUE
)

# check bleaching level
unique(levels(as.factor(data$Bleaching_Level))) # CK - this field is specific to ReefCheck; "nd" is, by default, population for all other data sources, and "Colony" appears to have already been filtered out in the BCO-DMO version


# remove no-data values in Percent Bleaching
class(data$Percent_Bleaching) # char
any(is.na(data$Percent_Bleaching)) # false
sum(data$Percent_Bleaching == "nd") # missing bleaching values for 6,846 records
data = data[data$Percent_Bleaching != "nd", ] # 41,361 rows to 34,515 rows


# get data after the start of modis
class(data$Date) # char
any(is.na(data$Date)) # false
any(data$Date == "nd") # false, double check...
any(nchar(data$Date) != 10) # false, all rows have valid values for yyyy-mm-dd
data$Date = as.Date(data$Date, format = "%Y-%m-%d")
class(data$Date) # date
any(is.na(data$Date)) # triple check, false

MODIS_start = as.Date("2002-07-04", format = "%Y-%m-%d")
data = data[data$Date > MODIS_start, ]
min(data$Date) # 2002-07-07


# also get data shallower than 20 m, the maximum derivable depth by Li et al (2021)
class(data$Depth_m) # char
any(data$Depth_m == "nd") # true
nrow(data[data$Depth_m == "nd", ]) # missing measured depth for 358 records (after 2002-07-04), these will be treated, by default, as zeros (normalized means) in INLA
data$Depth_m = as.numeric(data$Depth_m)
nrow(data[!is.na(data$Depth_m) & data$Depth_m > 20, ]) # 207 records deeper than 20 meters (and after 2002-07-04), remove these (although Li et al 2021 gives excellent relative depth at local scales, we cannot replace these with derived depth because global [chl-a] is assumed to be fixed and therefore the validity of absolute derived depth is unknown outside of their "validation" regions)
data = data[data$Depth_m <= 20 | is.na(data$Depth_m), ]

# function to calculate wave energy density (E)
wave_energy_density = function(Hs) {
  if (!is.numeric(Hs) || is.na(Hs) || Hs < 0) return(NA)
  E = (1 / 8) * 1025 * 9.81 * Hs^2
  return(E)
}

# Append E to the original data
data = data %>%
  mutate(
    Wave_Energy = mapply(wave_energy_density, SWH_mean)
  )

# Display updated data
head(data)

# Plot
plot(data$Depth_m, data$Wave_Energy, pch = 16, 
     col = as.numeric(cut(data$SWH_mean, breaks = 5)),
     xlab = "Depth (m)", ylab = "Wave Energy (J/m-2)", 
     main = "Wave Energy (WAVERYS H_s, WP, Local Depth)")
legend("topright", legend = levels(cut(data$SWH_mean, breaks = 5)), 
       col = 1:5, pch = 16, title = "H_s (m)")

# Process no-data values in hydro data
data$Water_Velocity[data$Water_Velocity == -9999] = NA
data$Water_Direction[data$Water_Direction == -9999] = NA

# clean rugosity
range(data$Rugosity, na.rm = TRUE)
data$Rugosity[data$Rugosity < 1] = NA
range(data$Rugosity, na.rm = TRUE)

# check data
str(data)

# prepare response variable
data$Percent_Bleaching = as.numeric(data$Percent_Bleaching)
data$Percent_Bleaching = data$Percent_Bleaching / 100

hist(data$Percent_Bleaching)

data$Ocean_Name[which(data$Ocean_Name == "Red Sea")] = "Indian"
data$Ocean_Name[which(data$Ocean_Name == "Arabian Gulf")] = "Indian"

# get polar alignment
par(mfrow = c(1,1))
data$Polar_Alignment = -(data$Equatorial_Alignment)
plot(data$Equatorial_Alignment ~ data$Polar_Alignment)

# check agreement between measured and derived depth
plot(data$Depth_m ~ data$Depth, xlab = "Derived Depth (m)", ylab = "Measured Depth")


png(file.path(results_dir, 'DepthVal_042225.png'), width = 6, height = 6, units = "in", res = 300)
par(mfrow = c(1,1), cex.lab = 1.5, cex.axis = 1.25, mar = c(5, 6, 3, 2))
plot(data$Depth_m ~ data$Depth, xlab = "Derived Depth (m)", ylab = "Measured Depth (m)", cex = 0.5); abline(0, 1, col = "blue")
dev.off()

# remove sparse sampling beyond 35 degrees
data = data[abs(data$Latitude_Degrees) <= 35, ] # remove sparse sampling above 35 deg
range(data$Latitude_Degrees)

# export raw data
write.csv(data, file.path(data_dir, "Ferris_et_al_2025_data_raw_042225.csv"), row.names = FALSE)

# get number of surveys per site
survey_counts = aggregate(
  x = rep(1, nrow(data)),  # A vector of 1s to count each row
  by = list(data$Latitude_Degrees, data$Longitude_Degrees),  # Group by these columns
  FUN = sum  # Sum the 1s to get the count
)

colnames(survey_counts) = c("Latitude_Degrees", "Longitude_Degrees", "Count")

if (sum(survey_counts$Count) != nrow(data)) {
  stop('ahhhh!')
} else {
  print('math checks out')
}

# data for Figure 1 - produced in ArcGIS
write.csv(survey_counts, file.path(data_dir, "Ferris_et_al_2025_survey_counts_042225.csv"), row.names = FALSE) # update Fig 1

# Figure 2
plot.var = function(data, var, main = "", ylab = "Latitude (°)", xlim = c(-35, 37.5), xbreaks = seq(-35, 35, 10), xlab, ylim, ybreaks, color, panel) {
  
  bin_breaks = seq(-35, 35, 10)
  
  p = ggplot(data = data, aes(y = .data[[var]], x = Latitude_Degrees))+
    geom_point(color = 'grey90', size = 0.2, na.rm = TRUE)+
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    geom_boxplot(aes(group = cut(Latitude_Degrees, breaks = bin_breaks)), na.rm = TRUE, fill = "grey85", outlier.size = 0.25) +
    geom_smooth(na.rm = TRUE, color = color, se = FALSE, method = "gam", formula = y ~ s(x, bs = "cs", k = 5), linewidth = 2) +
    ylab(xlab)+
    xlab(ylab)+
    coord_flip() +
    annotate("text", y = mean(ylim), x = 36.9, label = panel, hjust = 0.5, vjust = -0.005, size = 5, fontface = "bold") +
    scale_x_continuous(limits = xlim, breaks = xbreaks) +
    scale_y_continuous(limits =  ylim, breaks = ybreaks) +
    theme_classic() +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 13),
      panel.border = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank()
    )
  
  if (panel != "a") {
    p = p + theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank()
    )
  }
  return(p)
}

sst_plot = plot.var(data = data, var = "SST_mean", xlab = "Temperature (°C)", ylim = c(10, 35), ybreaks = seq(10, 35, 5), color = "#377eb8", panel = "a")

par_plot = plot.var(data = data, var = "PAR_mean", xlab = expression("Irradiance (Einsteins m"^-2*" d"^-1*")"), ylim = c(15, 65), ybreaks = seq(20, 60, 10), color = "#4daf4a", panel = "b")

mhw_plot = plot.var(data = data[data$Heatwave_Intensity_Mean > 0 & !is.na(data$Heatwave_Intensity_Mean), ], var = "Heatwave_Intensity_Mean", xlab = "Heatwave intensity (°C)", ylim = c(0.5, 2), ybreaks = seq(0.5, 2, 0.5), color = "#e41a1c", panel = "c")

cloud_plot = plot.var(data = data, var = "Cloud_Cover", xlab = "Cloud Frequency (%)", ylim = c(0, 100), ybreaks = seq(0, 100, 25), color = "#984ea3", panel = "d")

fig2 = sst_plot | par_plot | mhw_plot | cloud_plot

fig2

ggsave(file.path(results_dir, "Figure2_042325b.png"), width = 10, height = 3.5, units = "in") # neatline added externally in Canva


# check for collinearity

# define continuous predictors
colnames(data)
data$Absolute_Latitude = abs(data$Latitude_Degrees)
continuous_vars = c('Depth', 'Depth_m' , 'Slope_Degrees', 'Polar_Alignment', 'Bathymetric_Position_Index', 'AdjSD', 'Rugosity',
                    'Water_Velocity', 'Absolute_Latitude',
                    'PAR_mean', 'Cloud_Cover',
                    'Turbidity_mean',
                    'Heatwave_Intensity_Mean', 'Heatwave_Intensity_Max', 'Heatwave_Intensity_Var', 'Heatwave_Intensity_Cumulative', 'Heatwave_Onset_Rate', 'Heatwave_Duration', 'Heatwave_Decline_Rate', 'Prior_Heatwave_Num',
                    'Tidal_Range', 'Wave_Energy',
                    'SST_mean')

# convert variables to numeric
for (var in continuous_vars) {
  data[[var]] = as.numeric(data[[var]])
}

# corrplot
df = select_if(data, is.numeric)
names(df)#
df = subset(df, select = continuous_vars)
M = cor(df, use = "pairwise.complete.obs")

par(mfrow = c(1,1))
png(file.path(results_dir, 'corrplot_042225.png'), width = 13, height = 13, units = "in", res = 600)
corrplot(M, method = "number")
dev.off()

# standardizing function
standardize_function = function(x){
  x.standardized = (x - mean(na.omit(x))) / sd(na.omit(x))
  return(x.standardized)
}

# get fields
names(data)

# initialize a new dataframe
data1 = data.frame(Site = data$Site_ID,
                   Country = data$Country_Name,
                   Ecoregion = data$Ecoregion_Name,
                   Ocean = data$Ocean_Name,
                   Latitude_Degrees = data$Latitude_Degrees,
                   Longitude_Degrees = data$Longitude_Degrees,
                   Data_Source = data$Data_Source,
                   Year = data$Date_Year,
                   Month = data$Date_Month,
                   Percent_Bleaching = data$Percent_Bleaching)

# iterate over variables, standardize, and add to above dataframe
for (var in continuous_vars) {
  print(var)
  data1[[var]] = standardize_function(data[[var]])
  par(mfrow = c(1, 2))
  hist(data[[var]], main = var)
  hist(data1[[var]], main = paste('Standardized', var))
  par(mfrow = c(1,1))
}

# set interaction var
data1$SST_PAR = data1$SST_mean * data1$PAR_mean

# check distribution of response
par(mfrow = c(1,1))
hist(data1$Percent_Bleaching)

# whoa! check number of zeros
num_zeros = sum(data1$Percent_Bleaching == 0, na.rm = TRUE)
print(paste('Number of zeros:', num_zeros)) # 15,502
print(paste('Percent of data that is zeros: ', round((num_zeros / nrow(data1))*100, 0))) # 52%!!!

# check number of ones
num_ones = sum(data1$Percent_Bleaching == 1, na.rm = TRUE)
print(paste('Number of ones:', num_ones)) # 93
print(paste('Percent of data that is ones: ', round((num_ones / nrow(data1))*100, 2))) # 0.3%

# duplicate df
Global = data1

# bind sites
Global_Loc = cbind(Global$Longitude_Degrees, Global$Latitude_Degrees)

# get unique sites for mesh
unique_Loc = unique(Global_Loc)
unique_coords_df = as.data.frame(unique_Loc)
write.csv(unique_coords_df, file.path(results_dir, "unique_coords_042225.csv"), row.names = FALSE)

# convert coords to Cartesian 
Global_Loc_cartesian = inla.mesh.map(Global_Loc, projection = "longlat")
unique_Loc_cartesian = inla.mesh.map(unique_Loc, projection = "longlat")

# set spherical mesh
radian_scalar = (pi/180)

max_edge_rad = 3 * radian_scalar # 333 km in radians
cutoff_rad = (5/111) * radian_scalar # 5 km in radians

MeshPred = inla.mesh.2d(unique_Loc_cartesian,
                        max.edge = c(1, 5) * max_edge_rad,
                        cutoff = cutoff_rad, # don't triangulate within 5 km
                        min.angle = 30,
                        crs = inla.CRS("sphere")
)



plot(MeshPred)
points(unique_Loc, col = 'blue', pch = 16, cex = 0.5)

print(MeshPred$n)

# set projector
A2_global = inla.spde.make.A(MeshPred, loc = Global_Loc_cartesian)
dim(A2_global)

# define the Matern correlation on the mesh
spde.globe = inla.spde2.matern(MeshPred, alpha=2)

# define spatial random field
w.index.globe = inla.spde.make.index(
  name    = 'w', 
  n.spde  = spde.globe$n.spde,
  n.group = 1,
  n.repl  = 1)

# set X dataframe for model
X_Global = data.frame(Intercept = rep(1, nrow(Global)), 
                      # random effects
                      Country = factor(Global$Country),
                      Ocean = factor(Global$Ocean),
                      Ecoregion = factor(Global$Ecoregion),
                      Site = factor(Global$Site),
                      Year = as.numeric(Global$Year),
                      Month = as.numeric(Global$Month),
                      Data_Source = factor(Global$Data_Source),
                      
                      # fixed effects
                      Temperature = Global$SST_mean,
                      Latitude = Global$Absolute_Latitude,
                      Cumulative_Heatwave_Intensity = Global$Heatwave_Intensity_Cumulative,
                      Temperature_Irradiance = Global$SST_PAR,
                      Depth = Global$Depth_m,
                      Rugosity = Global$AdjSD,
                      Reef_Slope = Global$Slope_Degrees,
                      Polar_Alignment = Global$Polar_Alignment,
                      Bathymetric_Position = Global$Bathymetric_Position_Index,
                      Current_Velocity = Global$Water_Velocity,
                      Wave_Energy = Global$Wave_Energy,
                      Turbidity = Global$Turbidity_mean,
                      Cloud_Frequency = Global$Cloud_Cover,
                      Number_of_Prior_Heatwaves = Global$Prior_Heatwave_Num,
                      Tidal_Range = Global$Tidal_Range,
                      
                      # carry over coords
                      Latitude_Degrees = Global$Latitude_Degrees,
                      Longitude_Degrees = Global$Longitude_Degrees

)

# get sampling effort
cat('Number of surveys: ', nrow(X_Global), '\nNumber of sites: ', nrow(unique(X_Global[, c("Latitude_Degrees", "Longitude_Degrees")])), '\nNumber of countries: ', length(unique(X_Global$Country)), '\nSurvey date range:', min(X_Global$Year), "to", max(X_Global$Year), '\n')


# Plot bleaching by raw month, split by hemisphere
Global$Hemisphere = ifelse(Global$Latitude_Degrees >= 0, "Northern", "Southern")

p1 = ggplot(Global, aes(x = Month, y = Percent_Bleaching)) +
  geom_boxplot(aes(group = Month), outlier.shape = NA) +  # Boxplot per month, no outliers for clarity
  facet_wrap(~ Hemisphere, labeller = as_labeller(c("Southern" = "Southern Hemisphere", "Northern" = "Northern Hemisphere"))) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +  # Month names
  labs(title = "Bleaching by Survey Month (m)",
       x = "Month", y = "Proportion Bleaching") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Global$seasonal_month = ifelse(Global$Latitude_Degrees >= 0, Global$Month, (Global$Month + 6) %% 12)
Global$seasonal_month[Global$seasonal_month == 0] = 12

p2 = ggplot(Global, aes(x = seasonal_month, y = Percent_Bleaching)) +
  geom_boxplot(aes(group = seasonal_month), outlier.shape = NA) +
  facet_wrap(~ Hemisphere, labeller = as_labeller(c("Southern" = "Southern Hemisphere", "Northern" = "Northern Hemisphere"))) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "Bleaching by Northern-Hemisphere-Centered Month (m*)",
       x = "Seasonal Month", y = "Proportion Bleaching") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p1/p2

# add seasonal month to x_global
X_Global$seasonal_month = ifelse(X_Global$Latitude_Degrees >= 0, X_Global$Month, (X_Global$Month + 6) %% 12)
X_Global$seasonal_month[X_Global$seasonal_month == 0] = 12


# check NA frequency
for (col in names(X_Global)[9:23]) {
  num_na = sum(is.na(X_Global[[col]]))
  percent_na = (num_na / nrow(X_Global)) * 100
  cat(col, ":", num_na, "NAs (", round(percent_na, 2), "%)\n")
}

# create stack
Stack = inla.stack(
  tag = "oBetaFit",
  data = list(y = data1$Percent_Bleaching),
  A = list(A2_global, 1),
  effects = list(
    w = w.index.globe,
    X = as.data.frame(X_Global)
  )
)

inla.setOption(verbose = TRUE, keep = TRUE, safe = FALSE, inla.call = "")


# INLA model function
fit.model = function(Variables) {
  prec.prior = list(prec = list(param = c(0.001, 0.001)))
  f0 = as.formula(paste0("y ~ -1 + Intercept +", paste0(Variables, collapse=" + ")))
  I_global = inla(f0,
                  family = "obeta", # thank you, Havard!
                  data = inla.stack.data(Stack),
                  control.inla = list(cmin = 0),
                  control.compute = list(dic=TRUE, waic=TRUE, cpo=TRUE),
                  control.predictor = list(A = inla.stack.A(Stack), compute=TRUE))
  print(paste('wAIC:', summary(I_global)$waic$waic))
  return(I_global)
}


# fit the model
model_path = file.path(results_dir, "Model", "model_obeta.rds")
if (file.exists(model_path)) {
  print('Loading saved model...')
  model_obeta = readRDS(file = model_path)
} else {
  print('Fitting model...')
  dir.create(file.path(results_dir, "Model"), recursive = TRUE)
  
  runtime = system.time({
    model_obeta = fit.model(c(
      "f(Ocean, model='iid')",# random intercept for ocean
      "f(Site, model='iid')", # random intercept for site
      "f(w, model=spde.globe)", # spatial autocorrelation
      "f(Data_Source, model='iid')", # random intercept for data source
      "f(Year, model='rw1', hyper=prec.prior, scale.model = TRUE)", # annual autocorrelation
      "f(seasonal_month, model='rw1', cyclic = TRUE, scale.model = TRUE)", # seasonal autocorrelation
      "Temperature",
      "Temperature_Irradiance",
      "Cumulative_Heatwave_Intensity",
      "Number_of_Prior_Heatwaves",
      "Cloud_Frequency",
      "Turbidity",
      "Current_Velocity",
      "Tidal_Range",
      "Wave_Energy",
      "Depth",
      "Rugosity",
      "Reef_Slope",
      "Polar_Alignment",
      "Bathymetric_Position",
      "Latitude"
    ))
  })
  
  print(runtime)
  
  saveRDS(model_obeta, file = model_path)
  
}

# export the GMRF for visualization in ArcGIS Pro
earth_radius_km = 6371

vertices = MeshPred$loc
triangles = MeshPred$graph$tv

vertices_scaled = vertices * earth_radius_km

cartesian_to_spherical = function(xyz, radius = earth_radius_km) {
  x = xyz[, 1]
  y = xyz[, 2]
  z = xyz[, 3]
  lon = atan2(y, x) * (180 / pi)
  r_xy = sqrt(x^2 + y^2)
  lat = atan2(z, r_xy) * (180 / pi)
  r = sqrt(x^2 + y^2 + z^2)
  elev = r - radius
  return(cbind(lon, lat, elev))
}

vertices_spherical = cartesian_to_spherical(vertices_scaled, earth_radius_km)
vertices_df = as.data.frame(vertices_spherical)
names(vertices_df) = c("lon", "lat", "elev")

create_triangle = function(triangle_idx, vertices) {
  coords_ = vertices[triangle_idx, ]
  coords_ = rbind(coords_, coords_[1, ])
  return(st_polygon(list(as.matrix(coords_)), dim = "XYZ"))
}
triangle_geom = apply(triangles, 1, create_triangle, vertices = vertices_df)
mesh_sf = st_sf(geometry = st_sfc(triangle_geom, crs = 4326))

st_write(mesh_sf, file.path(results_dir, "Mesh_3D_spherical2.shp"), delete_layer = TRUE, layer_options = "SHPT=POLYGONZ")



# function to plot the fixed effects
plot.fixed = function(dataframe) {
  
  # Helper function to compute credibility intervals
  get_credibility_intervals = function(marginal, levels = c(0.95, 0.90, 0.80)) {
    ci = sapply(levels, function(level) {
      cred_in = inla.hpdmarginal(level, marginal)
      return(abs(cred_in[1] - inla.emarginal(function(x) x, marginal)))
    })
    return(ci)
  }
  
  # Compute posterior means and credibility intervals for each fixed effect
  df = data.frame(
    Var = character(0),
    mu = numeric(0),
    ci95 = numeric(0),
    ci90 = numeric(0),
    ci80 = numeric(0),
    sig = integer(0)
  )
  
  for (var in names(dataframe$marginals.fixed)) {
    if (var != "Intercept") {
      marginal = dataframe$marginals.fixed[[var]]
      mu = inla.emarginal(function(x) x, marginal)  # Posterior mean
      ci = get_credibility_intervals(marginal)  # Credibility intervals
      
      
      # Determine significance based on CI
      sig = sum(
        sign(mu - ci[1]) + sign(mu + ci[1]),  # 95% CI
        sign(mu - ci[2]) + sign(mu + ci[2]),  # 90% CI
        sign(mu - ci[3]) + sign(mu + ci[3])   # 80% CI
      )
      
      df = rbind(df, data.frame(Var = var, mu = mu, ci95 = ci[1], ci90 = ci[2], ci80 = ci[3], sig = sig))
    }
  }
  
  # Set colors for significance levels
  cols = c("-6" = "#0000FF", "-4" = "#0000FF", "-2" = "#6666FF", # blues
           "0" = "#C0C0C0", # grey
           "2" = "#FF6666", "4" = "#FF0000", "6" = "#FF0000") # reds
  
  # Plot with points and colored significance
  ggplot(df, aes(x = reorder(Var, mu), y = mu, color = factor(sig))) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5, linetype = "dashed") + 
    geom_errorbar(aes(ymin = mu - ci95, ymax = mu + ci95), width = 0, linewidth = 1.5) +
    geom_errorbar(aes(ymin = mu - ci90, ymax = mu + ci90), width = 0, linewidth = 3) +
    geom_errorbar(aes(ymin = mu - ci80, ymax = mu + ci80), width = 0, linewidth = 4.5) +
    geom_point(size = 6) +
    scale_colour_manual(values = cols, breaks = c("-6", "-4", "-2", "0", "2", "4", "6")) +
    coord_flip() +
    xlab("") +
    ylab(expression(italic(β[j]))) +
    theme_classic() +
    #scale_y_continuous(limits = c(-0.4, 0.2), breaks = seq(-0.6, 0.3, by = 0.1), labels = scales::number) +
    #ggbreak::scale_y_break(breaks = c(-0.4, -0.2), scales = "fixed") +
    theme(legend.position = "none",
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          axis.title.x = element_text(size = 20),
          axis.text = element_text(size = 18, color = "black"),
          axis.ticks = element_line(linewidth = 1),
          axis.text.x.top = element_blank(),
          axis.ticks.x.top = element_blank(),
          panel.grid.major.y = element_line(linewidth = 0.25, color = '#E5E4E2'),
          plot.margin = margin(20, 20, 20, 20))
}
plot.fixed(model_obeta)

png(file.path(results_dir, 'Bleaching_INLA_Fixed_042225ir.png'), width = 12, height = 12, units = "in", res = 300)
plot.new()
p = plot.fixed(model_obeta)
print(p)
dev.off()


# get the nominal range
nom_range_result = inla.spde2.result(model_obeta, "w", spde.globe)
nom_range_rad = nom_range_result[["marginals.range.nominal"]][[1]]
nom_range_km = nom_range_rad
nom_range_km[, 1] = nom_range_rad[, 1] * earth_radius_km
nom_range_km_mean = mean(nom_range_km[, 1])
print(paste('Average nominal range:', round(nom_range_km_mean, 0), "km"))

png(file.path(results_dir, 'Nominal_range_042225.png'), width = 10, height = 10, units = "in", res = 300)
par(mfrow = c(1,1), cex.lab = 1.5, cex.axis = 1.25, mar = c(5, 6, 4, 2) + 0.1)
plot(nom_range_km, type = "l", xlab = "Nominal range (km)", ylab = "Posterior density")
density_at_mean = approx(nom_range_km[, 1], nom_range_km[, 2], xout = nom_range_km_mean)$y
segments(x0 = nom_range_km_mean, y0 = 0, x1 = nom_range_km_mean, y1 = density_at_mean)
dev.off()


# check clouds ~ latitude ~ bleaching
month_names = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
lat_ranges = list("1" = c(-16, 5), "2" = c(-16, 4), "3" = c(-13, 5), "4" = c(-9, 7), "5" = c(-4, 12), "6" = c(2, 13), "7" = c(3, 15), "8" = c(4, 18), "9" = c(3, 16), "10" = c(-4, 13), "11" = c(-9, 12), "12" = c(-13, 9)) # Waliser and Gautier (1993) J. Climate - Figure 4H latitudinal range of 4 HRC days/month
png(file.path(results_dir, 'Cloud_Lat_Monthly_040625.png'), width = 9, height = 12, units = "in", res = 300)
par(mfrow = c(4, 3), cex.lab = 1.75, cex.axis = 1.5, cex.main = 2, mar = c(6, 6, 4, 2) * 0.8)  # 4 rows, 3 columns, adjust margins
color_ramp = colorRampPalette(c("blue", "red"))
months = sort(unique(data$Date_Month))
bin_factor = 5
for (month in months) {
  # Subset data for the current month
  month_data = data[data$Date_Month == month, ]
  
  # Get the full month name
  month_name = month_names[month]
  
  # Scale Cloud_Cover to (0, 1) for Beta distribution (0-100, no exact 0 or 100)
  month_data$Cloud_Cover_scaled = month_data$Cloud_Cover / 100
  
  # Manually bin Latitude_Degrees to the nearest degree
  month_data$Latitude_Binned = round(month_data$Latitude_Degrees / bin_factor) * bin_factor # bin by 2 deg
  bleaching_means = tapply(month_data$Percent_Bleaching, month_data$Latitude_Binned, mean, na.rm = TRUE)
  lat_bins = as.numeric(names(bleaching_means))
  
  # color point by bleaching
  bleaching = month_data$Percent_Bleaching
  point_colors = color_ramp(100)[as.numeric(cut(bleaching, breaks = 100, include.lowest = TRUE))]
  point_colors = adjustcolor(point_colors, alpha.f = 0.2)
  
  # conditional axes labels
  xlab_text = ifelse(month > 9, "Latitude (Degrees)", "")  # Only show x-label for months 10-12 (row 4)
  ylab_text = ifelse(month %in% c(1, 4, 7, 10), "Cloud Frequency", "")  # Only show y-label for column 1
  
  # Create scatter plot of Cloud_Cover vs Latitude_Degrees (original scale for plotting)
  plot(month_data$Latitude_Degrees,
       month_data$Cloud_Cover,
       xlab = xlab_text,
       ylab = ylab_text,
       main = month_name,
       font.main = 1, # Use full month name as title
       pch = 1,
       cex = 0.1,
       col = "grey80",
       xlim = c(-35, 35),
       ylim = c(0, 100),
       xaxt = "n",
       yaxt = "n")  # Gray points for contrast
  axis(1, at = c(-30, -15, 0, 15, 30))
  axis(2, at = c(0, 25, 50, 75, 100))
  usr = par("usr")
  x_pos = usr[1]+0.02 * (usr[2] - usr[1])
  y_pos = usr[3]+0.93 * (usr[4] - usr[3])
  
  # add  number of samples to upper left corner
  text(x = x_pos, y = y_pos, labels = paste0("n = ", nrow(month_data)), cex = 1.25, pos = 4, col = "black")
  
  # fit a simple cubic spline to the data
  spline_fit = smooth.spline(month_data$Latitude_Degrees, month_data$Cloud_Cover, df = 5)
  
  # predict the spline over lat_seq
  lat_seq = seq(-35, 35, length.out = 100)
  spline_pred = predict(spline_fit, lat_seq)$y
  spline_pred = pmax(0, pmin(100, spline_pred))
  
  # Draw a horizontal line with upward end caps for the latitude range
  month_key = as.character(month)
  lat_range = lat_ranges[[month_key]]
  
  # Horizontal line slightly above x-axis (at y = 5)
  lines(x = lat_range, y = c(5, 5), col = "black", lwd = 1.5)
  # Upward end caps (from y = 5 to y = 10)
  segments(x0 = lat_range[1], y0 = 5, x1 = lat_range[1], y1 = 10, col = "black", lwd = 1.5)
  segments(x0 = lat_range[2], y0 = 5, x1 = lat_range[2], y1 = 10, col = "black", lwd = 1.5)
  
  # vertical dashed lines up to the spline 
  spline_at_endcaps = predict(spline_fit, lat_range)$y  # Get spline values at lat_range endpoints
  segments(x0 = lat_range[1], y0 = 8, x1 = lat_range[1], y1 = spline_at_endcaps[1], 
           col = "black", lty = 2, lwd = 0.75)  # Left dashed line
  segments(x0 = lat_range[2], y0 = 8, x1 = lat_range[2], y1 = spline_at_endcaps[2], 
           col = "black", lty = 2, lwd = 0.75)  # Right dashed line
  text(x = mean(lat_range), y = 5.5, labels = "ITCZ", col = "black", cex = 0.9, pos = 3)
  
  
  # map bleaching means to colors for lat_seq segments
  bleaching_colors = color_ramp(100)[as.numeric(cut(bleaching_means, breaks = 100, include.lowest = TRUE))]
  lat_seq_bins = round(lat_seq / bin_factor) * bin_factor
  segment_colors = sapply(lat_seq_bins, function(lat) {
    idx = which(lat_bins == lat)
    if (length(idx) > 0) bleaching_colors[idx] else "grey50"
  })
  
  # draw the prediction curve in colored segments
  for (i in 1:(length(lat_seq) - 1)) {
    lines(lat_seq[i:(i + 1)], spline_pred[i:(i + 1)], col = segment_colors[i], lwd = 4)
  }
  
  # Add the highest average bleaching percent in the upper right corner
  max_bleaching = round(max(bleaching_means, na.rm = TRUE) * 100, 1)  # Round to 1 decimal place
  x_pos = usr[1] + 0.99 * (usr[2] - usr[1])
  y_pos = usr[3] + 0.93 * (usr[4] - usr[3])
  text(x = x_pos, y = y_pos, labels = paste0(max_bleaching, "%"), cex = 1.25, col = "black", pos = 2)
}
dev.off()




# get data indices
index_obeta = inla.stack.index(Stack, tag = "oBetaFit")$data

# get fit
obeta_fitted = model_obeta$summary.fitted.values[index_obeta, "mean"]
range(obeta_fitted)

# plot obs~fit and pit
png(file.path(results_dir, 'Fit_PIT_042225.png'), width = 10, height = 5, units = "in", res = 300)
par(mfrow = c(1,2))
plot(data1$Percent_Bleaching ~ obeta_fitted, cex = 0.5, ylab = "Observed Bleaching", xlab = "Fitted Bleaching", ylim = c(0, 1), xlim = c(0, 1)); abline(0, 1, col = "blue", lwd = 4)
hist(model_obeta$cpo$pit, n = 50, xlab = "Probability Integral Transform", main = "") # oh no...
dev.off()


# get obeta cutpoints
xx = inla.hyperpar.sample(10^5, model_obeta, intern = FALSE)

colnames(xx)

loc = xx[, "offset location-parameter for the obeta observations"]
width = xx[, "offset width-parameter for the obeta observations"]

k1.mc.logit = loc - width
k2.mc.logit = loc + width

par(mfrow = c(1, 2))

k1.mc = inla.link.invlogit(k1.mc.logit)
k2.mc = inla.link.invlogit(k2.mc.logit)

hist(k1.mc, prob = TRUE, n = 100)
hist(k2.mc, prob = TRUE, n = 100)

k1 = mean(k1.mc)
k2 = mean(k2.mc)
print(k1)
print(k2)

# plot distribution of cutpoints and their position along the distribution of the response data
png(file.path(results_dir, 'Cutpoints_042225.png'), width = 12, height = 4, units = "in", res = 300)
par(mfrow = c(1,3), cex.lab = 1.5, cex.axis = 1.25)
hist(k1.mc, prob = TRUE, n = 100, xlab = expression(k[1]), main = "", col = "blue")
hist(k2.mc, prob = TRUE, n = 100, xlab = expression(k[2]), main = "", col = "red")
hist(data1$Percent_Bleaching, main = "", xlab = "Observed Bleaching")
y_max = max(hist(data1$Percent_Bleaching, plot = FALSE)$counts) * 0.25
segments(x0 = k1, y0 = 0, x1 = k1, y1 = y_max, col = "blue", lwd = 4)
segments(x0 = k2, y0 = 0, x1 = k2, y1 = y_max, col = "red", lwd = 4)
dev.off()


# get annual-random effect
annual_effect = as.data.frame(model_obeta$summary.random$Year)

ggplot(annual_effect) +   
  geom_line(aes(ID, `0.5quant`)) +   
  geom_line(aes(ID, `0.025quant`), linetype="dashed") +   
  geom_line(aes(ID, `0.975quant`), linetype="dashed") +
  xlab("Year") + ylab("Logit-scale annual-random effect on bleaching") +
  theme_classic() +
  scale_x_continuous(breaks = seq(min(X_Global$Year), max(X_Global$Year), by = 1)) +
  scale_y_continuous(breaks = seq(round(min(annual_effect$`0.025quant`), 1), round(max(annual_effect$`0.975quant`), 1), by = 0.2),
                     labels = scales::label_number()) +
  theme(text=element_text(size=14)) +
  geom_hline(yintercept = 0)

ggsave(file.path(results_dir, 'Year_RW1_042225.png'), width = 12, height = 8, units = "in")


# get seasonal effect
seasonal_effect = as.data.frame(model_obeta$summary.random$seasonal_month)

seasonal_walk = ggplot(seasonal_effect) +   
  geom_line(aes(ID, `0.5quant`)) +   
  geom_line(aes(ID, `0.025quant`), linetype="dashed") +   
  geom_line(aes(ID, `0.975quant`), linetype="dashed") +
  xlab("North-hemisphere-centered seasonal month (m*)") + ylab("Logit-scale seasonal-random effect on bleaching") +
  theme_classic() +
  scale_x_continuous(breaks = seq(min(X_Global$seasonal_month), max(X_Global$seasonal_month), by = 1)) +
  scale_y_continuous(breaks = seq(round(min(seasonal_effect$`0.025quant`), 1), round(max(seasonal_effect$`0.975quant`), 1), by = 0.2),
                     labels = scales::label_number()) +
  geom_hline(yintercept = 0)+
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )

seasonal_walk

ggsave(file.path(results_dir, 'Seasonal_cRW1_042225.png'), width = 12, height = 8, units = "in")



# project latent field

# get aspect ratio
lon_range = range(Global_Loc[, 1])
print(lon_range)
lat_range = range(Global_Loc[, 2])
print(lat_range)
lon_extent = diff(lon_range)
print(lon_extent)
lat_extent = diff(lat_range)
print(lat_extent)
aspect_ratio = lon_extent / lat_extent
print(aspect_ratio)

# set dims for equal resolution; approx 5 km
nrow = 1500
print(nrow)
ncol = ceiling(nrow * aspect_ratio)
print(ncol)

# calculate res
x_res = lon_extent / ncol
print(x_res)
y_res = lat_extent / nrow
print(y_res)

# projector
proj = inla.mesh.projector(MeshPred, dims = c(ncol, nrow), projection = "longlat")

# get latent data
index = inla.stack.index(Stack, "oBetaFit")$data
latent_mean = model_obeta$summary.random$w[,"mean"]
latent_sd = model_obeta$summary.random$w[,"sd"]

# project latent mean data onto rast
latent_mean_proj = inla.mesh.project(proj, latent_mean)
latent_mean_proj = t(latent_mean_proj)
latent_mean_proj = latent_mean_proj[nrow:1, ]
dim(latent_mean_proj)

latent_mean_rast = rast(
  nrows = nrow,
  ncols = ncol,
  xmin = min(proj$x),
  xmax = max(proj$x),
  ymin = min(proj$y),
  ymax = max(proj$y),
  vals = latent_mean_proj
)

print(latent_mean_rast)

crs(latent_mean_rast) = "+proj=longlat +datum=WGS84"
plot(latent_mean_rast, xlim = c(100, 140), ylim = c(-15, 15))

latent_path = "G:/Florida Tech/Research/Global/Manuscripts/Topography/Results/Latent Effects"
writeRaster(latent_mean_rast, file.path(latent_path, "mean_latent_bleaching_042225.tif"), filetype = "GTiff", overwrite = TRUE)



# project latent sd data onto rast
latent_sd_proj = inla.mesh.project(proj, latent_sd)
latent_sd_proj = t(latent_sd_proj)
latent_sd_proj = latent_sd_proj[nrow:1, ]
dim(latent_sd_proj)

latent_sd_rast = rast(
  nrows = nrow,
  ncols = ncol,
  xmin = min(proj$x),
  xmax = max(proj$x),
  ymin = min(proj$y),
  ymax = max(proj$y),
  vals = latent_sd_proj
)

print(latent_sd_rast)

crs(latent_sd_rast) = "+proj=longlat +datum=WGS84"
plot(latent_sd_rast)

latent_path = "G:/Florida Tech/Research/Global/Manuscripts/Topography/Results/Latent Effects"
writeRaster(latent_sd_rast, file.path(latent_path, "sd_latent_bleaching_042225.tif"), filetype = "GTiff", overwrite = TRUE)




# extract fitted values at observation locations
index = inla.stack.index(Stack, "oBetaFit")$data
fitted = model_obeta$summary.fitted.values$mean[index]
range(fitted) # 0.0006 - 0.9

# project fit onto mesh
A_t = t(A2_global)
fitted_mesh = as.vector(A_t %*% fitted) / rowSums(A_t) # there must be a better way of doing this...
fitted_mesh[is.nan(fitted_mesh)] = mean(fitted)
range(fitted_mesh) # 0.00097 - 0.87

# make a 5-km rast with equal lat-lon resolution
lon_range = range(Global_Loc[, 1])
lat_range = range(Global_Loc[, 2])
lon_extent = diff(lon_range)
lat_extent = diff(lat_range)
aspect_ratio = lon_extent / lat_extent
nrow = 1500
ncol = ceiling(nrow * aspect_ratio)
x_res = lon_extent / ncol
y_res = lat_extent / nrow

# project fitted mesh onto the rast
proj = inla.mesh.projector(MeshPred, dims = c(ncol, nrow), projection = "longlat")
fitted_proj = inla.mesh.project(proj, fitted_mesh)
fitted_proj = t(fitted_proj)
fitted_proj = fitted_proj[nrow:1, ]

# set the rast
fitted_rast = rast(
  nrows = nrow,
  ncols = ncol,
  xmin = min(proj$x),
  xmax = max(proj$x),
  ymin = min(proj$y),
  ymax = max(proj$y),
  vals = fitted_proj
)
crs(fitted_rast) = "+proj=longlat +datum=WGS84"

# lot
plot(fitted_rast, asp = 1)

fitted_path = "G:/Florida Tech/Research/Global/Manuscripts/Topography/Results/Fit Maps"
writeRaster(fitted_rast, file.path(fitted_path, "mean_fit_bleaching_5km_042225.tif"), filetype = "GTiff", overwrite = TRUE)


