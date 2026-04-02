options(width = 200)

library(INLAjoint)
library(dplyr)
library(ggplot2)

# Load preprocessed data
load("dataHRS_BMI_full.RData")

ids_survival <- unique(survival_data$HHIDPN)
ids_longitudinal <- unique(longitudinal_data$HHIDPN)
common_ids <- intersect(ids_survival, ids_longitudinal)
surv <- survival_data %>%
  filter(HHIDPN %in% common_ids) %>%
  as.data.frame()

lon <- longitudinal_data %>%
  filter(HHIDPN %in% common_ids) %>%
  as.data.frame()

# Create contiguous ID mapping
id_mapping <- data.frame(
  HHIDPN = sort(unique(c(surv$HHIDPN, lon$HHIDPN))),
  id = seq_along(unique(c(surv$HHIDPN, lon$HHIDPN)))
)
surv <- surv %>%
  left_join(id_mapping, by = "HHIDPN") %>%
  select(-HHIDPN) %>%
  as.data.frame()

lon <- lon %>%
  left_join(id_mapping, by = "HHIDPN") %>%
  select(-HHIDPN) %>%
  as.data.frame()

# Survival formula
surv_formula <- inla.surv(surv_time_decades, status) ~ age_cat + gender + race +
  smoking + hypertension + diabetes + heart_disease + stroke + cancer

# Longitudinal formula
long_formula <- lbmi ~ time_decades * (age_cat + gender) + race + smoking +
  hypertension + diabetes + heart_disease + stroke + cancer +
  (1 + time_decades | id)
#
# JM_CV_CS <- joint(
#   formLong = long_formula,
#   formSurv = surv_formula,
#   dataSurv = surv,
#   dataLong = lon,
#   family = "gaussian",
#   id = "id",
#   timeVar = "time_decades",
#   assoc = c("CV_CS"),
#   basRisk = "rw2",
#   control = list(int.strategy = "eb")
# )
# summary(JM_CV_CS, sdcor = TRUE, hr = TRUE)
# save(JM_CV_CS, file = "JM_CV_CS_full.RData")
#
# JM_L_CV_CS <- joint(
#   formLong = long_formula,
#   formSurv = surv_formula,
#   dataSurv = surv,
#   dataLong = lon,
#   family = "gaussian",
#   id = "id",
#   timeVar = "time_decades",
#   assoc = c("L_CV_CS"),
#   basRisk = "rw2",
#   control = list(int.strategy = "eb",
#                  NLpriorAssoc = list(
#                    mean = list(mean = 0, prec = 0.01, initial = 0.1),
#                    slope = list(mean = 0, prec = 0.01, initial = 0.1),
#                    spline = list(mean = 0, prec = 30, initial = 0.1)))
# )
# summary(JM_L_CV_CS, sdcor = TRUE, hr = TRUE)
# save(JM_L_CV_CS, file = "JM_L_CV_CS_full.RData")

JM_NL_CV_CS <- joint(
  formLong = long_formula,
  formSurv = surv_formula,
  dataSurv = surv,
  dataLong = lon,
  family = "gaussian",
  id = "id",
  timeVar = "time_decades",
  assoc = c("NL_CV_CS"),
  basRisk = "rw2",
  control = list(int.strategy = "eb",
                 keep = TRUE, 
                 NLpriorAssoc = list(
                   mean = list(mean = 0, prec = 0.01, initial = 0.1),
                   slope = list(mean = 0, prec = 0.01, initial = 0.1),
                   spline = list(mean = 0, prec = 30, initial = 0.1)))
)
summary(JM_NL_CV_CS, sdcor = TRUE, hr = TRUE)
# save(JM_NL_CV_CS, file = "JM_NL_CV_CS_full.RData")

