inla.setOption(inla.call = "inla.mkl.work")

latent_ar1_testdata <- function() {
  data1 <- data.frame(
    time = c(1, 2, 3, 5, 4),
    obs = c(1, 2, 1, 4, 2)
  )
  data1 <- data.frame(
    time = c(1, 2, 3.5, 5, 4),
    obs = c(1, 2, 1, 4, 2)
  )
  data2 <- data1[5:1, , drop = FALSE]
  data3 <- data1[c(2, 5, 4, 3, 1), , drop = FALSE]

  cmp <- obs ~ time(time, model = "ar1") - Intercept
  list(
    data = list(data1 = data1, data2 = data2, data3 = data3),
    cmp = cmp
  )
}


old <- function() {
  fit$fit_inla <- list(
    INLA::inla(obs ~ f(time, model = "ar1") - 1,
      data = fit$data[[1]], family = "gaussian", 
      verbose = TRUE
    ),
    INLA::inla(obs ~ f(time, model = "ar1") - 1,
      data = fit$data[[2]], family = "gaussian", 
      verbose = TRUE
    ),
    INLA::inla(obs ~ f(time, model = "ar1") - 1,
      data = fit$data[[3]], family = "gaussian", 
      verbose = TRUE
    )
  )

  cbind(
    fit$fit_inla[[1]]$summary.hyperpar[, "mean"],
    fit$fit_inla[[2]]$summary.hyperpar[, "mean"],
    fit$fit_inla[[3]]$summary.hyperpar[, "mean"]
  )
  cbind(
    fit$fit_inla[[1]]$summary.random$time$mean,
    fit$fit_inla[[2]]$summary.random$time$mean,
    fit$fit_inla[[3]]$summary.random$time$mean
  )
}

fit <- latent_ar1_testdata()
old()
