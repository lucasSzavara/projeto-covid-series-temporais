library(dplyr)

umbrae <- function(y_true, y_pred, y_pred_baseline=c()) {
  if (length(y_pred_baseline) == 0) {
    y_pred_baseline <- lag(y_true, default=c(0))
  }
  et <- (y_pred - y_true)^2
  et_star <- (y_pred_baseline - y_true)^2
  brae <- et / (et + et_star)
  # print(paste('BRAE was null in ', mean(is.na(brae)) * 100, '% of cases'))
  brae[is.na(brae)] <- 0.5
  mbrae <- mean(brae)
  return(mbrae / (1 - mbrae))
}

mad.mean <- function(y_true, y_pred) {
  mad <- mean(abs(y_pred - y_true))
  return(mad / mean(y_true))
}
