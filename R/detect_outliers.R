globalVariables(c(".fitted", ".model", ".resid", "arima",
                  "ave", "comb", "croston", "ets", "index",
                  "lm", "naive", "neural", "res_comb",
                  "snaive", "value", "frequency" ))
#' Flagged anomalies and their degree of outlierness
#'
#' @description Flagged anomalies and their degree of
#'  outlierness
#'
#' @param .data A tsibble.
#' @param variable A column to find outliers
#' @param cmbn_model A vector of models to compute combination forcast
#' @param ... Other arguments passed to [fabletools::model()].
#' @importFrom feasts STL
#' @importFrom fabletools model autoplot augment combination_model
#' @importFrom stray find_HDoutliers
#' @importFrom tidyr pivot_wider
#' @importFrom fasster FASSTER
#' @import fable
#' @import dplyr
#' @export
#' @examples
#' library(fabletools)
#' data <- USAccDeaths
#' # Convert 10th data point as an outlier
#' data[10] <- 700000
#' p <- data %>%
#'   tsibble::as_tsibble() %>%
#'   detect_outliers(variable = "value",
#'   cmbn_model = c("naive","snaive", "ave"))
#' p
detect_outliers <- function(.data, variable,
                            cmbn_model = c("ets" , "arima", "snaive",
                                           "naive", "croston",
                                           "ave", "neural", "lm",
                                           "theta", "fasster"), ...) {
  stopifnot(is_tsibble(.data))

  frq <- frequency(.data)
  model_list <-c(
   ets = fable::ETS(!!rlang::ensym(variable)),
   arima =fable::ARIMA(!!rlang::ensym(variable)),
   snaive = fable::SNAIVE(!!rlang::ensym(variable)),
   croston= fable::CROSTON(!!rlang::ensym(variable)),
   ave= fable::MEAN(!!rlang::ensym(variable)),
   naive= fable::NAIVE(!!rlang::ensym(variable)),
   neural= fable::NNETAR(!!rlang::ensym(variable)),
   lm=fable::TSLM(!!rlang::ensym(variable) ~ trend()+season()),
   theta = fable::THETA(value),
   fasster = fasster::FASSTER(value ~ trend(1) + fourier(frq))
)

  mod_idx <- which(!names(model_list) %in% cmbn_model)
  model_list[mod_idx] <- NULL
  model_specs <- rlang::set_names(model_list %>% unlist())
  # print(model_specs)

  mod <-.data %>%
    fabletools::model(
      cmbn_mod = fabletools::combination_model(!!!model_specs)
      )

  res <- augment(mod)

  return(res)
}
