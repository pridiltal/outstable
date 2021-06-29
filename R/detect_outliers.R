globalVariables(c(".fitted", ".model", ".resid", "arima",
                  "ave", "comb", "croston", "ets", "index",
                  "lm", "naive", "neural", "res_comb",
                  "snaive", "value", "frequency", ".outtype" ))
#' Flagged anomalies and their degree of outlierness
#'
#' @description Flagged anomalies and their degree of
#'  outlierness
#'
#' @param .data A tsibble.
#' @param variable A column to find outliers
#' @param cmbn_model A vector of models to compute combination forcast
#' @param p_rate False positive rate. Default value is set to 0.001.
#' @param trials Number of trials to generate the extreme value distirbution. Default value is set to 500.
#' @param ... Other arguments passed to [fabletools::model()].
#' @return It returns a tsibble with class 'outstable' containing the following components:
#'    \item{.outscore}{The degree of outlierness.}
#'    \item{.outtype}{Type of the data point" outlier or typical}
#' @importFrom feasts STL
#' @importFrom fabletools model autoplot augment combination_model
#' @importFrom tidyr pivot_wider drop_na
#' @importFrom fasster FASSTER
#' @importFrom tsibble index index_var is_tsibble
#' @import fable
#' @import dplyr
#' @export
#' @examples
#' library(fabletools)
#' data <- USAccDeaths
#' # Convert 20th data point as an outlier
#' data[20] <- data[20]*5
#' p <- data %>%
#'   tsibble::as_tsibble() %>%
#'   detect_outliers(variable = "value",
#'   cmbn_model = c("arima", "theta", "ave"),
#'   p_rate = 0.001)
#' p %>% dplyr::filter(.outtype == "outlier")
#' p %>% fabletools::autoplot(value)
#'
detect_outliers <- function(.data, variable,
                            cmbn_model = c("ets" , "arima", "snaive",
                                           "naive", "croston",
                                           "ave", "neural", "lm",
                                           "theta", "fasster"),
                            p_rate = 0.01, trials = 500, ...) {
  stopifnot(tsibble::is_tsibble(.data))
  index <- tsibble::index(.data)
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
   theta = fable::THETA(!!rlang::ensym(variable)),
   fasster = fasster::FASSTER(!!rlang::ensym(variable) ~ trend(1) + fourier(frq))
)

  mod_idx <- which(!names(model_list) %in% cmbn_model)
  model_list[mod_idx] <- NULL
  model_specs <- rlang::set_names(model_list %>% unlist())
  # print(model_specs)

  mod <-.data %>%
    fabletools::model(
      cmbn_mod = fabletools::combination_model(!!!model_specs)
      )

  # Find outliers based on residuals
  res <- augment(mod) %>%
    dplyr::select(index, .resid) %>%
    tidyr::drop_na()

  out <- set_cutoff(res$.resid, p_rate = p_rate, trials = trials)

  res <- res %>%
    dplyr::mutate(.outscore = unitize(out$out_score),
                  .outtype = out$type ) %>%
    dplyr::select(-.resid)

  data <- dplyr::left_join(.data, res, by = tsibble::index_var(.data))

  #structure(data, "outstable")
  class(data) <- c("outstable", "tbl_ts",
                   "tbl_df", "tbl", "data.frame")
  return(data)
}

unitize <- function(z) {
  zrange <- range(z)
  if (!(dif <- diff(zrange))) {
    return(rep(0, length(z)))
  }
  ((z - zrange[2]) / dif)*(-1)
}
