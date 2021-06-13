globalVariables(c(".fitted", ".model", ".resid", "arima",
                  "ave", "comb", "croston", "ets", "index",
                  "lm", "naive", "neural", "res_comb",
                  "snaive", "value" ))
#' Flagged anomalies and their degree of outlierness
#'
#' @description Flagged anomalies and their degree of
#'  outlierness
#'
#' @param .data A tsibble.
#' @param  variable A column to find outliers
#' @param ... Other arguments passed to [feasts::STL()].
#' @importFrom feasts STL
#' @importFrom fabletools model autoplot augment
#' @importFrom stray find_HDoutliers
#' @importFrom tidyr pivot_wider
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
#'   detect_outliers("value", window = 40)
#' p
detect_outliers <- function(.data, variable, ...) {
  stopifnot(is_tsibble(.data))

 # decomp_tbl <- d %>%
  #  model(
  #    STL(!!rlang::ensym(variable) ~ trend(window = window) +
   #     season(window = "periodic"), robust = TRUE)
   # ) %>%
   # components()

  mod <-.data %>%
    fabletools::model(
      ets = fable::ETS(!!rlang::ensym(variable)),
      arima = fable::ARIMA(!!rlang::ensym(variable)),
      snaive = fable::SNAIVE(!!rlang::ensym(variable)),
      croston= fable::CROSTON(!!rlang::ensym(variable)),
      ave= fable::MEAN(!!rlang::ensym(variable)),
      naive= fable::NAIVE(!!rlang::ensym(variable)),
      neural= fable::NNETAR(!!rlang::ensym(variable)),
      lm=fable::TSLM(!!rlang::ensym(variable) ~ trend()+season())
    )

  rescomb <-  fabletools::augment(mod) %>%
    dplyr::select(index, .model, .fitted, value) %>%
    tidyr::pivot_wider(names_from = .model, values_from = .fitted) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(comb = mean(c(ets, arima,snaive,
                           croston, ave, naive,
                           neural, lm), na.rm = TRUE),
                  res_comb = value - comb) %>%
    select(res_comb)

   res <- augment(mod) %>%
     dplyr::select(index, .model, .resid, value) %>%
     tidyr::pivot_wider(names_from = .model, values_from = .resid) %>%
     mutate(res_comb = rescomb$res_comb )

   #res <- res %>% left_join(rescomb, by = "index" )
  # out <- stray::find_HDoutliers(decomp_tbl$remainder)

 # outstable <- .data %>%
 #   mutate(type = out$type, outscore = out$out_scores)

 # print(decomp_tbl %>% fabletools::autoplot())
  return(res)
}
