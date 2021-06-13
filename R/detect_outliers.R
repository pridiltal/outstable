#' Flagged anomalies and their degree of outlierness
#'
#' @description Flagged anomalies and their degree of
#'  outlierness
#'
#' @param .data A tsibble.
#' @param  variable A column to find outliers
#' @param ... Other arguments passed to [feasts::STL()].
#' @importFrom feasts STL
#' @importFrom fabletools model autoplot
#' @importFrom stray find_HDoutliers
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
    model(
      ets = ETS(!!rlang::ensym(variable)),
      arima = ARIMA(!!rlang::ensym(variable)),
      snaive = SNAIVE(!!rlang::ensym(variable)),
      croston= CROSTON(!!rlang::ensym(variable)),
      ave=MEAN(!!rlang::ensym(variable)),
      naive=NAIVE(!!rlang::ensym(variable)),
      neural=NNETAR(!!rlang::ensym(variable)),
      lm=TSLM(!!rlang::ensym(variable) ~ trend()+season())
    )

  fit <- augment(mod) %>%
    dplyr::select(index, .model, .fitted, value) %>%
    pivot_wider(names_from = .model, values_from = .fitted) %>%
    rowwise() %>%
    mutate(comb = mean(c(ets, arima,snaive,
                           croston, ave, naive,
                           neural, lm), na.rm = TRUE))
 # out <- stray::find_HDoutliers(decomp_tbl$remainder)

 # outstable <- .data %>%
 #   mutate(type = out$type, outscore = out$out_scores)

 # print(decomp_tbl %>% fabletools::autoplot())
  return(fit)
}
