globalVariables(c(".altered"))
#' Replace outliers and missing values in tidy time series data.
#'
#' @description Replace outliers and missing values in
#' the output returned by \code{detect_outliers}
#'
#' @param .data A tsibble returned by \code{detect_outliers}
#' @param variable A column to find outliers
#' @param replace.missing If TRUE, it not only replaces outliers,
#'  but also interpolates missing values. Defualt is set to TRUE.
#' @param impute Method to be used for replacing each NA with
#'  interpolated values: "linear" or "spline." Default is set to "linear"
#' @importFrom dplyr mutate
#' @importFrom tsibble index is_tsibble
#' @importFrom zoo na.approx na.spline
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
#' altered_data <- p %>% cleanse_data(variable = "value", impute = "spline")
#' altered_data %>% fabletools::autoplot(.altered)
#'
cleanse_data <- function(.data, variable, replace.missing = TRUE,
                         impute= "linear")
{
  stopifnot( is_outstable(.data))
  index <- tsibble::index(.data)

  data <- .data %>%
    dplyr::mutate(.altered =
                    ifelse(.data$.outtype == "typical",
                           !!rlang::ensym(variable),NA))

  if(replace.missing)
  {
    # Find the index position of the first non-NA value
    nonNAindex <- which(!is.na(data$.altered))
    firstNonNA <- min(nonNAindex) - 1

    if(impute == "linear")
    {
      alt <- c(rep(NA,firstNonNA),
               zoo::na.approx(.altered))
      data <- data %>%
        dplyr::mutate(
          .altered = alt)
    }
    if(impute == "spline")
    {
      alt <- c(rep(NA,firstNonNA),
               zoo::na.spline(.altered))
      data <- data %>%
        dplyr::mutate(
          .altered = alt)
    }

  }
  return(data)
}
