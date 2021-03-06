#' Test if the object is an outstable
#'
#' @description This function returns TRUE for
#'  outstable or subclasses thereof, and
#'  FALSE for all other objects.
#'  Outstable can be generated by \code{detect_outliers}
#'
#' @param x An object
#' @return TRUE if the object inherits from the `outstable` class.
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
#' is_outstable(p)
#'
is_outstable <- function(x) {
  inherits(x, "outstable")
}
