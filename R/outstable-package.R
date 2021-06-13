#' outstable: Tidy Time Series Anomaly Detection
#'
#' The **outstable** package provides a framework for detecting
#' anomalies in tidy time series data. An anomaly is defined as an
#' observation that is predicted as very unlikely given the robust time
#' series forecast models. The algorithm works with tidy temporal data
#' provided by the tsibble package and produces an outstable, a tsibble
#' with flagged anomalies and their degree of outlierness. An approach
#' based on extreme value theory is applied to residual series in order to
#' calculate a data-driven anomalous threshold. The proposed framework
#' can also provide a cleansed tsibble that closely integrates with the
#' tidy forecasting workflow used in the fable package. A number of
#' different approaches are available for the data cleansing process.
#'
#' @aliases NULL outstable-package
#' @importFrom rlang .data %||% set_names sym expr enexpr quo enquo parse_expr
#' @importFrom tsibble is_tsibble
#' @importFrom dplyr select mutate
#' @keywords internal
"_PACKAGE"
# allowing for the use of the dot when piping
utils::globalVariables(c(".", "components"))
