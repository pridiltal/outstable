#' Set a threshold for outlier detection
#'
#' @description This function forecasts a boundary
#' for the typical behaviour using a representative
#' sample of the typical behaviour derived from a
#' given vector, An approach based on extreme value
#' theory is used for this boundary prediction process.
#' @param x A numerical vector
#' @param p_rate False positive rate. Default value is set to 0.001
#' @param trials Number of trials to generate the extreme value distirbution. Default value is set to 500.
#' @return Returns a list of flagged anomalies and their degree of
#'  outlierness
#' @export
#' @importFrom ks  kde rkde
#' @importFrom stats quantile na.omit
#' @references Clifton, D. A., Hugueny, S., & Tarassenko, L. (2011). Novelty detection with multivariate extreme value statistics.
#' Journal of signal processing systems, 65 (3),371-389.
#' @examples
#' p <- c(rnorm(100),1000)
#' out <- outstable::set_cutoff(p, p_rate = 0.01)
#' print(out)
#'
set_cutoff <- function(x, p_rate = 0.01, trials = 500) {

  cutoff <- stats::quantile(x, na.rm = TRUE,
                     probs=c(p_rate/2,(1-p_rate/2)))
  typical <- x[x>cutoff[1] & x <cutoff[2]] %>%
    stats::na.omit() %>%
    as.matrix()

  # Calculating the density region for typical data
  fhat2 <- ks::kde(x = typical,
                   compute.cont = TRUE,
                   eval.points = na.omit(x))

  # Generating data to find the anomalous threshold value
  m <- nrow(typical)
  xtreme_fx <- numeric(trials)
  f_extreme <- function(x) {
  s <- ks::rkde(m, fhat2, positive=FALSE)
  fhat <- ks::kde(
      x = typical,  compute.cont = TRUE,
      eval.points = s)
    return(tempt <- min(fhat$estimate))
  }


  xtreme_fx <- sapply(X = xtreme_fx,
                      f_extreme)


  k <- 1 / (2 * pi)
  psi_trans <- ifelse(
    xtreme_fx < k, (-2 * log(xtreme_fx) - 2 * log(2 * pi))^0.5, 0)
  p <- sum(!(psi_trans == 0)) / length(psi_trans)
  y <- -log(-log(1 - p_rate * p))
  cm <- sqrt(2 * log(m)) - ((log(log(m)) + log(4 * pi)) / (2 * sqrt(2 * log(m))))
  dm <- 1 / (sqrt(2 * log(m)))
  t <- cm + y * dm
  threshold_fnx <- exp(-((t^2) + 2 * log(2 * pi)) / 2)
  type <- as.factor(ifelse(fhat2$estimate < threshold_fnx,
                 "outlier", "typical"))

  return(list(out_score =  fhat2$estimate, type = type, threshold = threshold_fnx))
}
