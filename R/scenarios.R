#' Multi-gas emissions scenarios for Ireland
#'
#' Scenarios expressed as fraction of 2018 emissions in 2025, 2030,2035,2050,2100
#' Linear interpolation is assumed by climr between these years.
#'
#' Scenario labels A, B, C, D, E
#'
#' @format A data frame with 100 rows and 5 variables:
#' \describe{
#'   \item{scenario}{scenario name}
#'   \item{gas}{wmghg. co2 = co2-fossil lulucf = co2-land}
#'   \item{year}{year}
#'   \item{factor}{fraction of 2018 emissions from EPA}
#'   \item{ktCO2e}{implied emissions in ktCO2-eq using AR4 GWP100}
#' }
#' @source 1990-2018 \url{http://www.epa.ie/} \url{http://www.cso.ie/}
"scenarios"
