#' Sample greenhouse gas emission scenarios for Ireland 2025,2030,2035,2050 & 2100
#'
#' A sample dataset containing annual emissions CO$_2$ (fossil-co2), CH4, N2O and LULUCF (CO2) as a percentage of 2018 emissions.
#'
#' @format A data frame with 1100 rows and 3 variables:
#' \describe{
#'   \item{scenario}{scenario name}
#'   \item{gas}{wmghg}
#'   \item{year}{year}
#'   \item{factor}{fraction of 2018 emissions in year}
#'   \item{ktCO2e}{emissions in year expressed in ktCO2-eq using AR4 GWP$_{100}$ values (i.e. ch4 25, n20 298 )}
#' }
#' @source 1990-2018 \url{http://www.epa.ie/} \url{http://www.cso.ie/}
"scenarios"
