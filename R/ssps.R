#' Shared Socioeconomic Pathways (and RCPs)
#'
#' Expressed as annual data 1945-2100 (linearly interpolated)
#'
#'
#' @format A data frame with 28480 rows and 5 variables:
#' \describe{
#'   \item{pathway}{path from 16 pathways 4 rcps and 12 ssps (ssp1_19 to ssp3_60)}
#'   \item{year}{year}
#'   \item{variable}{gas ffi_emissions,CH4_emissions, N2O_emissions,luc_emissions,SO2_emissions}
#'   \item{value}{emissions value (IPCC units)}
#'   \item{units}{units}
#' }
#' @source 1990-2018 \url{http://www.epa.ie/} \url{http://www.cso.ie/}
"ssps"
