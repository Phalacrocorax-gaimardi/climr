#' Hector fit parameters from Dorheim et al
#'
#' 6 emissions driven ESMs, partially penalised 7-parameter regression fits
#'
#'
#' @format A data frame with 6 rows and 8 variables:
#' \describe{
#'   \item{ESM}{ESM model code}
#'   \item{alpha_a}{anthropgenic aerosol scaling factor}
#'   \item{alpha_v}{volcano aerosol scaling factor}
#'   \item{ecs}{equilibrium climate sensitivity}
#'   \item{kappa}{vertical ocean diffusivity}
#'   \item{C_0}{preindustrial co2}
#'   \item{beta}{carbon fertilisation}
#'   \item{Q_10}{Q10}
#' }
#' @source \url{https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2019EA000980}
"hector_esm_fits"
