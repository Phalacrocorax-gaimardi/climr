#usethis::use_pipe()


#' getIRLContrib
#'
#' returns Ireland contribution to variables (emissions,concentrations,forcing,GSAT)
#'
#' @param scenario_name national scenario
#' @param rcp_path pathway (RCP or SSP)
#' @param ecs equilibrium climate sensitivity
#' @param kappa vertical ocean diffusivity (default 2.3 cm2/s)
#' @param alpha_a anthropogenic aerosol scaling (unitless, default value unity)
#' @param alpha_v volcanic aerosol scaling (unitless, default value unity)
#' @param C_0 preindustrial co2 concentration (ppmv)
#' @param beta carbon ferilisation
#' @param Q_10 soil carbon feedback
#' @param M0 pre-industrial methane concentration (ppbv)
#'
#' @return tibble containing emissions and Tgav
#' @export
#'
#' @examples
getIRLContrib <- function(scenario_name,rcp_path,ecs=3,kappa=2.3,alpha_a=1,alpha_v=1, C_0 = 276.0897, beta = 0.36 , Q_10 = 2, M0=653){
  #format scenario dataframe
  if( !(rcp_path %in% pathways)) stop(paste("pathway must be in",paste(pathways,collapse=" ")))
  scen <- dplyr::filter(scenarios_ipcc,scenario==scenario_name) %>% dplyr::select(-scenario,-factor)
  scen <- dplyr::bind_rows(allgas_irl,scen)
  #scen %>% ggplot(aes(Year,ktCO2e/1e+3,colour=gas)) +  geom_line()
  scen <- dplyr::left_join(tidyr::expand_grid(year=1745:2100, gas=c("co2","ch4","n2o","lulucf","so2")),scen)
  scen <- scen %>% dplyr::group_by(gas) %>% dplyr::mutate(value=zoo::na.approx(value))
  scen <- scen %>% dplyr::rowwise() %>% dplyr::mutate(units=gasunits(gas))
  #scen %>% ggplot(aes(Year,ktCO2e/1e+3,fill=gas)) + geom_area()
  scen <- scen %>% dplyr::arrange(gas)
  scen$gas <- stringr::str_replace(scen$gas,"ch4","CH4_emissions")
  scen$gas <- stringr::str_replace(scen$gas,"co2","ffi_emissions")
  scen$gas <- stringr::str_replace(scen$gas,"n2o","N2O_emissions")
  scen$gas <- stringr::str_replace(scen$gas,"lulucf","luc_emissions")
  scen$gas <- stringr::str_replace(scen$gas,"so2","SO2_emissions")
  names(scen)[2] <- c("variable")
  #change units to Gg S
  scen <- scen %>% dplyr::rowwise() %>% dplyr::mutate(value = replace(value,variable=="SO2_emissions",value*1000))
  #Tg -> Mt
  #Pg -> Gt
  rcp_ini <- system.file("input/hector_rcp26.ini", package = "hector")
  # rcp_ini <- switch(rcp_path,
  #                    "rcp26" = system.file("input/hector_rcp26.ini", package = "hector"),
  #                   "rcp45" = system.file("input/hector_rcp45.ini", package = "hector"),
  #                  "rcp60" = system.file("input/hector_rcp60.ini", package = "hector") ,
  #                 "rcp85" = system.file("input/hector_rcp85.ini", package = "hector")
  #)

  core <- hector::newcore(rcp_ini)
  core_exirl <- hector::newcore(rcp_ini)
  hector::reset(core)
  hector::setvar(core,NA,hector::ECS(),ecs,unit='degC')
  hector::setvar(core,NA,hector::DIFFUSIVITY(),kappa,unit='cm2/s')
  hector::setvar(core,NA,hector::AERO_SCALE(),alpha_a,unit='(unitless)')
  hector::setvar(core,NA,hector::VOLCANIC_SCALE(),alpha_v,unit='(unitless)')
  hector::setvar(core,NA,hector::PREINDUSTRIAL_CO2(),C_0,unit='ppmv CO2')
  hector::setvar(core,NA,hector::BETA(),beta,unit='(unitless)')
  hector::setvar(core,NA,hector::Q10_RH(),Q_10,unit='(unitless)')
  hector::setvar(core,NA,hector::PREINDUSTRIAL_CH4(),M0,unit='ppbv CH4')

  hector::setvar(core_exirl,NA,hector::ECS(),ecs,unit='degC')
  hector::setvar(core_exirl,NA,hector::DIFFUSIVITY(),kappa,unit='cm2/s')
  hector::setvar(core_exirl,NA,hector::AERO_SCALE(),alpha_a,unit='(unitless)')
  hector::setvar(core_exirl,NA,hector::VOLCANIC_SCALE(),alpha_v,unit='(unitless)')
  hector::setvar(core_exirl,NA,hector::PREINDUSTRIAL_CO2(),C_0,unit='ppmv CO2')
  hector::setvar(core_exirl,NA,hector::BETA(),beta,unit='(unitless)')
  hector::setvar(core_exirl,NA,hector::Q10_RH(),Q_10,unit='(unitless)')
  hector::setvar(core_exirl,NA,hector::PREINDUSTRIAL_CH4(),M0,unit='ppbv CH4')

  #rcp <- read_csv(paste("~/Policy/CCACBudgets/scenarios/",rcp_path,".csv",sep=""))
  rcp <- ssps %>% dplyr::filter(pathway==rcp_path)
  hector::setvar(core,1745:2100,var=hector::FFI_EMISSIONS(), values= dplyr::filter(rcp, variable=="ffi_emissions")$value, unit = "Pg C/yr")
  hector::setvar(core,1745:2100,var=hector::EMISSIONS_CH4(), values= dplyr::filter(rcp, variable=="CH4_emissions")$value, unit = "Tg CH4")
  hector::setvar(core,1745:2100,var=hector::EMISSIONS_N2O(), values= dplyr::filter(rcp, variable=="N2O_emissions")$value, unit = "Tg N")
  hector::setvar(core,1745:2100,var=hector::LUC_EMISSIONS(), values= dplyr::filter(rcp, variable=="luc_emissions")$value, unit = "Pg C/yr")
  hector::setvar(core,1745:2100,var=hector::EMISSIONS_SO2(), values= dplyr::filter(rcp, variable=="SO2_emissions")$value, unit = "Gg S")
  hector::run(core)
  #rcp26 %>% filter(variable %in% c("ffi_emissions","luc_emissions")) %>% ggplot(aes(year,value,colour=variable)) + geom_line() #+ scale_y_continuous(trans="sqrt")
  rcp_exirl <- rcp
  rcp_exirl$scenario <- "rcp_exirl"
  scen <- scen %>% dplyr::rename("irl"=value)
  rcp_exirl <- dplyr::left_join(rcp_exirl,scen)
  rcp_exirl <- rcp_exirl %>% dplyr::mutate(irl=tidyr::replace_na(irl,0))
  rcp_exirl <- rcp_exirl %>% dplyr::mutate(value=value-irl)
  rcp_exirl <- rcp_exirl %>% dplyr::select(-irl)
  #rcp26_exirl %>% filter(variable=="ffi_emissions") %>% ggplot(aes(year,value)) + geom_line()

  f1 <- tibble::as_tibble(hector::fetchvars(core,1745:2100,c(hector::FFI_EMISSIONS(),hector::EMISSIONS_CH4(),hector::EMISSIONS_N2O(),hector::LUC_EMISSIONS(),hector::EMISSIONS_SO2(),"Tgav",
                                             hector::ATMOSPHERIC_CO2(),hector::ATMOSPHERIC_CH4(),hector::ATMOSPHERIC_N2O()),scenario=rcp_path))[,-1]
  #f1 %>% filter(variable=="Tgav") %>% ggplot(aes(year,value)) + geom_line()
  hector::setvar(core_exirl,1745:2100,var=hector::FFI_EMISSIONS(), values= dplyr::filter(rcp_exirl, variable=="ffi_emissions")$value, unit = "Pg C/yr")
  hector::setvar(core_exirl,1745:2100,var=hector::EMISSIONS_CH4(), values= dplyr::filter(rcp_exirl, variable=="CH4_emissions")$value, unit = "Tg CH4")
  hector::setvar(core_exirl,1745:2100,var=hector::EMISSIONS_N2O(), values= dplyr::filter(rcp_exirl, variable=="N2O_emissions")$value, unit = "Tg N")
  hector::setvar(core_exirl,1745:2100,var=hector::LUC_EMISSIONS(), values= dplyr::filter(rcp_exirl, variable=="luc_emissions")$value, unit = "Pg C/yr")
  hector::setvar(core_exirl,1745:2100,var=hector::EMISSIONS_SO2(), values= dplyr::filter(rcp_exirl, variable=="SO2_emissions")$value, unit = "Gg S")
  #reset(core26)
  hector::run(core_exirl)
  f1 <- f1 %>% dplyr::rename("global"=value)
  f2 <- tibble::as_tibble(hector::fetchvars(core_exirl,1745:2100,c(hector::FFI_EMISSIONS(),hector::EMISSIONS_CH4(),hector::EMISSIONS_N2O(),hector::LUC_EMISSIONS(),"Tgav",
                                                   hector::ATMOSPHERIC_CO2(),hector::ATMOSPHERIC_CH4(),hector::ATMOSPHERIC_N2O()),scenario=paste(rcp_path,"exirl",sep="_")))[,-1]
  f2 <- f2 %>% dplyr::rename("global_exirl"=value)
  f <- dplyr::inner_join(f1,f2)
  f <- f %>% dplyr::mutate(ireland=global-global_exirl)
  #f1 %>% filter(year=="2050") %>% print.data.frame()
  #f2 %>% filter(year=="2050") %>% print.data.frame()
  f$scenario <- scenario_name
  f$pathway <- rcp_path
  f$ecs <- ecs
  f <- f %>% dplyr::select(pathway,scenario,variable,year,global,ireland,units)
  #print(paste("ECS =",hector::fetchvars(core,NA,hector::ECS())$value))
  hector::shutdown(core)
  hector::shutdown(core_exirl)
  return(f)
}





#' getIRL_dGSAT
#'
#' Ireland contribution to GSAT (global-mean surface air temperature)
#' wrapper function based on getIRLContrib
#'
#' @param scenario_name national scenario
#' @param rcp_path pathway (RCP or SSP)
#' @param ecs equilibrium climate sensitivity
#' @param kappa vertical ocean diffusivity (default 2.3 cm2/s)
#' @param alpha_a anthropogenic aerosol scaling (unitless, default value unity)
#' @param alpha_v volcanic aerosol scaling (unitless, default value unity)
#' @param C_0 preindustrial co2 concentration (ppmv)
#' @param beta carbon ferilisation
#' @param Q_10 soil carbon feedback
#' @param M0 pre-industrial methane concentration (ppbv)
#'
#' @return tibble
#' @export
#'
#' @examples
getIRL_dGSAT <- function(scenario_name,rcp_path,ecs=3,kappa=2.3,alpha_a=1,alpha_v=1,C_0=276.0897,beta=0.36,Q_10=2,M0=653){

  res <- getIRLContrib(scenario_name,rcp_path,ecs,kappa,alpha_a,alpha_v,C_0,beta,Q_10) %>% filter(variable=="Tgav") %>% select(-variable,-units)
  res$kappa <- kappa
  res$alpha_a <- alpha_a
  res$alpha_v <- alpha_v
  res$C_0 <- C_0
  res$beta <- beta
  res$Q_10 <- Q_10
  return(res)

}


#' IPCC emissions unit lookup
#'
#' @param gas name - co2, ch4, n20, lulucf, so2
#'
#' @return unit
#' @export
#'
#' @examples
gasunits <- function(gas){
  #NB fair has Tg S units
  switch(gas,
         "co2"="Pg C/yr",
         "ch4"= "Tg CH4",
         "n2o"= "Tg N",
         "lulucf"= "Pg C/yr",
         "so2"= "Gg S")
}



