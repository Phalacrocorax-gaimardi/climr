#usethis::use_pipe()


#' getIRLContrib
#'
#' compute the Irish contribution to global warming based on historical data in ghg_irl
#'
#' @param scenario_name name of scenario from scenarios A, B, C, D, E
#' @param rcp_path names of RCP pathway currently RCP29,RCP45,RCP60, RCP85
#' @param ecs value for equilibrium climate sensitivity (typical 3C)
#'
#' @return a tibble year Tgav rcp scenarion
#' @export
#'
#' @examples
#' getIRLContrib("A","rcp85",2.9)
#'
getIRLContrib <- function(scenario_name,rcp_path,ecs){
  #format scenario dataframe
  scen <- dplyr::filter(scenarios,scenario==scenario_name) %>% dplyr::select(-scenario,-factor)
  scen <- dplyr::bind_rows(ghg_irl,scen)
  #scen %>% ggplot(aes(Year,ktCO2e/1e+3,colour=gas)) +  geom_line()
  scen <- dplyr::left_join(tidyr::expand_grid(year=1745:2100, gas=c("co2","ch4","n2o","lulucf")),scen)
  scen <- scen %>% dplyr::group_by(gas) %>% dplyr::mutate(ktCO2e=zoo::na.approx(ktCO2e))
  #scen %>% ggplot(aes(Year,ktCO2e/1e+3,fill=gas)) + geom_area()
  gwps <- tibble::tibble(gas=c("co2","ch4","n2o","lulucf"),gwp=c(1,25,298,1)) #accounting gwp100s
  scen <- dplyr::inner_join(scen,gwps)
  scen <- scen %>% dplyr::mutate(kt=ktCO2e/gwp)
  scen <- scen %>% dplyr::arrange(gas)
  scen$gas <- stringr::str_replace(scen$gas,"ch4","CH4_emissions")
  scen$gas <- stringr::str_replace(scen$gas,"co2","ffi_emissions")
  scen$gas <- stringr::str_replace(scen$gas,"n2o","N2O_emissions")
  scen$gas <- stringr::str_replace(scen$gas,"lulucf","luc_emissions")
  names(scen)[1:2] <- c("year","variable")
  scen$units <- NA
  scen <- scen %>% dplyr::mutate(kt=replace(kt,variable=="CH4_emissions",kt/1e+3), units=replace(units,variable=="CH4_emissions","Tg CH4"))
  scen <- scen %>% dplyr::mutate(kt=replace(kt,variable=="N2O_emissions",0.636*kt/(1e+3)), units=replace(units,variable=="N2O_emissions","Tg N"))
  scen <- scen %>% dplyr::mutate(kt=replace(kt,variable=="ffi_emissions",kt/(3.67*1e+6)), units=replace(units,variable=="ffi_emissions","Pg C/yr"))
  scen <- scen %>% dplyr::mutate(kt=replace(kt,variable=="luc_emissions",kt/(3.67*1e+6)), units=replace(units,variable=="luc_emissions","Pg C/yr"))
  names(scen)[5] <- "value"
  scen <- scen %>% dplyr::select(year,variable,value,units)
  #convert to GtC = PgC
  #co2$GtC <- co2$kt/(3.67*1e+6)
  #historical file 0.635
  #n2o$MtN <- n2o$kt/(0.636*1e+3)
  # 1 g N2O => 2*14/(2*14+16) = 0.636
  #Tg -> Mt
  #Pg -> Gt
  rcp_ini <- switch(rcp_path,
                    "rcp26" = system.file("input/hector_rcp26.ini", package = "hector"),
                    "rcp45" = system.file("input/hector_rcp45.ini", package = "hector"),
                    "rcp60" = system.file("input/hector_rcp60.ini", package = "hector") ,
                    "rcp85" = system.file("input/hector_rcp85.ini", package = "hector")
  )

  core <- hector::newcore(rcp_ini)
  core_exirl <- hector::newcore(rcp_ini)
  hector::reset(core)
  hector::setvar(core,NA,hector::ECS(),ecs,unit='degC')
  hector::setvar(core_exirl,NA,hector::ECS(),ecs,unit='degC')
  hector::run(core)
  rcp <- tibble::as_tibble(hector::fetchvars(core, 1745:2100, c(hector::FFI_EMISSIONS(),hector::EMISSIONS_CH4(),hector::EMISSIONS_N2O(),hector::LUC_EMISSIONS()),scenario=rcp_path))

  rcp_exirl <- rcp
  rcp_exirl$scenario <- "rcp_exirl"
  scen <- scen %>% dplyr::rename("irl"=value)
  rcp_exirl <- dplyr::left_join(rcp_exirl,scen)
  rcp_exirl <- rcp_exirl %>% dplyr::mutate(irl=tidyr::replace_na(irl,0))
  rcp_exirl <- rcp_exirl %>% dplyr::mutate(value=value-irl)
  rcp_exirl <- rcp_exirl %>% dplyr::select(-irl)

  f1 <- tibble::as_tibble(hector::fetchvars(core,1745:2100,c(hector::FFI_EMISSIONS(),hector::EMISSIONS_CH4(),hector::EMISSIONS_N2O(),hector::LUC_EMISSIONS(),"Tgav"),scenario=rcp_path))[,-1]
  #f1 %>% filter(variable=="Tgav") %>% ggplot(aes(year,value)) + geom_line()
  hector::setvar(core_exirl,1745:2100,var=hector::FFI_EMISSIONS(), values= dplyr::filter(rcp_exirl, variable=="ffi_emissions")$value, unit = "Pg C/yr")
  hector::setvar(core_exirl,1745:2100,var=hector::EMISSIONS_CH4(), values= dplyr::filter(rcp_exirl, variable=="CH4_emissions")$value, unit = "Tg CH4")
  hector::setvar(core_exirl,1745:2100,var=hector::EMISSIONS_N2O(), values= dplyr::filter(rcp_exirl, variable=="N2O_emissions")$value, unit = "Tg N")
  hector::setvar(core_exirl,1745:2100,var=hector::LUC_EMISSIONS(), values= dplyr::filter(rcp_exirl, variable=="luc_emissions")$value, unit = "Pg C/yr")
  #reset(core26)
  hector::run(core_exirl)
  f1 <- f1 %>% dplyr::rename("global"=value)
  f2 <- tibble::as_tibble(hector::fetchvars(core_exirl,1745:2100,c(hector::FFI_EMISSIONS(),hector::EMISSIONS_CH4(),hector::EMISSIONS_N2O(),hector::LUC_EMISSIONS(),"Tgav"),scenario=paste(rcp_path,"exirl",sep="_")))[,-1]
  f2 <- f2 %>% dplyr::rename("global_exirl"=value)
  f <- dplyr::inner_join(f1,f2)
  f <- f %>% dplyr::mutate(diff=global-global_exirl)
  #f1 %>% filter(year=="2050") %>% print.data.frame()
  #f2 %>% filter(year=="2050") %>% print.data.frame()
  f$scenario <- scenario_name
  f$pathway <- rcp_path
  f$ecs <- ecs
  print(paste("ECS =",hector::fetchvars(core,NA,hector::ECS())$value))
  hector::shutdown(core)
  hector::shutdown(core_exirl)
  return(f)
}


#' scenarioMatrix
#'
#' combined national and 4 global scenarios from AR4
#'
#' @param scenarios scenarios data frame
#' @param ecs_0 climate sensitivity
#'
#' @return data frame : year, variable, global value, units, global value excluding Ireland, national contributin, national scenario, global scenario, ecs
#' @export
#'
#' @examples
#' scenarioMatrix(scenarios,2.9)
#'
scenarioMatrix <- function(scenarios, ecs_0){
  scenario_names <- scenarios$scenario %>% unique()
  d1 <- lapply(scenario_names,function(s) getIRLContrib(s,"rcp26",ecs_0)) %>% dplyr::bind_rows()
  d2 <- lapply(scenario_names,function(s) getIRLContrib(s,"rcp45",ecs_0)) %>% dplyr::bind_rows()
  d3 <- lapply(scenario_names,function(s) getIRLContrib(s,"rcp60",ecs_0)) %>% dplyr::bind_rows()
  d4 <- lapply(scenario_names,function(s) getIRLContrib(s,"rcp85",ecs_0)) %>% dplyr::bind_rows()
#https://advances.sciencemag.org/content/6/26/eaba1981 ecs range 1.8 to 5.6
res <- dplyr::bind_rows(d1,d2,d3,d4)
return(res)
}



