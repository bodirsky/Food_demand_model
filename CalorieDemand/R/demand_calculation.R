demand_calculation <-
function(
  scenario_name,
  dat_scen=demand_input, # input dataset
  pop_scen="pop_mio_ssp2",
  gdp_scen="gdp_mioUSD05MER_ssp2", 
  # specifications for regression models
  dem_regr_type=func_gA,
  ls_regr_type=func_hA, 
  # specifications for calibration to historical data
  calib_year_start="y1990",
  calib_year_end="y2100",
  calib_type="convergence",
  
  # specifications for total calory demand
  dem_aim=NULL, 
  dem_aim_type= "s",
  dem_aim_direction= "down",
  dem_aim_startyear="y2010",
  dem_aim_endyear="y2100",
  
  # specifications for livestock share
  ls_aim=NULL, 
  ls_aim_type= "s",
  ls_aim_direction= "up",
  ls_aim_startyear="y2010",
  ls_aim_endyear="y2100"
) {
  
  
  ### readin
  
  ll <- demand_dataset(dat_scen,gdp_scen,dat_scen,pop_scen)
  gdp <- ll$gdp
  pop <- ll$pop
  
  gdp_pc <- gdp/pop
  
  countries <- substr(dimnames(dat_scen)[[1]],1,3)
  years     <- getYears(dat_scen)
  
  #calibration data
  calib0 <- calibration_dataset(dat_scen, calib_year_start, 
                                c("kcal_pc_fao", "lvst_kcal_pc_fao"))
  calib <- array(NA,dim=c(dim(calib0)[1],3),dimnames=list(dimnames(calib0)[[1]],c(dimnames(calib0)[[2]],"ls_cal")))
  calib[,1:2] <- calib0
  calib[,3]   <- calib[,2]/calib[,1]
  
  ###checks
  
  ### dem projections
  # Apply regression on country data 
  
  tc_reg <- dem_regr_type(gdp_pc)
  as_reg <- ls_regr_type(gdp_pc)
  
  # Calibrate country data
  
  tc_calib <- calibrate_it(
    origin=tc_reg, 
    cal_to=calib[,"kc_cal"], 
    cal_type=calib_type,
    cal_year=calib_year_start, 
    end_year=calib_year_end, 
    report_calibration_factors=FALSE)
  
  as_calib <- calibrate_it(
    origin=as_reg, 
    cal_to=calib[,"ls_cal"], 
    cal_type=calib_type,
    cal_year=calib_year_start, 
    end_year=calib_year_end, 
    report_calibration_factors=FALSE)
  
  # dem_aim_convergence
  
  if (!is.null(dem_aim)) {
    kcal <- convergence(
      origin=      tc_calib,
      aim=         dem_aim,
      type=        dem_aim_type,
      start_year=  dem_aim_startyear,
      end_year=    dem_aim_endyear,   
      direction=   dem_aim_direction
    ) 
  } else {
    kcal <- tc_calib
  } 
  
  if (!is.null(ls_aim)) {
    ls <- convergence(
      origin=      as_calib,
      aim=         ls_aim,
      type=        ls_aim_type,
      start_year=  ls_aim_startyear,
      end_year=    ls_aim_endyear,   
      direction=   ls_aim_direction
    ) 
  } else {
    ls <- as_calib
  } 
  
  ### calculate total demand
  
  dem <- kcal * pop
  l   <- kcal * pop * ls  
  
  ### Add History before calibration point 
  
  if (scenario_name=="history"){
    nyears <- 1:length(getYears(dat_scen))
  } else {
    nyears <- 1:(which(dimnames(dat_scen)[[2]]==calib_year_start)-1)
  }
  
  gdp[,nyears]  <- demand_data(dat_scen[,nyears,],"gdp_mioUSD05MER_wb")
  pop[,nyears]  <- demand_data(dat_scen[,nyears,],"pop_mio_wb")
  kcal[,nyears] <- demand_data(dat_scen[,nyears,],"kcal_pc_fao")
  dem[,nyears]  <- kcal[,nyears]*pop[,nyears]
  l[,nyears]    <- demand_data(dat_scen[,nyears,],"lvst_kcal_pc_fao")*pop[,nyears]   
  ls[,nyears]   <- l[,nyears]/dem[,nyears]
  gdp_pc[,nyears] <- gdp[,nyears]/pop[,nyears]
  # waste shr: at least 15% and all above 2200 kcal
  waste_shr <- ((kcal>(2200/0.85))*(kcal-2200)+(kcal<=(2200/0.85))*(kcal*0.15))/kcal
  waste     <- ((kcal>(2200/0.85))*(kcal-2200)+(kcal<=(2200/0.85))*(kcal*0.15))*pop
  
  ### Aggregate to region  
  
  gdp_reg    <- regAggregate(gdp, vectorfunction= function(x) {sum(x, na.rm = FALSE)})      
  pop_reg    <- regAggregate(pop, vectorfunction= function(x) {sum(x, na.rm = FALSE)})                    
  dem_reg    <- regAggregate(dem, vectorfunction= function(x) {sum(x, na.rm = FALSE)})  
  l_reg      <- regAggregate(l,   vectorfunction= function(x) {sum(x, na.rm = TRUE)})      
  waste_reg  <- regAggregate(waste, vectorfunction= function(x) {sum(x, na.rm = TRUE)})  
  
  gdp_pc_reg     <- gdp_reg/pop_reg          
  kcal_reg       <- dem_reg/pop_reg
  ls_reg         <- l_reg/dem_reg
  waste_shr_reg  <- waste_reg/dem_reg
  
  ### change units
  # kcal per day --> PJ per year
  #      1       --> 4.184 * 365 / 1000000
  CalJoule <- 4.184*365*1e-6
  dem       <- CalJoule * dem
  l         <- CalJoule * l 
  waste     <- CalJoule * waste 
  dem_reg   <- CalJoule * dem_reg 
  l_reg     <- CalJoule * l_reg 
  waste_reg <- CalJoule * waste_reg 
  
  ### prepare output
  
  vars <- c("dem","ls","pop","gdp","gdp_pc","kcal","l"
#,"waste","waste_shr"
  )
  country <- array(NA,dim=c(dim(dem),length(vars)),
                   dimnames=c(dimnames(dem),list(vars)))
  country[,,1] <- dem
  country[,,2] <- ls
  country[,,3] <- pop
  country[,,4] <- gdp
  country[,,5] <- gdp_pc
  country[,,6] <- kcal
  country[,,7] <- l
#  country[,,8] <- waste
#  country[,,9] <- waste_shr
  
  varreg <- vars
  reg <- array(NA,dim=c(dim(dem_reg),length(varreg)),
               dimnames=c(dimnames(dem_reg),list(varreg)))
  reg[,,1] <- dem_reg
  reg[,,2] <- ls_reg
  reg[,,3] <- pop_reg
  reg[,,4] <- gdp_reg
  reg[,,5] <- gdp_pc_reg
  reg[,,6] <- kcal_reg
  reg[,,7] <- l_reg
#  reg[,,8] <- waste_reg
#  reg[,,9] <- waste_shr_reg
  
  out <- list(reg=reg,country=country)
  
  return(out)
}
