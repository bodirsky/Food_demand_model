demand_dataset <-
function(xgdp,gdpname,xpop,popname) {
  gdp <- demand_data(xgdp,gdpname)
  pop <- demand_data(xpop,popname)
  if (max(dim(gdp)-dim(pop)) > 0) stop(paste("dimensions of gdp data",dim(gdp),
                                             "do not match dimensions of population data",dim(pop)))
  list(gdp=gdp,pop=pop)
}
