demand_data <-
function(x,dname){ # data-specific function for creating demand objects
  if (!dname %in% dimnames(x)[[3]]) stop(paste("wrong data specifier",dname))
  dat       <- as.array(x[,,dname])
  countries <- substr(dimnames(dat)[[1]],1,3)
  years     <- getYears(dat)
  obj       <- demand_object(dat,countries,years) 
  obj
}
