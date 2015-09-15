calibration_dataset <-
function(x,year,names=c(tcname,acname,fcname)) {
  obj <- NULL
  for (n in names) {
    cal <- demand_data(x,n)
    yy <- paste("y",getYears(cal),sep="")
    if (!year %in% yy) stop(paste("Year",year,"not in calibration data",n))
    obj <- cbind(obj,cal[,match(year,yy)])
  }
  dimnames(obj)[[2]] <- paste(substr(names,1,2),"cal",sep="_")
  obj
}
