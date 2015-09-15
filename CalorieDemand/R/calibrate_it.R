calibrate_it <-
function(origin, cal_to, cal_type = "convergence", cal_year = NULL, 
          end_year = NULL, report_calibration_factors = FALSE) 
{
  if (cal_type == "convergence" & (is.null(cal_year) | is.null(end_year))) 
    stop("for convergence, cal_year and end_year is required")
  if (dim(origin)[1] != length(cal_to)) 
    stop("number of countries of origin and cal_to do not match")
  if (!is.null(names(cal_to)) & (!identical(dimnames(origin)[[1]],names(cal_to)))) 
    stop("names of cal_to has to be identical with origin or NULL")
  calibration_factor <- array(1, dim(origin), dimnames(origin))
  calibrated         <- array(NA, dim(origin), dimnames(origin))
  calibrated <- switch(cal_type,
    "none" = {if (report_calibration_factors == TRUE) print(1)
              origin},
    "convergence" = {
      calibration_factor[, ] <- cal_to/origin[, cal_year]
      calibration_factor <- convergence(origin = calibration_factor, 
                                      aim = 1, start_year = cal_year, 
                                      end_year = end_year, 
                                      direction = NULL, type = "linear")
      if (report_calibration_factors == TRUE) print(calibration_factor)
      origin * calibration_factor},
    "growth_rate" = {
      cal_origin <- matrix(rep(origin[,cal_year],dim(origin)[2]),dim(origin),byrow=F)
      cal_origin <- array(cal_origin,dim=dim(origin),dimnames=dimnames(origin))
      if (report_calibration_factors == TRUE) print(cal_origin * cal_to)
      origin/cal_origin * cal_to},
    stop("unknown cal_type")
  )
  return(calibrated)
}
