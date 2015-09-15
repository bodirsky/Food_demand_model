demand_object <-
function(x,country,years) { # x has to be a twodimensional array 
  if (!is.array(x))                 stop("data are not an array")
  #if (length(dim(x)) != 2)          stop("data are not two-dimensional")
  if (length(country) != dim(x)[1]) stop("data rows do not equal number of countries")
  if (length(years) != dim(x)[2])   stop("data columns do not equal number of years")
  dimnames(x)[[1]] <- paste(country,1:length(country),sep=".")
  dimnames(x)[[2]] <- paste("y",years,sep="")
  x
}
