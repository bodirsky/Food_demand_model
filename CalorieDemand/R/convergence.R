convergence <-
function(origin, aim, start_year = NULL, end_year = NULL, direction = NULL, 
                        type = "smooth") {
  if (is.null(dim(aim))) aim <- array(aim,dim(origin),dimnames(origin))
  if (is.null(start_year)) stop("startyear needed")
  if (is.null(end_year))   stop("endyear needed")
  if (!is.numeric(start_year)) start_year <- as.numeric(substr(start_year, 2, 5))
  if (!is.numeric(end_year))   end_year <- as.numeric(substr(end_year, 2, 5))
  if (!is.null(direction)) {
    aim <- as.array(aim)
    if (direction == "up") aim[which(aim < origin)] <- as.array(origin)[which(aim < origin)]
    else if (direction == "down") 
      aim[which(aim > origin)] <- as.array(origin)[which(aim > origin)]
    else stop("Illegal direction setting, only up and down are allowed arguments!")
  }
  years <- matrix(rep(getYears(origin),dim(origin)[1]),dim(origin),byrow=T)
  years <- as.array(years,dim=dim(origin),dimnames=dimnames(origin))
  #names(years) <- dimnames(origin)[[2]]
  pos <- (years - start_year)/(end_year - start_year)
  pos[pos < 0] <- 0
  pos[pos > 1] <- 1
  mix <- switch(type,
                "linear" = pos,
                "s" =  pos^4/(0.07 + pos^4) * 1.07,
                "smooth" = pos^3/(0.1 + pos^3),
                stop("convergence type does not exist")
  )
  converged <- aim * mix + origin * (1 - mix)
  return(converged)
}
