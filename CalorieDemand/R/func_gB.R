func_gB <-
function(x,p=c(pa1=387.473708, pa2=9.774726,    pa3=933.888522,
                          pb1=0.008445119,pb2=-0.755692561,pb3=0.08940805)) {
  if (length(p) != 6) stop(paste("Parameter vector of length 6 needed, length",length(p)))
  yy <- getYears(x)
  t  <- as.array(matrix(yy,nrow=dim(x)[1],ncol=length(yy),byrow=T))
  f1 <- (p[["pa3"]]+(p[["pa1"]]*t)/(t+p[["pa2"]]))
  f2 <- (p[["pb3"]]+(p[["pb1"]]*t)/(t+p[["pb2"]]))
  f1 * x^{f2}
}
