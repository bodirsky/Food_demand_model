func_hB <-
function(x,p=c(pa1=1.371507e-2,pa2=-5.295249e-6,
                          pb1=-1.102410e-4,pb2=6.403996e-8)) {
  if (length(p) != 4) stop(paste("Parameter vector of length 4 needed, length",length(p)))
  yy <- getYears(x)
  t  <- as.array(matrix(yy,nrow=dim(x)[1],ncol=length(yy),byrow=T))
  (p[["pa1"]] + p[["pa2"]]*t)*(x^(0.5))*exp(-(p[["pb1"]] + p[["pb2"]]*t) * x)
}
