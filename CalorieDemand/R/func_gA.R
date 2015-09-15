func_gA <-
function(x,p=c(pa1 = 2.8251159,pa2 = 2.1313756e-3,
                          pb1 = 0.1622219,pb2 = -3.12439e-5)) {
  if (length(p) != 4) stop(paste("Parameter vector of length 4 needed, length",length(p)))
  yy <- getYears(x)
  t  <- as.array(matrix(yy,nrow=dim(x)[1],ncol=length(yy),byrow=T))
  exp(p[["pa1"]]+p[["pa2"]]*t)*x^{p[["pb1"]]+p[["pb2"]]*t}
}
