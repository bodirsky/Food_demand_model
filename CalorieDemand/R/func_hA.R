func_hA <-
function(x,p=c(pa = -36.732779363,pb = 4.497483702,
                          pc = 0.016039027,pd = -0.002077227)) {
  if (length(p) != 4) stop(paste("Parameter vector of length 4 needed, length",length(p)))
  yy <- getYears(x)
  t  <- as.array(matrix(yy,nrow=dim(x)[1],ncol=length(yy),byrow=T))
  exp(p[["pa"]] + p[["pb"]]*log(x) + p[["pc"]]*t + p[["pd"]]*log(x)*t)
}
