func_g0 <-
function(x,p=c(pa = 7.074079,pb = 0.099321)) {
  if (length(p) != 2) stop(paste("Parameter vector of length 2 needed, length",length(p)))
  exp(p[["pa"]])*x^{p[["pb"]]}
}
