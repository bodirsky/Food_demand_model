regAggregate <-
function (data, vectorfunction = function(x) {sum(x, na.rm = TRUE)}, query=iso_reg, ...) {
  region <- as.character(query[match(substr(dimnames(data)[[1]],1,3),query[,"iso"]),"reg"])
  dimnames(data)[[1]] <- region
  #data <- rename_dimnames(data = data, query = query, dim = dim, 
  #                        from = from, to = to)
  groups <- sort(unique(region))
  out <- array(NA, dim = c(length(groups), dim(data)[[2]]), dimnames = list(groups, dimnames(data)[[2]]))
  for (element_x in groups) {
    part_x <- data[which(region==element_x),]
    out[which(groups == element_x), ] <- apply(part_x, MARGIN = c(2), FUN = (vectorfunction))
  }
  return(out)
}
