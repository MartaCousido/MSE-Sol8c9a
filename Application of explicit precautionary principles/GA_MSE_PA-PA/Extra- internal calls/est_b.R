
function(idx, ay, 
         I_trigger, idxB_lag, idxB_range_3,
         ...) {
  
  ### if fewer iterations provided expand
  if (isTRUE(length(I_trigger) < dims(idx)$iter)) {
    I_trigger <- rep(I_trigger, dims(idx)$iter)
    ### if more iterations provided, subset
  } else if (isTRUE(length(I_trigger) > dims(idx)$iter)) {
    I_trigger <- I_trigger[an(dimnames(idx)$iter)]
  }
  
  ### calculate index mean
  idx_yrs <- seq(to = ay - idxB_lag, length.out = idxB_range_3)
  idx_mean <- yearMeans(idx[, ac(idx_yrs)])
  ### ratio
  idx_ratio <- c(idx_mean / I_trigger)
  ### b is 1 or smaller
  idx_ratio <- ifelse(idx_ratio < 1, idx_ratio, 1)
  
  return(idx_ratio)
  
}