
  
  ### if fewer iterations provided expand
  if (isTRUE(length(Lref) < dims(idx)$iter)) {
    Lref <- rep(Lref, dims(idx)$iter)
    ### if more iterations provided, subset
  } else if (isTRUE(length(Lref) > dims(idx)$iter)) {
    Lref <- Lref[an(dimnames(idx)$iter)]
  }
  
  ### get mean length in catch
  idx_yrs <- seq(to = ay - idxL_range, length.out = idxL_lag)
  idx_mean <- yearMeans(idx[, ac(idx_yrs)])
  ### length relative to reference
  idx_ratio <- c(idx_mean / Lref)
  ### avoid negative values
  idx_ratio <- ifelse(idx_ratio > 0, idx_ratio, 0)
  ### avoid NAs, happens if catch = 0
  idx_ratio <- ifelse(is.na(idx_ratio), 1, idx_ratio)
