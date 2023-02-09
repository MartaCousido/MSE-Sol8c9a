idx = idx$idxB; ay = ay;
idxB_lag = idxB_lag; idxB_range_1 = idxB_range_1; 
idxB_range_2 = idxB_range_2

yrs_a <- seq(to = c(ay - idxB_lag), length.out = idxB_range_1)
yrs_b <- seq(to = min(yrs_a) - 1, length.out = idxB_range_2)
idx_a <- yearMeans(idx[, ac(yrs_a)])
idx_b <- yearMeans(idx[, ac(yrs_b)])
idx_ratio <- c(idx_a / idx_b)

return(idx_ratio)