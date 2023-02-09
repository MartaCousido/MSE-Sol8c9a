is_comps
function(ctrl, args, tracking, interval = 2, 
         upper_constraint = Inf, lower_constraint = 0, 
         cap_below_b = TRUE, ...) {
  
  ay <- args$ay ### current year
  iy <- args$iy ### first simulation year
  
  advice <- ctrl@trgtArray[ac(ay + args$management_lag), "val", ]
  
  ### check if new advice requested
  if ((ay - iy) %% interval == 0) {
    
    ### apply TAC constraint, if requested
    if (!is.infinite(upper_constraint) | lower_constraint != 0) {
      
      ### get last advice
      if (isTRUE(ay == iy)) {
        ### use OM value in first year of projection
        adv_last <- tracking["C.om", ac(iy)]
      } else {
        adv_last <- tracking["metric.is", ac(ay - 1)]
      }
      ### ratio of new advice/last advice
      adv_ratio <- advice/adv_last
      
      ### upper constraint
      if (!is.infinite(upper_constraint)) {
        ### find positions
        pos_upper <- which(adv_ratio > upper_constraint)
        ### turn of constraint when index below Itrigger?
        if (isFALSE(cap_below_b)) {
          pos_upper <- setdiff(pos_upper, 
                               which(c(tracking[, ac(ay)]["comp_b", ]) < 1))
        }
        ### limit advice
        if (length(pos_upper) > 0) {
          advice[pos_upper] <- adv_last[,,,,, pos_upper] * upper_constraint
        }
        ### lower constraint
      }
      if (lower_constraint != 0) {
        ### find positions
        pos_lower <- which(adv_ratio < lower_constraint)
        ### turn of constraint when index below Itrigger?
        if (isFALSE(cap_below_b)) {
          pos_lower <- setdiff(pos_lower, 
                               which(c(tracking[, ac(ay)]["comp_b", ]) < 1))
        }
        ### limit advice
        if (length(pos_lower) > 0) {
          advice[pos_lower] <- adv_last[,,,,, pos_lower] * lower_constraint
        }
      }
    }
    
    ### otherwise do nothing here and recycle last year's advice
  } else {
    
    advice <- tracking["metric.is", ac(ay - 1)]
    
  }
  ctrl@trgtArray[ac(ay + args$management_lag),"val",] <- advice
  
  return(list(ctrl = ctrl, tracking = tracking))
  
}