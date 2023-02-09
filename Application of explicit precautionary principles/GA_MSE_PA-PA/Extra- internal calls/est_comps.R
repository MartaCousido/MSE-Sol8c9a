est_comps
function(stk, idx, tracking, args,
         comp_r = FALSE; comp_f = FALSE; comp_b = FALSE;
         comp_i = FALSE; comp_c = TRUE; comp_m = FALSE;
         idxB_lag = 1; idxB_range_1 = 2; idxB_range_2 = 3;
         idxB_range_3 = 1;
         catch_lag = 1; catch_range = 1;
         Lref; I_trigger;
         idxL_lag = 1; idxL_range = 1;
         pa_buffer = FALSE; pa_size = 0.8; pa_duration = 3;
         Bmsy = NA;
         ...) {
  
  ay <- args$ay
  
  ### component r: index trend
  if (isTRUE(comp_r)) {
    r_res <- est_r(idx = idx$idxB, ay = ay,
                   idxB_lag = idxB_lag, idxB_range_1 = idxB_range_1, 
                   idxB_range_2 = idxB_range_2)
  } else {
    r_res <- 1
  }
  tracking["comp_r", ac(ay)] <- r_res
  
  ### component f: length data
  if (isTRUE(comp_f)) {
    f_res <- est_f(idx = idx$idxL, ay = ay,
                   Lref = Lref, idxL_range = idxL_range, idxL_lag = idxL_lag)
  } else {
    f_res <- 1
  }
  tracking["comp_f", ac(ay)] <- f_res
  
  ### component b: biomass safeguard
  if (isTRUE(comp_b)) {
    b_res <- est_b(idx = idx$idxB, ay = ay,
                   I_trigger = I_trigger, idxB_lag = idxB_lag, 
                   idxB_range_3 = idxB_range_3)
  } else {
    b_res <- 1
  }
  
  ### PA buffer
  if (isTRUE(pa_buffer)) {
    b_res <- est_pa(idx = idx$PA_status, ay = ay, 
                    tracking = tracking, idxB_lag = idxB_lag,
                    pa_size = pa_size, pa_duration = pa_duration)
  }
  tracking["comp_b", ac(ay)] <- b_res
  
  ### component i: index value
  if (isTRUE(comp_i)) {
    i_res <- est_i(idx = idx$idxB, ay = ay,
                   idxB_lag = idxB_lag, idxB_range_3 = idxB_range_3)
  } else {
    i_res <- 1
  }
  tracking["comp_i", ac(ay)] <- i_res
  
  ### current catch
  if (isTRUE(comp_c)) {
    c_res <- est_c(ay = ay, catch = catch(stk), catch_lag = catch_lag, 
                   catch_range = catch_range)
  } else {
    c_res <- 1
  }
  tracking["comp_c", ac(ay)] <- c_res
  
  ### component m: multiplier
  if (!isFALSE(comp_m)) {
    m_res <- comp_m
    ### subset to iteration when simultion is split into blocks
    if (isTRUE(length(comp_m) > dims(stk)$iter)) {
      m_res <- comp_m[as.numeric(dimnames(stk)$iter)]
    }
  } else {
    m_res <- 1
  }
  tracking["multiplier", ac(ay)] <- m_res
  
  return(list(stk = stk, tracking = tracking))
  
}
> 