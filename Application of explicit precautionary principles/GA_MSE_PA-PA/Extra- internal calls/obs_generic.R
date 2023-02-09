
idx_dev = TRUE; ssb = FALSE;
lngth = TRUE; lngth_dev = TRUE;
lngth_par = pars_l;
PA_status = FALSE; PA_status_dev = FALSE;
PA_Bmsy = c(refpts(brps[[stock]])["msy", "ssb"]) ;
PA_Fmsy = c(refpts(brps[[stock]])["msy", "harvest"])



  
  #ay <- args$
  ### update observations
  observations$stk <- stk
  ### use SSB as index?
  if (isTRUE(ssb_idx)) {
    observations$idx$idxB <- ssb(observations$stk)
    ### TSB?
  } else  if (isTRUE(tsb_idx)) {
    observations$idx$idxB <- tsb(observations$stk)
    ### otherwise calculate biomass index
  } else {
    observations$idx$idxB <- quantSums(stk@stock.n * stk@stock.wt * 
                                         observations$idx$sel)
  }
  ### use mean length in catch?
  if (isTRUE(lngth)) {
    observations$idx$idxL <- lmean(stk = stk, params = lngth_par)
  }
  ### stock status for PA buffer?
  if (isTRUE(PA_status)) {
    observations$idx$PA_status[] <- ssb(observations$stk) > 0.5*PA_Bmsy & 
      fbar(observations$stk) < PA_Fmsy
  }
  
  ### observation model
  stk0 <- observations$stk
  idx0 <- observations$idx
  ### add deviances to index?
  if (isTRUE(idx_dev)) {
    if (isTRUE(ssb_idx) | isTRUE(tsb_idx)) {
      idx0$idxB <- observations$idx$idxB * deviances$idx$idxB
    } else {
      idx0$idxB <- quantSums(stk@stock.n * stk@stock.wt * 
                               observations$idx$sel * deviances$idx$sel)
      if (isTRUE("idxB" %in% names(deviances$idx)) & 
          all.equal(dim(deviances$idx$idxB), dim(idx0$idxB)))
        idx0$idxB <- idx0$idxB * deviances$idx$idxB
    }
  }
  ### uncertainty for catch length
  if (isTRUE(lngth) & isTRUE(lngth_dev)) {
    idx0$idxL <- observations$idx$idxL * deviances$idx$idxL
  }
  ### uncertainty for stock status for PA buffer
  if (isTRUE(PA_status) & isTRUE(PA_status_dev)) {
    idx0$PA_status <- ifelse(observations$idx$PA_status == TRUE, 
                             deviances$idx$PA_status["positive", ],
                             deviances$idx$PA_status["negative", ])
  }
  
  
  return(list(stk = stk0, idx = idx0, observations = observations,
              tracking = tracking))