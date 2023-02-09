mp
function (om, oem = NULL, iem = NULL, ctrl, args, scenario = "test", 
          tracking = "missing", verbose = TRUE, cut_hist = TRUE, ...) 
{
  stk.om <- stock(om)
  name(stk.om) <- scenario
  sr.om <- sr(om)
  sr.om.res <- residuals(sr.om)
  sr.om.res.mult <- sr.om@logerror
  projection <- projection(om)
  if (!is.null(args$nblocks)) 
    nblocks <- args$nblocks
  else nblocks <- 1
  fy <- args$fy
  iy <- args$iy
  nsqy <- args$nsqy
  vy <- args$vy <- ac(iy:fy)
  it <- args$it <- dim(stk.om)[6]
  if (is.null(args$data_lag)) 
    args$data_lag <- 1
  if (is.null(args$management_lag)) 
    args$management_lag <- 1
  seed_part <- ifelse(!is.null(args$seed_part), args$seed_part, 
                      FALSE)
  metric <- c("C.obs", "F.est", "B.est", "C.est", "conv.est", 
              "metric.phcr", "metric.hcr", "metric.is", "metric.iem", 
              "metric.fb", "F.om", "B.om", "C.om")
  if (!missing(tracking)) 
    metric <- c(tracking, metric)
  tracking <- FLQuant(NA, dimnames = list(metric = metric, 
                                          year = c(dims(stk.om)$minyear:fy), iter = 1:it))
  catch.inty <- catch(stk.om)[, ac((iy + 1):(iy + args$management_lag))]
  fsq.inty <- c(yearMeans(fbar(stk.om)[, ac((iy - 1):(iy - 
                                                        nsqy))]))
  if (sum(is.na(catch.inty)) == 0) {
    tracking["metric.is", ac((iy - args$management_lag + 
                                1):iy)] <- tracking["metric.iem", ac((iy - args$management_lag + 
                                                                        1):iy)] <- catch.inty
  }
  else {
    tracking["metric.is", ac((iy - args$management_lag + 
                                1):iy)] <- tracking["metric.iem", ac((iy - args$management_lag + 
                                                                        1):iy)] <- 1
  }
  if (!is.null(args$seed)) 
    set.seed(args$seed)
  if (exists(fleetBehaviour(om))) 
    fb <- fleetBehaviour(om)
  else fb <- NULL
  if (is.null(oem)) {
    flqdc <- catch.n(stock(om))
    flqdc[] <- 1
    stkDev <- FLQuants(catch.n = flqdc)
    flqdi <- stock.n(stock(om))
    flqdi[] <- 1
    idxDev <- FLQuants(index.q = flqdi)
    dev <- list(idx = idxDev, stk = stkDev)
    idx <- FLIndex(index = stock.n(stock(om)))
    range(idx)[c("startf", "endf")] <- c(0, 0)
    obs <- list(idx = FLIndices(stkn = idx), stk = stock(om))
    oem <- FLoem(method = perfect.oem, observations = obs, 
                 deviances = dev)
  }
  nblocks <- args$nblocks
  if (nblocks > 1) {
    cat("Going parallel!\n")
    it_blocks <- split(seq(args$it), cut(seq(args$it), breaks = nblocks))
    lst0 <- foreach(j = it_blocks, j_part = seq_along(it_blocks), 
                    .combine = function(...) {
                      list(stk.om = do.call("combine_attr", lapply(list(...), 
                                                                   "[[", "stk.om")), tracking = do.call("combine", 
                                                                                                        lapply(list(...), "[[", "tracking")), oem = do.call("combine", 
                                                                                                                                                            lapply(list(...), "[[", "oem")))
                    }, .multicombine = TRUE, .errorhandling = "stop", 
                    .inorder = TRUE) %dopar% {
                      args$it <- length(j)
                      if (isTRUE(seed_part)) 
                        set.seed(j_part)
                      call0 <- list(stk.om = mse:::iter_attr(stk.om, j), 
                                    sr.om = FLCore::iter(sr.om, j), sr.om.res = sr.om.res[, 
                                                                                          , , , , j], oem = iters(oem, j), tracking = tracking[, 
                                                                                                                                               , , , , j], sr.om.res.mult = sr.om.res.mult, 
                                    fb = fb, projection = projection, iem = iters(iem, 
                                                                                  j), ctrl = iters(ctrl, j), args = args, verbose = verbose)
                      out <- do.call(goFish, call0)
                      list(stk.om = out$stk.om, tracking = out$tracking, 
                           oem = out$oem)
                    }
  }
  else {
    cat("Going single core !\n")
    call0 <- list(stk.om = stk.om, sr.om = sr.om, sr.om.res = sr.om.res, 
                  oem = oem, tracking = tracking, sr.om.res.mult = sr.om.res.mult, 
                  fb = fb, projection = projection, iem = iem, ctrl = ctrl, 
                  args = args, verbose = verbose)
    out <- do.call(goFish, call0)
    lst0 <- list(stk.om = out$stk.om, tracking = out$tracking, 
                 oem = out$oem)
  }
  stk.om <- lst0$stk.om
  tracking <- lst0$tracking
  oem <- lst0$oem
  if (verbose) 
    cat("\n")
  res <- as(om, "FLmse")
  if (isTRUE(cut_hist)) {
    stock(res) <- window(stk.om, start = iy, end = fy)
    tracking(res) <- window(tracking, end = fy - args$management_lag)
  }
  else {
    stock(res) <- stk.om
    tracking(res) <- tracking
  }
  args(res) <- args
  res@oem <- oem
  res@control <- ctrl
  return(res)
}