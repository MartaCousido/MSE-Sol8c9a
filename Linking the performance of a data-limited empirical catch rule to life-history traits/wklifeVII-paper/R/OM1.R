### ------------------------------------------------------------------------ ###
### Create biological stocks from life-history parameters                    ###
### Code for article: Linking the performance of a data-limited empirical catch
### rule to life-history traits
### Maria Grazia Pennino.
### Marta Cousido Rocha.
### Last modification: 16/01/2023
### ------------------------------------------------------------------------ ###



### Set directory (your directory must be wklifeVII-paper)
setwd(paste(getwd(),"/R",sep=""))

### scenarios (OM sets) to run
#scns <- 29
scns <- as.numeric(commandArgs(TRUE))

### load packages
library(FLife)
library(FLBRP)
library(FLasher)
library(foreach)
library(doParallel)

### set up cluster for parallel computing
cl <- makeCluster(parallel::detectCores()/2)
registerDoParallel(cl)

### load additional functions
source("MP_functions.R")

### ------------------------------------------------------------------------ ###
### load data ####
### ------------------------------------------------------------------------ ###

### extended list
stocks_lh <- read.csv("input/stock_solea.csv")
### subset to non-NA stocks
stocks_lh <- stocks_lh[!is.na(stocks_lh$a), ]
names(stocks_lh)[1] <- "name"

### use lmax as proxy for linf, if linf not provided but lmax exits
pos_lmax <- which(!is.na(stocks_lh$lmax) & is.na(stocks_lh$linf))
stocks_lh$linf[pos_lmax] <- stocks_lh$lmax[pos_lmax]

### set steepness to 0.75
stocks_lh$s <- 0.75

### change name for stocks that appear twice
stocks_lh$stock <- as.character(stocks_lh$stock)
if (nrow(stocks_lh) > 15) {
  stocks_lh$stock[29] <- paste0(stocks_lh$stock[29], "_2")
}

### ------------------------------------------------------------------------ ###
### create final brps ####
### ------------------------------------------------------------------------ ###

### set fbar range
### only used for default OMs
stocks_lh$minfbar <- 1
stocks_lh$maxfbar <- 3

### OM scenarios (M, etc.)
### stored in csv file, load here
OM_scns <- read.csv("input/OM_scns.csv", stringsAsFactors = FALSE)

### create FLBRP objects from life-history data
brps <- foreach(OM_scn = split(OM_scns, 1:nrow(OM_scns)),
                .final = function(x) {
                  names(x) <- OM_scns$id
                  return(x)
                }) %:%
  foreach(i = split(stocks_lh, 1:nrow(stocks_lh)),
          .errorhandling = "pass", .packages = c("FLife", "FLBRP"),
          .final = function(x) {
            names(x) <- stocks_lh$stock
            return(x)
          }) %dopar% {
              return(OM_scn)
            }
            #return(i)
             # }
            # The i data file will contains for each scenario of OMs the information of
            # the 29 stocks. For all the scenarios the information of the stocks is repeated.
            # The foreach with     %:% and dopar works as a loop with i and j where i
            # refers to scenarios and j to stocks and for that in each iter we have a particular
            # scenario and stock.
            # Example of selection:
            OM_scn=brps
            OM_scn=OM_scn[[1]]$sol8c9a
            i=brps         
            i=i[[1]]$sol8c9a
            
            
  ### create brp
  ### get lh params
  lh_res <- c(dimnames(lhPar(FLPar(linf = 1)))$params, "l50")
  lh_avail <- intersect(lh_res, names(i))
  lh_pars <- i[, lh_avail]
  lh_pars <- lh_pars[, !is.na(lh_pars)]
  lh_pars <- as(lh_pars, "FLPar")
  ### set default t0, if missing
  if (!"t0" %in% dimnames(lh_pars)$params) {
    lh_pars <- rbind(lh_pars, FLPar(t0 = -0.1))
  }
  ### calculate l50, if missing and a50 exists
  if (!"l50" %in% dimnames(lh_pars)$params & 
      all(c("a50", "k", "linf", "t0") %in% dimnames(lh_pars)$params)) {
    lh_pars <- rbind(lh_pars, 
                     FLPar(l50 = vonB(age = c(lh_pars["a50"]), 
                                      params = lh_pars)))
  }
  ### handle steepness
  if (!is.na(OM_scn$steepness)) {
    if (!is.na(as.numeric(OM_scn$steepness))) {
      lh_pars["s"] <- as.numeric(OM_scn$steepness)
    } else if (OM_scn$steepness == "l50linf") {
      l50linf <- lh_pars["l50"] / lh_pars["linf"]
      ### calculate steepness according to Wiff et al. 2018
      x <- l50linf
      y <- 2.706 - 3.698*x
      invLogit <- function(y) (0.2 + exp(y))/(1 + exp(y))
      lh_pars["s"] <- invLogit(y)
    } else if (OM_scn$steepness == "k") {
      ### h dependent on k...
      dat <- data.frame(ks = c(min(stocks_lh$k), max(stocks_lh$k)),
                        hs = c(0.5, 0.9))
      model <- lm(hs ~ ks, data = dat)
      new_s <- predict(object = model, new = data.frame(ks = c(lh_pars["k"])))
      lh_pars["s"] <- new_s
    } else if (OM_scn$steepness == "Myers") {
      ### h from Myers et al. 1999
      lh_pars["s"] <- ifelse(!is.na(i$h_Myers), i$h_Myers, c(lh_pars["s"]))
    }
  }

  ### create missing pars
  lh_pars <- lhPar(lh_pars)
  
  ### Max age: age at l = 0.95 * linf
  max_age <- ceiling(log(0.05)/(-c(lh_pars["k"])) + c(lh_pars["t0"]))
  ### max from F
  ### alternative for Lorenzen mortality
  if (OM_scn$m == "lorenzen") {
    ### age with 5% survival
    max_ageP <- ceiling(-log(0.05)/(OM_scn$M1))
    if (max_age < max_ageP) max_age <- max_ageP
  }
  
  ### set up M
  ### either return function or vector with values
  m_def <- if (OM_scn$m == "gislason") {
    ### hardcode Gislason natural mortality function here
    ### used to be default in FLife
    function(length,params) {
      exp(0.55)*(length^-1.61) %*% (params["linf"]^1.44) %*% params["k"]
    }
  } else if (OM_scn$m == "lorenzen") {
    
      ages <- 1:max_age
      M1 <- OM_scn$M1
      M2 <- OM_scn$M2
      age_M1 <- 1
      age_M2 <- 20
      
    a <- exp(log((vonB(age_M2, lh_pars))^log(M2) / 
                   (vonB(age_M1, lh_pars))^log(M1)) / 
      log((vonB(age_M2, lh_pars)) / (vonB(age_M1, lh_pars))))
    b <- log(M1 / M2) /
      log( (vonB(age_M2, lh_pars)) / (vonB(age_M1, lh_pars)))
    a * vonB(ages, lh_pars)^b
    
  } else if (is.numeric(as.numeric(as.character(OM_scn$m)))) {
    as.numeric(as.character(OM_scn$m))
  }
  
  ### set-up selectivity (if specified)
  if (!is.na(OM_scn$selectivity)) {
    if (isTRUE(OM_scn$selectivity == "before")) {
      ### move maturity curve to the left 
      ### 2x the difference between a50 and a95
      sel_def <- function(age, params) {
        ### mimic age definition as used in maturity function
        new_age <- age - 0.5 + c(params["a50"] - floor(params["a50"]) +
                                   2 * params["ato95"])
        sel. <- FLife::logistic(new_age, params)
        return(sel.)
      }
    } else if (isTRUE(OM_scn$selectivity == "maturity")) {
      sel_def <- function(age, params) {
        ### mimic age definition as used in maturity function
        new_age <- age - 0.5 + c(params["a50"] - floor(params["a50"]))
        sel. <- FLife::logistic(new_age, params)
        return(sel.)
      }
    } else if (isTRUE(OM_scn$selectivity == "after")) {
      ### move maturity curve to the right 
      ### 2x the difference between a50 and a95
      sel_def <- function(age, params) {
        ### mimic age definition as used in maturity function
        new_age <- age - 0.5 + c(params["a50"] - floor(params["a50"]) -
                                   2 * params["ato95"])
        sel. <- FLife::logistic(new_age, params)
        return(sel.)
      }
    } else if (isTRUE(grepl(x = OM_scn$selectivity, pattern = "shift_"))) {
      ### shift selectivity curve by ages:
      shift <- an(strsplit(x = OM_scn$selectivity, split = "_")[[1]][2])
      sel_def <- function(age, params) {
        age_new <- age - shift
        sel. <- FLife::dnormal(age = age_new, params = params)
        return(sel.)
      }
    }
  } else {
    sel_def <- FLife::dnormal
  }
  
  ### fbar range: 
  ### for default OMs: use manual definition 
  ### for modified OMs: use age at full selection
  if (isTRUE(!OM_scn$idSEQ %in% c(2:16))) {
    fbar_age <- c(i$minfbar, i$maxfbar)
  } else {
    fbar_age <- rep(c(round(lh_pars["a1"])), 2)
  }
    
  ### create brp
  brp <- lhEql(lh_pars, range = c(min = 1, max = max_age, 
                                  minfbar = fbar_age[1], maxfbar = fbar_age[2], 
                                  plusgroup = max_age), 
               m = m_def, sel = sel_def
               )
  
  ### save life-history parameters in FLBRP
  attr(brp, "lhpar") <- lh_pars
  attr(brp, "OM_scn") <- OM_scn
  attr(brp, "lhpar_calc") <- FLPar(
    M = mean(m(brp)),
    M_mat = weighted.mean(x = m(brp), w = mat(brp)),
    MK = weighted.mean(x = m(brp), w = mat(brp)) / c(lh_pars["k"]),
    amax = dims(brp)$plusgroup
  )
  
  name(brp) <- i$stock
  desc(brp) <- i$stock
  
  return(brp)

}
#names(brps) <- stocks_lh$stock

### save brps
saveRDS(brps, file = "input/brps_paper.rds")
#brps <- readRDS("input/brps_paper.rds")
#saveRDS(brps[[1]], file = "input/brps.rds")
# brps <- readRDS(file = "input/brps_wklife8.rds")
# brps <- readRDS(file = "input/brps_paper.rds")
# brps$I_trigger <- brps[[1]]

### calculate M
# stocks_lh$M <- sapply(brps[[1]], function(x){
#  mean(m(x))
# })
# ### mature M (only mature proportion of stock considered)
# stocks_lh$M_mat <- sapply(brps[[1]], function(x){
#   weighted.mean(x = m(x), w = mat(x))
# })
# 
# ### M/K
# stocks_lh$MK <- with(stocks_lh, M_mat / k)
# 
# ### max age
# stocks_lh$amax <- sapply(brps[[1]], function(x) dims(x)$plusgroup)

### get reference points
# ref_pts <- lapply(brps[[1]], refpts)
# names(ref_pts) <- stocks_lh$stock
# saveRDS(ref_pts, "input/refpts.rds")

### save stock list
# write.csv(file = "input/stock_list_full2.csv", x = stocks_lh)

### get full list of lhpars, including calculated ones
# lhpar_calc <- lapply(brps[[1]], function(x) x@lhpar)
# as.data.frame(lhpar_calc)

### extract and save refpts
refpts <- lapply(brps, function(x) {
  lapply(x, refpts)
})
### refpts
refpts_tmp <- lapply(brps, function(x) lapply(x, refpts))
saveRDS(refpts_tmp, file = "input/refpts_paper.rds")
#saveRDS(refpts_tmp[[1]], file = "input/refpts.rds")

### ------------------------------------------------------------------------ ###
### create FLStocks ####
### ------------------------------------------------------------------------ ###
### This code is needed if we combine both github repositories?
### number of iterations
#its <- 500

OMs <- foreach(OM_scn = split(OM_scns, 1:nrow(OM_scns))[scns],
               brps_i = brps[scns],
               .final = function(x) {
                 names(x) <- OM_scns$id[scns]
                 return(x)
               }) %:% 
  foreach(i = brps_i, .errorhandling = "stop", 
          .packages = c("FLife", "FLasher", "FLBRP")) %dopar% {
  
  #### coerce FLBRP into FLStock
  ### keep only second year (first year with non zero catch)
  stk <- as(i, "FLStock")[, 2]
  ### name first year "1"
  stk <- qapply(stk, function(x) {
    dimnames(x)$year <- "1"; return(x)
  })
  
  ### extend object to year 100
  stk <- fwdWindow(stk, i, end = 100)
  
  ### propagate with requested number of iterations
  stk <- propagate(stk, OM_scn$n_iter)
  
  ### create FLSR object
  stk_sr <- FLSR(params = params(i), model = model(i))
  
  ### create residuals for (historical) projection
  set.seed(0)
  residuals <- rlnoise(OM_scn$n_iter, rec(stk) %=% 0, 
                       sd = OM_scn$rec_sd, b = OM_scn$rec_rho)
  
  ### project forward, targeting 0.5 F_MSY
  years_target <- ((dims(stk)$minyear + 1):75)
  stk <- fwd(stk, sr = stk_sr, 
             control = fwdControl(year = years_target, 
                                  value = refpts(i)['msy', 'harvest']*0.5, 
                                  quant = "f"),
             residuals = residuals[, ac(years_target)])
  
  ### project last 25 years with 2 scenarios: one-way & roller-coaster
  years_target <- 76:100
  ### effort limit
  effort_limit <- ifelse(grepl(x = OM_scn$selectivity, "shift_"), FALSE, TRUE)
  ### one-way
  stk_one_way <- oneWayTrip(stk, sr = stk_sr, brp = i, years = years_target,
                            residuals = residuals[, ac(years_target)],
                            f0 = refpts(i)["msy", "harvest"]*0.5,
                            effort_limit = effort_limit)
  
  ### roller-coaster
  stk_roller_coaster <- rollerCoaster(stk, sr = stk_sr, brp = i, 
                                      up = 0.2, down = 0.2, 
                                      years = years_target,
                                      residuals = residuals[, ac(years_target)],
                                      f0 = refpts(i)["msy", "harvest"]*0.5,
                                      effort_limit = effort_limit)
  
  ### extract life-history parameters
  lhpar <- attr(i, "lhpar")
  attr(i, "lhpar") <- NULL
  
  ### set name of stock
  name(stk_one_way) <- name(i)
  name(stk_roller_coaster) <- name(i)
  name(stk_sr) <- name(i)
  ### description
  desc(stk_one_way) <- paste(name(i), "one-way")
  desc(stk_roller_coaster) <- paste(name(i), "roller-coaster")
  desc(stk_sr) <- name(i)

  ### save
  path_i <- paste0("input/OM1/", OM_scn$id, "/")
  ### one-way
  dir.create(paste0(path_i, "one-way"), recursive = TRUE)
  saveRDS(list(sr = stk_sr, brp = i, lhpar = lhpar, 
               stk = window(stk_one_way, start = 75)), 
          file = paste0(path_i, "one-way/", name(i), ".rds"))
  ### roller-coaster
  dir.create(paste0(path_i, "roller-coaster"), recursive = TRUE)
  saveRDS(list(sr = stk_sr, brp = i, lhpar = lhpar, 
               stk = window(stk_roller_coaster, start = 75)), 
          file = paste0(path_i, "roller-coaster/", name(i), ".rds"))

  # ### return list
  # return(list(sr = stk_sr, brp = i, lhpar = lhpar, 
  #             stk_one_way = stk_one_way, 
  #             stk_roller_coaster = stk_roller_coaster))
  
}

stopCluster(cl)

