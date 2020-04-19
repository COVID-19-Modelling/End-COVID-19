library(rstan)


get_absorb_data <- function(nc) {
  nh <- nc$I
  nhm2 <- rev(rev(nh)[-(1:2)])
  nhm0 <- nh[-(1:2)]
  
  nd <- nc$D
  ndm2 <- rev(rev(nd)[-(1:2)])
  ndm0 <- nd[-(1:2)]
  
  nr <- nc$R
  nrm2 <- rev(rev(nr)[-(1:2)])
  nrm0 <- nr[-(1:2)]
  
  list(
    n_t = length(nh) - 2,
    dh = nhm0 - nhm2,
    dd = ndm0 - ndm2,
    dr = nrm0 - nrm2,
    x1 = nhm0 + nhm2,
    H = nhm0
  )
}


get_two_step_data <- function(nc, f_abs) {
  nabs <- (nc$I + nc$D + nc$R)[1:(length(nc$I) - 2)]
  
  ni <- summary(f_abs, pars = "I")$summary[, 1]
  nim2 <- rev(rev(ni)[-(1:2)])
  nim0 <- ni[-(1:2)]
  
  nh <- nc$I[-(1:2)]
  nhm2 <- rev(rev(nh)[-(1:2)])
  nhm0 <- nh[-(1:2)]
  
  mn = max(ni + nabs)
  
  list(
    n_t = length(nabs) - 2,
    di = nim0 - nim2,
    dh = nhm0 - nhm2,
    x1 = nim0 + nim2,
    x2 = nim0 * nim2,
    n_a = length(nabs),
    A = nabs,
    I = ni,
    r_out = sum(summary(f_abs, pars = c("r_death", "r_rec"))$summary[, 1]),
    m_mn = mn,
    m_mx = mn * 20
  )
}


m_abs <- stan_model("Source/Absorb.stan")
m_bass <- stan_model("Source/BassSIR.stan")


sample_absorb <- function(d, r_iso,
                          pars = c("r_death", "r_rec", "I", "cdg", "hi_death", "hi_rec"),
                          ...) {
  d$r_iso <- r_iso
  out <- rstan::sampling(m_abs, data = d, pars=pars, ...)
  return(out)
}


sample_bass <- function(d, r_iso,
                        pars = c("beta", "kappa", "m", "I_hat", "R0", "Rt", "PrEx"),
                        ...) {
  d$r_iso <- r_iso
  out <- rstan::sampling(m_bass, data = d, pars=pars, ...)
  return(out)
}


a_time_point <- function(sel, 
                         r_iso = 1/6.1,
                         n_iter_1 = 1E4,
                         n_iter_2 = 1E5,
                         n_collect = 5000,
                         n_chain = 3,
                         thres = 1.05, ...) {
  d <- BassSIR::as_bass_data(sel, status = "Cumulative")
  
  pass <- F
  
  dat1 <- get_absorb_data(d)
  
  f_abs <- sample_absorb(dat1, r_iso, iter = n_iter_1, warmup = n_iter_1 - n_collect, 
                         chain = n_chain, ...)
  
  dat2 <- get_two_step_data(d, f_abs)
  
  f_bass <- sample_bass(dat2, r_iso, iter = n_iter_2, warmup = n_iter_2 - n_collect, 
                        chain = n_chain, ...)
  
  
  if (length(f_bass) >= 1) {
    ps <- c(0.025, 0.5, 0.975)
    
    sts <- rbind(summary(f_abs, pars=c("r_death", "r_rec", "hi_death", "hi_rec"), prob = ps)$summary,
                 summary(f_bass, pars=c("beta", "kappa", "m", "R0"), prob = ps)$summary)
    
    for (index in c("Rt", "I_hat", "PrEx")) {
      summ <- summary(f_bass, pars = index, prob = ps)$summary
      sts <- rbind(sts, summ[nrow(summ), ])
    }
    rownames(sts)[(nrow(sts) - 2):nrow(sts)] <- c("Rt", "I_hat", "PrEx")
    
    sts <- sts[, c("mean", "50%", "2.5%", "97.5%")]
    colnames(sts) <- c("mean", "median", "lower", "upper")
    sts <- data.frame(sts)
    
    Rhat <- max(summary(f_bass, c("m", "beta", "kappa"))$summary[, "Rhat"])
    pass <- Rhat < thres
    
  } else {
    sts <- list()
    Rhat <- Inf
    pass <- F
  }
  
  list(
    Date = sel$Date[nrow(sel)],
    Indices = sts,
    Converge = pass,
    Rhat = Rhat,
    N_try = 1
  )
}


new_country <- function(raw, loc, len = 14, ...) {
  if (nrow(raw) <= len + 2) {
    sel <- raw
    indices <- list(a_time_point(sel, ...))
  } else {
    indices <- lapply((len + 2):nrow(raw), function(i) {
      sel <- raw[i - ((len + 1):0), ]
      res <- suppressWarnings(a_time_point(sel, ...))
      cat(loc, " ", format.Date(res$Date, "%d-%b"), "\n")
      res
    })
  }
  
  list(
    Location = loc,
    Estimates = indices
  )
}


update_country <- function(raw, old_res, len = 14, ...) {
  dates <- sapply(old_res$Estimates, function(ind) { ind$Date })
  loc = old_res$Location
  
  if (nrow(raw) <= len + 2) {
    sel <- raw
    if (!(sel$Date[nrow(sel)] %in% dates)) {
      old_res$Estimates[[length(old_res$Estimates) + 1]] <-  suppressWarnings(a_time_point(sel, ...))
    }
  } else {
    for (i in (len + 2):nrow(raw)) {
      sel <- raw[i - ((len + 1):0), ]
      if (!(sel$Date[nrow(sel)] %in% dates)) {
        res <-  suppressWarnings(a_time_point(sel, ...))
        old_res$Estimates[[length(old_res$Estimates) + 1]] <- res
        cat(loc, " ", format.Date(res$Date, "%d-%b"), "\n")
      }
    }
  }
  old_res
}


renew_country <- function(raw, old_res, len = 14, ...) {
  loc = old_res$Location
  dates <- sapply(old_res$Estimates, function(ind) { ind$Date })
  
  if (nrow(raw) <= len + 2) {
    sel <- raw
    res <- old_res$Estimates[[1]]
    if (!res$Converge) {
      res_new <- suppressWarnings(a_time_point(sel, ...))
      if (res_new$Rhat < res$Rhat) {
        res_new$N_try <- res_new$N_try + res$N_try 
        res <- res_new
      }
    }
    indices <- list(res)
  } else {
    indices <- lapply((len + 2):nrow(raw), function(i) {
      sel <- raw[i - ((len + 1):0), ]
      res <- old_res$Estimates[[which(dates == sel$Date[nrow(sel)])]]
      if (!res$Converge) {
        res_new <- suppressWarnings(a_time_point(sel, ...))
        if (res_new$Rhat < res$Rhat) {
          res_new$N_try <- res_new$N_try + res$N_try 
          res <- res_new
        }
        cat(loc, " ", format.Date(res$Date, "%d-%b"), ifelse(res$Converge, "*", ""), "\n")
      }
      res
    })
  }
  
  list(
    Location = loc,
    Estimates = indices
  )
}

