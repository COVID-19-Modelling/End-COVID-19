a_time_point <- function(sel, n_iter = 1E4, max_attempt = 4, thres = 1.02) {
  d <- BassSIR::as_bass_data(sel, status = "Cumulative")
  pass <- F
  n_attempt <- 0
  
  while (n_attempt < max_attempt & !pass) {
    res <- tryCatch({
      BassSIR::fit_full(d, n_iter = n_iter)
    },
    error = function(e) { list() }
    )
    
    n_attempt <- n_attempt + 1
    
    if (any(res$BUGS$summary[c("r_death", "r_rec", "beta", "kappa"), "Rhat"] > thres)) {
      n_iter <- n_iter * (n_attempt + 1) / n_attempt
    } else if (length(res) > 1) {
      pass <- T
    }
  }
  
  if (length(res) > 1) {
    sts <- summary(res)$Stats
  } else {
    sts <- list()
  }
  
  list(
    Date = sel$Date[nrow(sel)],
    Indices = sts,
    Converge = pass,
    N_try = n_attempt
  )
}


new_country <- function(raw, len = 14, ...) {
  if (nrow(raw) <= len + 2) {
    sel <- raw
    indices <- list(a_time_point(sel, ...))
  } else {
    indices <- lapply((len + 2):nrow(raw), function(i) {
      sel <- raw[i - ((len + 1):0), ]
      a_time_point(sel, ...)
    })
  }
  
  list(
    Location = country,
    Estimates = indices
  )
}


update_country <- function(raw, old_res, len = 14, check_converge = F, ...) {
  dates <- sapply(old_res$Estimates, function(ind) { ind$Date })
  
  if (nrow(raw) <= len + 2) {
    sel <- raw
    if (!(sel$Date[nrow(sel)] %in% dates)) {
      old_res$Estimates[[length(old_res$Estimates) + 1]] <- a_time_point(sel, ...)
    }
  } else {
    for (i in (len + 2):nrow(raw)) {
      sel <- raw[i - ((len + 1):0), ]
      if (!(sel$Date[nrow(sel)] %in% dates)) {
        old_res$Estimates[[length(old_res$Estimates) + 1]] <- a_time_point(sel, ...)
      }
    }
  }
  old_res
}