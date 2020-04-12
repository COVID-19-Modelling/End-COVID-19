load(file = "Data/Input.rdata")
load(file = "Output/EpiIndices.rdata")


a_time_point <- function(sel, n_iter = 1E4) {
  d <- BassSIR::as_bass_data(sel, status = "Cumulative")
  sts <- tryCatch({
    res <- BassSIR::fit_full(d, n_iter = n_iter)
    summary(res)$Stats
  },
  error = function(e) { list() }
  )
  
  list(
    Date = sel$Date[nrow(sel)],
    Indices = sts
  )
}


new_country <- function(raw, n_iter = 1E4, len = 14) {
  if (nrow(raw) <= len + 2) {
    sel <- raw
    indices <- list(a_time_point(sel, n_iter))
  } else {
    indices <- lapply((len + 2):nrow(raw), function(i) {
      sel <- raw[i - ((len + 1):0), ]
      a_time_point(sel, n_iter)
    })
  }
  
  list(
    Location = country,
    Estimates = indices
  )
}


update_country <- function(raw, old_res, n_iter = 1E4, len = 14) {
  dates <- sapply(old_res$Estimates, function(ind) { ind$Date })
  
  if (nrow(raw) <= len + 2) {
    sel <- raw
    if (!(sel$Date[nrow(sel)] %in% dates)) {
      old_res$Estimates[[length(old_res$Estimates) + 1]] <- a_time_point(sel, n_iter)
    }
  } else {
    for (i in (len + 2):nrow(raw)) {
      sel <- raw[i - ((len + 1):0), ]
      if (!(sel$Date[nrow(sel)] %in% dates)) {
        old_res$Estimates[[length(old_res$Estimates) + 1]] <- a_time_point(sel, n_iter)
      }
    }
  }
  old_res
}


n_iter <- 1E4

for (country in names(n_by_country)) {
  raw <- n_by_country[[country]]
  
  if (country %in% names(epi_indices)) {
    old_res <- epi_indices[[country]]
    epi_indices[[country]] <- update_country(raw, old_res, n_iter = n_iter, len = 14)
  } else {
    epi_indices[[country]] <- new_country(raw, n_iter = n_iter, len = 14)
  }
  
  write(jsonlite::toJSON(epi_indices, auto_unbox = T), "Output/EpiIndices.json")
  save(epi_indices, file = "Output/EpiIndices.rdata")
  cat(country, "N=", length(epi_indices[[country]]$Estimates), "\n")
}


# save(epi_indices, file = "Output/EpiIndices.rdata")
# write(jsonlite::toJSON(epi_indices, auto_unbox = T), "Output/EpiIndices.json")

