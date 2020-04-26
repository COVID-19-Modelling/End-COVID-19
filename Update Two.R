source("Source/TwoStep.R")
load(file = "Data/Input.rdata")

# epi_indices <- list()
load(file = "Output/EpiIndices.rdata")

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')

n_iter <- 1E4
n_chain <- 2

renew <- T


for (country in names(n_by_country)) {
  raw <- n_by_country[[country]]
  
  if (country %in% names(epi_indices)) {
    old_res <- epi_indices[[country]]
    res <- update_country(raw, old_res, n_chain = n_chain, refresh = 0, n_iter_2 = n_iter)
    if (renew) {
      res <- renew_country(raw, old_res = res, refresh = 0, n_chain = n_chain, n_iter_2 = n_iter * 2)
    }
    epi_indices[[country]] <- res
  } else {
    epi_indices[[country]] <- new_country(raw, loc=country, n_chain = n_chain, refresh = 0, n_iter_2 = n_iter)
  }
  
  write(jsonlite::toJSON(epi_indices, auto_unbox = T), "Output/EpiIndices.json")
  save(epi_indices, file = "Output/EpiIndices.rdata")
  cat(country, "N=", length(epi_indices[[country]]$Estimates), "\n")
}
