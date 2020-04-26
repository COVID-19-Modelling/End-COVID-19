load(file = "Data/Input.rdata")
load(file = "Output/EpiIndices.rdata")

source("Source/Runs.R")


n_iter <- 1E4
max_attempt <- 3

for (country in names(n_by_country)) {
  raw <- n_by_country[[country]]
  
  if (country %in% names(epi_indices)) {
    old_res <- epi_indices[[country]]
    epi_indices[[country]] <- update_country(raw, old_res, n_iter = n_iter, max_attempt = max_attempt, len = 14)
  } else {
    epi_indices[[country]] <- new_country(raw, n_iter = n_iter, max_attempt = max_attempt, len = 14)
  }
  
  write(jsonlite::toJSON(epi_indices, auto_unbox = T), "Output/EpiIndices.json")
  save(epi_indices, file = "Output/EpiIndices.rdata")
  cat(country, "N=", length(epi_indices[[country]]$Estimates), "\n")
}


# save(epi_indices, file = "Output/EpiIndices.rdata")
# write(jsonlite::toJSON(epi_indices, auto_unbox = T), "Output/EpiIndices.json")

