root <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

confirmed <- read.csv(paste0(root, "time_series_covid19_confirmed_global.csv"))
recovered <- read.csv(paste0(root, "time_series_covid19_recovered_global.csv"))
deaths <- read.csv(paste0(root, "time_series_covid19_deaths_global.csv"))


locations <- unique(confirmed$Country.Region)
cat(length(locations), " countries/regions in the data\n")
n_by_country <- list()

t0 <- as.Date("01-03-2020", "%d-%m-%y")

for (country in locations) {
  temp <- data.frame(
    Location = country,
    Date = as.Date("22-01-2020", "%d-%m-%y") + 0:(ncol(confirmed)-5),
    Confirmed = colSums(confirmed[confirmed$Country.Region == country, -c(1:4)]),
    Cured = colSums(recovered[recovered$Country.Region == country, -c(1:4)]),
    Dead = colSums(deaths[deaths$Country.Region == country, -c(1:4)])
  )
  temp <- temp[temp$Confirmed > 100, ]
  temp <- temp[temp$Date >= t0, ]
  rownames(temp) <- NULL
  if (nrow(temp) >= 16) {
    n_by_country[[country]] <- temp
  }
}

names(n_by_country)[names(n_by_country) == "Taiwan*"] <- "Taiwan"
names(n_by_country)[names(n_by_country) == "Korea, South"] <- "South Korea"


cat(length(n_by_country), " countries/regions collected\n")


save(n_by_country, file = "Data/Input.rdata")

