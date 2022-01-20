## Google trends
library(gtrendsR)
library(reshape2)

# Buscamos tequila
google.trends = gtrends(c("tequila"), gprop = "web", time = "all")[[1]]
google.trends = dcast(google.trends, date ~ keyword + geo, value.var = "hits")
rownames(google.trends) = google.trends$date

# google.trends$date = NULL
# Sys.setenv(TZ = "UTC")
# plot(google.trends$tequila_world)
# write.csv(google.trends, "C:/Users/Naim/Desktop/tequila_general.csv")

google.trends.country = gtrends(c("tequila"), geo = c("MX", "US"), gprop = "web", time = "2010-06-30 2022-01-19")[[1]]

# geo.codes = sort(unique(countries[substr(countries$sub_code, 1, 2) == "IT", ]$sub_code))
# write.csv(google.trends.country, "C:/Users/Naim/Desktop/tequila_mx_us.csv")

# Buscamos mezcal
google.trends = gtrends(c("mezcal"), gprop = "web", time = "all")[[1]]
google.trends = dcast(google.trends, date ~ keyword + geo, value.var = "hits")
rownames(google.trends) = google.trends$date

# write.csv(google.trends, "C:/Users/Naim/Desktop/mezcal_general.csv")

google.trends.country = gtrends(c("mezcal"), geo = c("MX", "US"), gprop = "web", time = "2010-06-30 2022-01-19")[[1]]

# write.csv(google.trends.country, "C:/Users/Naim/Desktop/mezcal_mx_us.csv")
