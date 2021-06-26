library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(tidyr)
library(data.table)
library(ggplot2)
library(hrbrthemes)

getLapsByRace <- function(year, race){
  url1 <- paste0("https://ergast.com/api/f1/", year, "/", race, "/laps.json")
  laps1 <- GET(url1)
  laps1 <- fromJSON(content(laps1, as = "text"))
  limit <- as.integer(laps1$MRData$total) - 30 # this is used to determine how many records to request in second api call
  laps1 <- laps1$MRData$RaceTable$Races$Laps[[1]]

  url2 <- paste0("https://ergast.com/api/f1/", year, "/", race, "/laps.json?offset=30&limit=", limit)
  laps2 <- GET(url2)
  laps2 <- fromJSON(content(laps2, as = "text"))$MRData$RaceTable$Races$Laps[[1]]

  laps <- bind_rows(laps1, laps2) %>% unnest(.data$Timings) %>%
    mutate(lap = as.integer(.data$number), position = as.integer(.data$position)) %>%
    select(-.data$number)

  laps$time <- ms(laps$time)
  laps$seconds <- 60*lubridate::minute(laps$time) + lubridate::second(laps$time)
  laps
}

year = 2002; race = 3
lap_chart = function(year, race){
  laps = getLapsByRace(year, race)
  data = data.table(laps[, c(1, 2, 4, 5)])
  data[, `:=` (driverName = tools::toTitleCase(gsub("_", " ", driverId)))]
  drivers = data[, .(.N, totalTime = sum(seconds)), by = .(driverName)][order(-N, totalTime)]$driverName
  data$driverName = factor(data$driverName, levels = drivers)

  lapChart = ggplot(data, aes(x = lap, y = position, color = driverName)) + 
    geom_line(size = 1.2) +
    theme_ipsum_es() +
    labs(title = paste(year, " Race ", race, " - ", "Laps Chart", sep = ""), 
         x = "Lap", 
         y = "Position", 
         color = "Driver Name") +
    scale_y_reverse(breaks=1:19)
  lapChart
}
lap_chart(2020, 12)


plot_comparison = function(year, race, driver1, driver2){
  laps = getLapsByRace(year, race)
  
  data = data.table(laps[, c(1, 2, 4, 5)])
  data[, `:=` (driverName = tools::toTitleCase(gsub("_", " ", driverId)))]
  drivers = data[, .(.N, totalTime = sum(seconds)), by = .(driverName)][order(-N, totalTime)]$driverName
  data$driverName = factor(data$driverName, levels = drivers)

  comparisonData = data[(driverName == driver1 | driverName == driver2)][order(lap, driverName)]
  comparisonData[, diff := seconds - seconds[1], .(lap)]

  comparisonPlot = ggplot(comparisonData, aes(x = lap, y = diff, color = driverName)) + 
    geom_line(size = 1.2) + 
    theme_ipsum_es() + 
    labs(title = paste(year, " Race ", race, " - ", driver1, " vs ", driver2, " Comparison", sep = ""), 
         x = "Lap", 
         y = "Difference", 
        color = "Driver Name") + 
    scale_color_manual(values = c("green", "blue")) +
    coord_cartesian(ylim = c(-2, 2))
  
  return(comparisonPlot)
}
plot_comparison(2021, 4, "Hamilton", "Bottas")


fastest_lap_chart = function(year, race){
  laps = getLapsByRace(year, race)
  data = data.table(laps[, c(1, 2, 4, 5)])

  data[, `:=` (driverName = tools::toTitleCase(gsub("_", " ", driverId)))]
  drivers = data[, .(.N, totalTime = sum(seconds)), by = .(driverName)][order(-N, totalTime)]$driverName
  data$driverName = factor(data$driverName, levels = drivers)
  fastestLap = data[, .(position = match(driverName, drivers), fl = min(seconds), onlap = lap[(seconds == min(seconds))]), driverName][order(fl)]

  fastestLapChart = ggplot(fastestLap, aes(x = factor(driverName, levels = driverName), y = fl, fill = onlap, group = 1)) + 
    geom_bar(stat = "identity")+ 
    geom_line(mapping = aes(x = factor(driverName, levels = driverName), y = 4 * position), color = "red", size = 1) + 
    scale_y_continuous(sec.axis = sec_axis(~ ./4)) +
    theme_ipsum_es() + 
    labs(title = paste(year, " Race ", race, " - ", "Fastest Laps", sep = ""), 
           x = "Lap", 
           y = "Fastest Lap (s)") + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  fastestLapChart
}

fastest_lap_chart(2000, 3)

