library(dplyr)
library(leaflet)
library(plotly)
library(lubridate)
library(ggplot2)
library(knitr)

shooting_data <-
  read.csv(
    "/Users/jayrg007/Desktop/INFO201/a5-b-JayrG007/data/shootings-2018.csv",
    header = TRUE
  )

shooting_plot <- ggplot(data = shooting_data) + geom_point(
  mapping =
    aes(
      x = num_killed,
      y = state, color = num_killed
    )
)
shooting_plot <- shooting_plot +
  ggtitle("Number of people killed in each state") +
  xlab("Number of people killed") +
  ylab("State")


map_of_shootings <- leaflet(shooting_data) %>%
  addTiles() %>%
  addCircles(
    lng = ~long,
    lat = ~lat,
    radius = ~sqrt(num_killed) * 30,
    weight = 4,
    popup =
      paste0(
      "City: ", shooting_data$city, "<br>",
      "Killed: ", shooting_data$num_killed, "<br>",
      "Injured: ", shooting_data$num_injured, "<br>")
  )


total_events <- nrow(shooting_data)

total_lives_lost <- sum(shooting_data$num_killed)

total_people_injured <- sum(shooting_data$num_injured)

city_most_impacted <- shooting_data %>%
  select(num_killed, num_injured, city) %>%
  filter(num_killed == max(num_killed)) %>%
  select(city)

num_killed_in_city <- shooting_data %>%
  select(city, num_killed) %>%
  filter(num_killed == max(num_killed)) %>%
  select(num_killed)

summary_table <- shooting_data %>%
  summarize(
    Total_Killed = sum(num_killed),
    Average_Killed = mean(num_killed),
    Total_Injured = sum(num_injured),
    Average_Injured = mean(num_injured)
  )

parkland_date <- shooting_data %>%
  filter(city == "Pompano Beach (Parkland)") %>%
  select(date)

parkland_incident <- shooting_data %>%
  filter(city == "Pompano Beach (Parkland)") %>%
  select(city)

num_killed_in_parkland <- shooting_data %>%
  filter(city == "Pompano Beach (Parkland)") %>%
  select(num_killed)

num_injured_in_parkland <- shooting_data %>%
  filter(city == "Pompano Beach (Parkland)") %>%
  select(num_injured)
