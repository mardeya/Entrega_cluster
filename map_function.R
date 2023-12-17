library(factoextra)
library(leaflet)
library(tidyverse)


# Pes clustering de k-mitjanes posar
#   generate_map(km$cluster)
# Pes clustering jerarquitzat posar
#   generate_map(cutree(hc, k=4))

generate_map <- function (cluster) {
  world_map <- map_data("world")

  world_map <- world_map %>%
    mutate(region = ifelse(region == "USA", "United States", region)) %>%
    mutate(region = ifelse(region == "UK", "United Kingdom", region)) %>%
    mutate(region = ifelse(region == "Russia", "Russian Federation", region)) %>%
    mutate(region = ifelse(region == "Tanzania", "Tanzania (United Republic of)", region)) %>%
    mutate(region = ifelse(region == "Saint Vincent", "Saint Vincent and the Grenadines", region)) %>%
    mutate(region = ifelse(region == "Venezuela", "Venezuela (Bolivarian Republic of)", region)) %>%
    mutate(region = ifelse(region == "Vietnam", "Viet Nam", region)) %>%
    mutate(region = ifelse(region == "Swaziland", "Eswatini (Kingdom of)", region)) %>%  # quin nom és aquest
    mutate(region = ifelse(region == "Syria", "Syrian Arab Republic", region)) %>%
    mutate(region = ifelse(region == "Moldova", "Moldova (Republic of)", region)) %>%
    mutate(region = ifelse(region == "Laos", "Lao People's Democratic Republic", region)) %>%
    mutate(region = ifelse(region == "Democratic Republic of the Congo", "Congo (Democratic Republic of the)", region)) %>%
    mutate(region = ifelse(region == "Republic of Congo", "Congo", region)) %>%
    mutate(region = ifelse(region == "Ivory Coast", "Cote d'Ivoire", region)) %>%
    mutate(region = ifelse(region == "Czech Republic", "Czechia", region)) %>%
    mutate(region = ifelse(region == "Somalia", "Somalya", region)) %>%
    mutate(region = ifelse(region == "Iran", "Iran (Islamic Republic of)", region)) %>%
    mutate(region = ifelse(region == "Bolivia", "Bolivia (Plurinational State of)", region))

  w <- world_map %>% ggplot(aes(x = long, y = lat)) +
    geom_polygon(aes(group = group, fill = as.factor(cluster[region]))) +
    theme(legend.position = "none")

  return(w)
}
