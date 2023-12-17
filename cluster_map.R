library(tidyverse)
library(factoextra)
library(leaflet)

db = read.csv("dataset_tidy.csv")
db <- db %>%
  mutate(HDI_level = factor(HDI_level, ordered = T,
                            levels = c("Low", "Medium", "High", "Very High")),
         Continent = factor(Continent))
db %>% glimpse


rownames(db) <- db$Country
db_cuant <- db %>% select(HDI, CO2_prod, GNIPC, Life_expect, Population) %>% na.omit()
datos <- scale(db_cuant)  #escalar las variables
mat_dist <- dist(datos, method = "euclidian")  # Distancia euclidíana
round(as.matrix(mat_dist)[1:5, 1:5], 2)


set.seed(101)
km_clusters <- kmeans(datos, centers = 4, nstart = 25)


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

mypalette <- colorNumeric( palette="viridis", domain=km_clusters$cluster[world_map$region], na.color="red")

world_map %>% ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = mypalette(km_clusters$cluster[region])))+
  # geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")
