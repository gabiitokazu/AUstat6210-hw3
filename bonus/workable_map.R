install.packages(c("htmltools","leaflet","ggmap"))
library(htmltools)
library(leaflet)
library(tidyverse)
library(ggmap)
library(sf)
library(mapview)

googlemaps_api_key = Sys.getenv("GOOGLEMAPS_API_KEY")

register_google(key = googlemaps_api_key)

Teams <- tibble(Teams = c("Real Madrid",
                          "FC Barcelona",
                          "Atlético Madrid",
                          "Athletic Bilbao",
                          "Valencia"))
Champions <- tibble(Champions = c(34,
                                  26,
                                  10,
                                  8,
                                  6))
City <- tibble(City = c("Madrid, Spain",
                        "Barcelona, Spain",
                        "Madrid, Spain",
                        "Bilbao, Spain",
                        "Valencia, Spain"))

bonus <- bind_cols(Teams, Champions, City)

try <- t(bonus)

# Cities <- as.data.frame(bonus$City)


#locations <- mutate_geocode(Cities, bonus$City)
map <- mutate_geocode(bonus, City)


#l ongitude and latitude to be plotted using the World Geographic System 1984 projection, which is referenced as European Petroleum Survey Group (EPSG) 4326. Geographic jargon aside, what matters at this stage is that EPSG 4326 is the projection used by web maps such as Google Maps.
#
map_sf <- st_as_sf(map, coords = c("lon","lat"), crs = 4326)

mapview(map_sf)


# nao tava funcionando porque nao reconhecia a API key nem
# reconhecia os caracteres
# cities <- bonus$City
# cities_df <- as.data.frame(cities, stringsAsFactor = FALSE)


##--example--------------------------------------------------
m <- leaflet() %>%
     addTiles() %>%  # Add default OpenStreetMap map tiles
     addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")

##-----------------------------------------------------------

Barcelona <- paste(sep = "<br/>",
                 "<b>Barcelona</b>",
                 "<a href='https://www.fcbarcelona.com/en/'>FC Barcelona</a>",
                 "Champions: 26"
)

Madrid <- paste(sep = "<br/>",
                "<b>Madrid</b>",
                "<a href='https://www.realmadrid.com/en'>Real Madrid CF</a>",
                "Champions: 34",
                "<a href='https://en.atleticodemadrid.com/'>Atletico Madrid</a>",
                "Champions: 10"
)

Bilbao <- paste(sep = "<br/>",
                "<b>Bilbao</b>",
                "<a href='https://www.athletic-club.eus/en'>Athletic Bilbao</a>",
                "Champions: 8"
)

Valencia <- paste(sep = "<br/>",
                  "<b>Valencia</b>",
                  "<a href='https://www.valenciacf.com/en'>Valencia CF</a>",
                  "Champions: 6"
)


n <- leaflet() %>%
     addTiles() %>%
     addMarkers(lng=-3.7037902, lat=40.41678, popup=Madrid) %>%
     addMarkers(lng=2.1734035, lat=41.38506, popup=Barcelona) %>%
     addMarkers(lng=-2.9349852, lat=43.26301, popup=Bilbao) %>%
     addMarkers(lng=-0.3762881, lat=39.46991, popup=Valencia)


m <- leaflet() %>%
     addTiles() %>%  # Add default OpenStreetMap map tiles
     addPopups(lng=-3.7037902, lat=40.41678, Madrid,
               options = popupOptions(closeButton = FALSE)) %>%
     addPopups(lng=2.1734035, lat=41.38506, Barcelona,
              options = popupOptions(closeButton = FALSE)) %>%
     addPopups(lng=-2.9349852, lat=43.26301, Bilbao,
               options = popupOptions(closeButton = FALSE)) %>%
     addPopups(lng=-0.3762881, lat=39.46991, Valencia,
               options = popupOptions(closeButton = FALSE))
     

o <- leaflet() %>%
     addTiles() %>% 
     addMarkers(lng=-3.7037902, lat=40.41678,
                label= Madrid,
                labelOptions = labelOptions(noHide = T))



































