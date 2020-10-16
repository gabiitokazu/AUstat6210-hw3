install.packages(c("htmltools","leaflet","ggmap"))
library(htmltools)
library(leaflet)
library(tidyverse)
library(ggmap)
library(sf)
library(mapview)

GGMAP_GOOGLE_API_KEY

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


#longitude and latitude to be plotted using the World Geographic System 1984 projection, which is referenced as European Petroleum Survey Group (EPSG) 4326. Geographic jargon aside, what matters at this stage is that EPSG 4326 is the projection used by web maps such as Google Maps.
#
map_sf <- st_as_sf(map, coords = c("lon","lat"), crs = 4326)

mapview(map_sf)




# nao tava funcionando porque nao reconhecia a API key nem
# reconhecia os caracteres
# cities <- bonus$City
# cities_df <- as.data.frame(cities, stringsAsFactor = FALSE)
# 
# 
# 
# 
# ------------------

##--example--------------------------------------------------
m <- leaflet() %>%
     addTiles() %>%  # Add default OpenStreetMap map tiles
     addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")

##-----------------------------------------------------------
ExampleCity <- paste(sep = "<br/>",
                     "<b>Example Team</b>",
                     "<a href='https://www.exampleteam.com/en/'>Example Team</a>",
                     "Champions: 100"
)





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
        addTiles() %>%
        addPopups(lng=-3.7037902, lat=40.41678, Madrid,
                  options = popupOptions(closeButton = FALSE)) %>%
        addCircles(lng=-3.7037902, lat=40.41678,
                   weight = 5, radius = (34000*2)) %>%
        addCircles(lng=-3.7037902, lat=40.41678,
                   weight = 5, radius = (10000*2)) %>%
        addPopups(lng=2.1734035, lat=41.38506, Barcelona,
                  options = popupOptions(closeButton = FALSE)) %>%
        addCircles(lng=2.1734035, lat=41.38506,
                   weight = 5, radius = (26000*2)) %>%
        addPopups(lng=-2.9349852, lat=43.26301, Bilbao,
                  options = popupOptions(closeButton = FALSE)) %>%
        addCircles(lng=-2.9349852, lat=43.26301,
                   weight = 5, radius = (8000*2)) %>%
        addPopups(lng=-0.3762881, lat=39.46991, Valencia,
                  options = popupOptions(closeButton = FALSE)) %>%
        addCircles(lng=-0.3762881, lat=39.46991,
                   weight = 5, radius = (6000*2))



m <- leaflet() %>%
        addTiles() %>%
        addCircles(lng=-3.7037902, lat=40.41678, popup=paste("Madrid"), weight = 5, radius = (34000*2))


     addHomeMarker(){L.circle({lng: -3.7037902, lat: 40.41678},
                              {color: "steelblue",
             radius: 200,
                                      fillcolor:"steelblue",
             opacity: 0.6}).addTo(m)}
        
#html:
L.circle({lng: -3.7037902, lat: 40.41678}, {color: "steelblue",
        radius: 200, fillcolor:"steelblue",
        opacity: 0.6}).addTo(m)

L.circleMaker(lng= 3.7037902, lat= 40.41678, radius = 25)
#-------

leaflet(m) %>% addTiles() %>%
        addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                   radius = ~sqrt(Pop) * 30, popup = ~City
        )
        
        
     
#---------------------------------------------------------
o <- leaflet() %>%
     addTiles() %>% 
     addMarkers(lng=-3.7037902, lat=40.41678,
                label= Madrid,
                labelOptions = labelOptions(noHide = T))

#----# iconzinho bonitinho
icons <- awesomeIcons(
        icon = "football-outline",
        iconColor = 'black',
        markerColor = "white",
        library = 'ion'
)
leaflet(data = mymap) %>%
        addTiles() %>%
        addAwesomeMarkers(icon = icons) %>%
        frameWidget()
#----#

####----------------------------------------------------------
# coisas bonitas:

Madrid2 <- c(lon=-3.7037902, lat=40.41678)
Barcelona2 <- c(lon=2.1734035, lat=41.38506)
Bilbao2 <- c(lon=-2.9349852, lat=43.26301)
Valencia2 <- c(lon=-0.3762881, lat=39.46991)

locus <- "Spain"

MyMap <- get_map(location=locus, zoom=12, source="stamen", maptype="watercolor", crop=TRUE)

MyMap <- get_map(location=locus, zoom=10, source="stamen", maptype="toner", crop=TRUE)

ggmap(MyMap)

p <- ggmap(MyM) %>%  # Add default OpenStreetMap map tiles
        addPopups(lng=-3.7037902, lat=40.41678, Madrid,
                  options = popupOptions(closeButton = FALSE)) %>%
        addPopups(lng=2.1734035, lat=41.38506, Barcelona,
                  options = popupOptions(closeButton = FALSE)) %>%
        addPopups(lng=-2.9349852, lat=43.26301, Bilbao,
                  options = popupOptions(closeButton = FALSE)) %>%
        addPopups(lng=-0.3762881, lat=39.46991, Valencia,
                  options = popupOptions(closeButton = FALSE))




#####--- totally not totally satisfied, but I want to play with other maps........























