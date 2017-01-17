## ---- warning = FALSE, message = FALSE, echo = FALSE---------------------
library(highcharter)
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
options(highcharter.theme = hc_theme_smpl())

## ---- warning=FALSE, message=FALSE---------------------------------------
hcmap("countries/nz/nz-all")
hcmap("custom/usa-and-canada", showInLegend = FALSE)
hcmap("countries/us/us-ca-all") %>%
  hc_title(text = "California")

## ------------------------------------------------------------------------
require(dplyr)

mapdata <- get_data_from_map(download_map_data("countries/us/us-all"))
glimpse(mapdata)

set.seed(1234)

data_fake <- mapdata %>% 
  select(code = `hc-a2`) %>% 
  mutate(value = 1e5 * abs(rt(nrow(.), df = 10)))

glimpse(data_fake)

## ------------------------------------------------------------------------
hcmap("countries/us/us-all", data = data_fake, value = "value",
      joinBy = c("hc-a2", "code"), name = "Fake data",
      dataLabels = list(enabled = TRUE, format = "{point.name}"),
      borderColor = "#FAFAFA", borderWidth = 0.1,
      tooltip = list(valueDecimals = 2, valuePrefix = "$", valueSuffix = "USD")) 

## ------------------------------------------------------------------------
data("quakes")

quakes <- quakes %>% 
  rename(lon = long) %>% 
  mutate(z = mag,
         color = colorize(depth)) %>% 
  filter(z > 5)

glimpse(quakes)

hcmap("countries/fj/fj-all", name = "Fiji") %>% 
  hc_add_series(data = quakes, type = "mapbubble",
                name = "Quakes", maxSize = "10") %>% 
  hc_title(text = "Locations of Earthquakes off Fiji") %>% 
  hc_subtitle(text = "Locations of seismic events of MB > 5.0. The events 
              occurred in a cube near Fiji since 1964")

## ------------------------------------------------------------------------
getContent <- function(url) {
  library(httr)
  content(GET(url))
}

world <- getContent("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json")
# is text
world <- jsonlite::fromJSON(world, simplifyVector = FALSE)

# http://cedeusdata.geosteiniger.cl/layers/geonode:mundo_corrientes_maritimas
marine <- getContent("http://cedeusdata.geosteiniger.cl/geoserver/wfs?srsName=EPSG%3A4326&typename=geonode%3Amundo_corrientes_maritimas&outputFormat=json&version=1.0.0&service=WFS&request=GetFeature")
# marine <- geojsonio::as.json(marine)


# http://cedeusdata.geosteiniger.cl/layers/geonode:mundo_limites_placas
plates <- getContent("http://cedeusdata.geosteiniger.cl/geoserver/wfs?srsName=EPSG%3A4326&typename=geonode%3Amundo_limites_placas&outputFormat=json&version=1.0.0&service=WFS&request=GetFeature")
# plates <- geojsonio::as.json(plates)

# http://cedeusdata.geosteiniger.cl/layers/geonode:mundo_volcanes
volcano <- getContent("http://cedeusdata.geosteiniger.cl/geoserver/wfs?srsName=EPSG%3A4326&typename=geonode%3Amundo_volcanes&outputFormat=json&version=1.0.0&service=WFS&request=GetFeature")
# volcano <- geojsonio::as.json(volcano)


## ------------------------------------------------------------------------
highchart(type = "map") %>%
  hc_chart(backgroundColor = "#161C20") %>% 
  hc_add_series(mapData = world, showInLegend = FALSE, nullColor = "#424242",
                borderWidth = 0) %>%
  hc_add_series(data = marine, type = "mapline", geojson = TRUE,
                color = "#2980b9", name = "Marine currents",
                tooltip = list(pointFormat = "{point.properties.NOMBRE}")) %>%
  hc_add_series(data = plates, type = "mapline", lineWidth = 2, zIndex = -1,
                geojson = TRUE,
                color = "#d35400", name = "Plates",
                tooltip = list(pointFormat = "{point.properties.TIPO}")) %>%
  hc_add_series(data = volcano, type = "mappoint",
                color = hex_to_rgba("#f1c40f", 0.4),
                geojson = TRUE, name = "Volcanos",
                tooltip = list(pointFormat = "{point.properties.NOMBRE}"),
                marker = list(lineWidth = 0, radius = 2))

## ------------------------------------------------------------------------
library(highcharter)
library(dplyr)
library(purrr)

set.seed(1234)

n <- 20
z <-  sample(1:n)
sequences <- map2(1:n, z, function(x, y){ ifelse(x == 1:n, y, 0) })

df <- data_frame(
  lat = runif(n, -180, 180),
  lon = runif(n, -180, 180),
  z = z,
  color = colorize(z),
  sequence = sequences
  )

hcmap() %>% 
  hc_add_series(data = df, type = "mapbubble",
                minSize = 0, maxSize = 30) %>% 
  hc_motion(enabled = TRUE, series = 1, labels = 1:n,
            loop = TRUE, autoPlay = TRUE, 
            updateInterval = 1000, magnet = list(step =  1)) %>% 
  hc_plotOptions(series = list(showInLegend = FALSE))

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  library(geojsonio)
#  
#  ausgeojson <- getContent("https://raw.githubusercontent.com/johan/world.geo.json/master/countries/AUS.geo.json")
#  ausgeojson <- jsonlite::fromJSON(ausgeojson, simplifyVector = FALSE)
#  ausgeojson <- geojsonio::as.json(ausgeojson)
#  class(ausgeojson)
#  
#  # http://openflights.org/data.html
#  airports <- read.csv("https://commondatastorage.googleapis.com/ckannet-storage/2012-07-09T214020/global_airports.csv")
#  airports <- filter(airports,
#                     country == "Australia",
#                     name != "Roma Street Railway Station")
#  
#  airp_geojson <- geojson_json(airports, lat = "latitude", lon = "longitude")
#  class(airp_geojson)
#  
#  highchart(type = "map") %>%
#    hc_add_series(mapData = ausgeojson, showInLegend = FALSE) %>%
#    hc_add_series(data = airp_geojson, type = "mappoint",
#                  dataLabels = list(enabled = FALSE),
#                  name = "Airports", tooltip = list(pointFormat = "{point.name}"))

