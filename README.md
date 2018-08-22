Import data
===========

The repository on Github **decision aid maps** includes the datasets. The R package RCurl was loaded to import web-based dataset.

``` r
library(RCurl)
usage_by_state_2015<- read.csv(text = getURL("https://raw.githubusercontent.com/XUKEREN/decision-aid-maps/master/usage%20by%20state%202015.csv"))
usage_by_state_2016<- read.csv(text = getURL("https://raw.githubusercontent.com/XUKEREN/decision-aid-maps/master/usage%20by%20state%202016.csv"))
usage_by_state_2017<- read.csv(text = getURL("https://raw.githubusercontent.com/XUKEREN/decision-aid-maps/master/usage%20by%20state%202017.csv"))
```

``` r
library(magrittr)
library(leaflet)
library(dplyr)
```

Leaflet map with circle markers
===============================

tidy data
---------

``` r
state<-tolower(usage_by_state_2015$Region)
freq<-usage_by_state_2015$Sessions
df <- data.frame( state,
                  freq,
                  stringsAsFactors = FALSE )
state.center$x<-replace(state.center$x, state.center$x==-127.2500, -149.4937)
state.center$y<-replace(state.center$y, state.center$y==49.2500, 64.2008)
state.center$x<-replace(state.center$x, state.center$x==-126.2500, -155.5828)
state.center$y<-replace(state.center$y, state.center$y==31.7500, 19.8968)
state50<-data.frame(state = tolower(state.name), 
                    long = state.center$x, 
                    lat = state.center$y,
                    stringsAsFactors = FALSE)
dc<-data.frame(state="district of columbia",long=-77.0369,lat=38.9072)
state51<-rbind(state50,dc)
state_level_df <- state51 %>%inner_join( df, by="state" )

freq2016<-usage_by_state_2016$Sessions
df2016 <- data.frame( state,
                      freq2016,
                      stringsAsFactors = FALSE )
state_level_df2016 <- state51 %>%inner_join( df2016, by="state" )

state<-tolower(usage_by_state_2017$Region)
freq2017<-usage_by_state_2017$Sessions
df2017 <- data.frame( state,
                      freq2017,
                      stringsAsFactors = FALSE )
state_level_df2017 <- state51 %>%inner_join( df2017, by="state" )
state_level_df$popup = paste(state_level_df$state, state_level_df$freq, sep=" sessions in 2015: ")
state_level_df2016$popup = paste(state_level_df2016$state, state_level_df2016$freq, sep=" sessions in 2016: ")
state_level_df2017$popup = paste(state_level_df2017$state, state_level_df2017$freq, sep=" sessions in 2017: ")
```

draw graphs
-----------

``` r
leaflet() %>% 
  addTiles(group = "OSM (default)")%>% 
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addCircles(lat = state_level_df$lat, 
             lng = state_level_df$long,weight = 2,
             radius =sqrt(state_level_df$freq) *10000,popup =state_level_df$popup, group = "Sessions 2015")%>%
  addCircles(lat = state_level_df2016$lat, 
             lng = state_level_df2016$long,weight = 5,color="#FFF100",
             radius =sqrt(state_level_df2016$freq) *10000,popup =state_level_df2016$popup, group = "Sessions 2016")%>%
  addCircles(lat = state_level_df2017$lat, 
             lng = state_level_df2017$long,weight = 5,color="#FF5F00",
             radius =sqrt(state_level_df2017$freq) *10000,popup =state_level_df2017$popup, group = "Sessions 2017")%>%
  addLayersControl(
    baseGroups = c("OSM (default)","Toner"),
    overlayGroups = c("Sessions 2015", "Sessions 2016","Sessions 2017"),
    options = layersControlOptions(collapsed = FALSE)
  )
```
Leaflet map with clusters
=========================

``` r
state_level_df_expanded <- state_level_df[rep(row.names(state_level_df), state_level_df$freq),]
content <- paste(sep = "<br/>",
                 "<b><a href='http://www.dartmouth-hitchcock.org/anniversary/dh125.html'>Work</a></b>",
                 "One Medical Center Drive",
                 "Lebanon, New Hampshire"
)
state_level_df2016_expanded <- state_level_df2016[rep(row.names(state_level_df2016), state_level_df2016$freq2016),]
state_level_df2017_expanded <- state_level_df2017[rep(row.names(state_level_df2017), state_level_df2017$freq2017),]
leaflet() %>% 
  addTiles(group = "OSM (default)")%>% 
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addMarkers(lat = state_level_df_expanded$lat, 
             lng = state_level_df_expanded$long,
             clusterOptions = markerClusterOptions(freezeAtZoom = 7),group = "Sessions 2015")%>%
  addPopups(-72.273558, 43.676177, content,
            options = popupOptions(closeButton = TRUE)
  )%>%
  addMarkers(lat = state_level_df2016_expanded$lat, 
             lng = state_level_df2016_expanded$long,
             clusterOptions = markerClusterOptions(freezeAtZoom = 7), group = "Sessions 2016")%>%
  addMarkers(lat = state_level_df2017_expanded$lat, 
             lng = state_level_df2017_expanded$long,
             clusterOptions = markerClusterOptions(freezeAtZoom = 7), group = "Sessions 2017")%>%
  addLayersControl(
    baseGroups = c("Toner","OSM (default)"),
    overlayGroups = c("Sessions 2015", "Sessions 2016","Sessions 2017"),
    options = layersControlOptions(collapsed = FALSE)
  )
```
``` r
citation(package = 'leaflet')
citation()
```

Choropleths
===========

tidy data
---------

``` r
library(geojson)
library(geojsonio)

url <- "http://leafletjs.com/examples/choropleth/us-states.js"

# read as text file
doc <- readLines(url)

# remove the javascript assignment at the front 
doc2 <- gsub("var statesData = ", "", doc)

# write out as a temp file and read
write(doc2, file = "tempgeo.json")
states <- geojson_read("tempgeo.json", what = "sp")
names(states)
states$name
states$density
class(states)
states@data$state<-tolower(states@data$name)
PR<-data.frame(state="puerto rico",long=NA,lat=NA,freq=NA,popup="NA")
state_level_dfNEW<-rbind(state_level_df,PR)
state_level_df<-state_level_dfNEW
merge1 <- states@data %>%inner_join( state_level_df, by="state" )
states@data <- merge1
states@data$freq

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states@data$freq, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g sessions in 2015",
  states$state, states$freq
) %>% lapply(htmltools::HTML)
```

draw graphs
-----------

``` r
leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addTiles(group = "OSM (default)")%>% 
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>% addPolygons(
  fillColor = ~pal(freq),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto")
  )%>% addLegend(pal = pal, values = freq, opacity = 0.7, title = NULL,
                 position = "bottomright")
```

References
==========

-   [Leaflet for R](https://rstudio.github.io/leaflet/)
