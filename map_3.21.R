#######add circles########
library(RCurl)
usage_by_state_2015<- read.csv(text = getURL("https://raw.githubusercontent.com/XUKEREN/decision-aid-maps/master/usage%20by%20state%202015.csv"))
View(usage_by_state_2015)
usage_by_state_2016<- read.csv(text = getURL("https://raw.githubusercontent.com/XUKEREN/decision-aid-maps/master/usage%20by%20state%202016.csv"))
View(usage_by_state_2016)
usage_by_state_2017<- read.csv(text = getURL("https://raw.githubusercontent.com/XUKEREN/decision-aid-maps/master/usage%20by%20state%202017.csv"))
library(magrittr)
library(leaflet)
library(dplyr)

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
names(state_level_df)
names(state_level_df2016)
names(state_level_df2017)
state_level_df$popup = paste(state_level_df$state, state_level_df$freq, sep=" sessions in 2015: ")
state_level_df2016$popup = paste(state_level_df2016$state, state_level_df2016$freq, sep=" sessions in 2016: ")
state_level_df2017$popup = paste(state_level_df2017$state, state_level_df2017$freq, sep=" sessions in 2017: ")

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

citation(package = 'leaflet')
citation()
