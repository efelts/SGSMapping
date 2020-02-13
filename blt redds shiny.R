



###creating file of waypoints
Dat <- read.csv("Waypoints.csv")



#only keep waypoints with coordinates
Dat <-filter(Dat, Dat$Lat..dec.deg. > .1)
Dat <-filter(Dat, Dat$Long..dec.deg. < .1)
Dat$Year <- as.factor(Dat$Year)




# create function to designate colors for each waypoint type
colorfunc <- function(Dat) {
  sapply(Dat$WaypointType, function(WaypointType){
    if(WaypointType == "Beaverdam"){
      "green"
    }    else if(WaypointType == "Redd"){
      "red"
    }    else if(WaypointType == "SectionEnds"){
      "black"
    }    else if(WaypointType == "LiveFish"){
      "blue"
    }    else if(WaypointType == "Logjam"){
      "white"
    }    else if(WaypointType == "Mine"){
      "orange"
    }    else if(WaypointType == "Barrier"){
      "purple"
    }  })
}











ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 1, right = 1,
                checkboxGroupInput("YearInput", 
                                   label = "Choose Year", 
                                   choices = unique(Dat$Year)),
                checkboxGroupInput("TypeInput", 
                                   label = "Choose Type", 
                                   choices = unique(Dat$WaypointType ))
                
  )
)


server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  CheckBoxYear <- reactive({   Dat %>%
      filter(Year %in% input$YearInput)%>%
      filter(WaypointType %in% input$TypeInput)
    
  })
  
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(Dat) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(lng = -115.33771, lat = 47.05847, zoom = 10) %>%
      addMarkers(lat = ~ Lat..dec.deg.,
                 lng = ~ Long..dec.deg.)
  })
  
  
  
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    
    
    #create icons that are all the same design but have the colors listed in the colorfunction above   
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = colorfunc(CheckBoxYear())
    )
    
    
    leafletProxy("map", data = CheckBoxYear()) %>%
      clearMarkerClusters() %>%
      clearMarkers()  %>%
      addAwesomeMarkers(lng = ~Long..dec.deg., 
                        lat = ~Lat..dec.deg., 
                        icon = ~icons, 
                        popup = ~ paste(Year,Stream, WaypointType), 
                        clusterOptions = markerClusterOptions(freezeAtZoom = 18)
      )
    
    
    
    
  })
  
}


shinyApp(ui, server) 



