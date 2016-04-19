# routr.nyc

#############################
## Setup Info 
#############################

#Set Palette of Colors
palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

#Load Libraries
library(shiny)
library(leaflet)
library(ggmap)
library(memisc)
library(dplyr)
library(geosphere)
library(plyr)
library(ggplot2)
library(htmltools)
library(DT)

#############################
## Functions 
#############################
  
decodeLine <- function(encoded){
  require(bitops)
  
  vlen <- nchar(encoded)
  vindex <- 0
  varray <- NULL
  vlat <- 0
  vlng <- 0
  
  while(vindex < vlen){
    vb <- NULL
    vshift <- 0
    vresult <- 0
    repeat{
      if(vindex + 1 <= vlen){
        vindex <- vindex + 1
        vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63  
      }
      
      vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
      vshift <- vshift + 5
      if(vb < 32) break
    }
    
    dlat <- ifelse(
      bitAnd(vresult, 1)
      , -(bitShiftR(vresult, 1)+1)
      , bitShiftR(vresult, 1)
    )
    vlat <- vlat + dlat
    
    vshift <- 0
    vresult <- 0
    repeat{
      if(vindex + 1 <= vlen) {
        vindex <- vindex+1
        vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63        
      }
      
      vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
      vshift <- vshift + 5
      if(vb < 32) break
    }
    
    dlng <- ifelse(
      bitAnd(vresult, 1)
      , -(bitShiftR(vresult, 1)+1)
      , bitShiftR(vresult, 1)
    )
    vlng <- vlng + dlng
    
    varray <- rbind(varray, c(vlat * 1e-5, vlng * 1e-5))
  }
  coords <- data.frame(varray)
  names(coords) <- c("lat", "lon")
  coords
}

# 

deg2rad <- function(deg) return(deg*pi/180)

gcd.slc <- function(long1, lat1, long2, lat2) {
  # Convert degrees to radians
  long1 <- deg2rad(long1)
  lat1 <- deg2rad(lat1)
  long2 <- deg2rad(long2)
  lat2 <- deg2rad(lat2)
  
  R <- 6371.0002161 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}


#############################
## Shiny UI
#############################

#Initialize input control selection vectors
mapLayers = c("OpenStreetMap.Mapnik"
              ,"OpenStreetMap.BlackAndWhite"
              ,"Stamen.TonerLite"
              ,"Esri.WorldStreetMap"
              , "Stamen.Watercolor"
              )

zipPrecision = c("Very Precise", "Precise", "Semi Precise")


ui <- fluidPage(
  navbarPage("RoutR", id="nav",
  tabPanel("Map",
           div(class="outer",
               
               tags$head(
                 # Include some custom CSS
                 includeCSS("www/styles.css")
               ),
               
               leafletOutput("mymap", height = "100%"),
               
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                             width = 330, height = "auto",
                             
                             h2("Route Explorer"),
                             
                             textInput(inputId = 'origin', label = 'Enter your Origin', value = "", placeholder ="e.g. Times Square"),
                             textInput(inputId = 'destination', label = 'Enter your Destination',  value = "", placeholder ="e.g. The Empire State Building"),
                             fluidRow(
                               column(4, 
                                      actionButton("route", "Route!")
                               ),
                               column(8, 
                                      selectInput("precision", "Level of Location Precision", zipPrecision, selected = zipPrecision[3])
                               )
                             ),
                              tags$style(type='text/css', "#route { width:100%; margin-top: 25px;}"),
                              checkboxInput("randommap", label = "Random Map Layer?", value = FALSE),
                             # selectInput('layer', 'Map Layer', choices = mapLayers, selected = mapLayers[1]),
                             
                             
                             htmlOutput("route_count"),
                             plotOutput("boxtrips", height = 200),
                             plotOutput("pricedist", height = 200)
                        
                        
               ),
               
          
               tags$div(id="cite",
                        'Data compiled from ', tags$em('NYC FOIL'), ' by James, Greg & Drew'
               )
           )
  ),
  
  tabPanel("Data Explorer",
           downloadButton('downloadData', 'Download Route Data'),
           tags$hr(), 
           DT::dataTableOutput("triptable")
          ),
  tabPanel("About",
           titlePanel("About RoutR"),
           sidebarLayout(
             sidebarPanel(
               p("RoutR leverages NYC open data to help people in NYC find and evaluate optimal transportation options given a start and end destination. You may clone it's public source code from:"),
               code('https://github.com/gregce/RoutR'),
               br(),
               br(),
               img(src = "routr.png", height = 25, width = 25),
               "RoutR is a product of ", a("MIDS", 
                                          href = "https://datascience.berkeley.edu/")
             ),
             mainPanel(h2("Team"),
               p("RoutR was made using ", a("Shiny", 
                                                 href = "http://www.rstudio.com/shiny"), "by the following three fine fellows:"),
               fluidRow(
                 column(3, align="center",
                        img(src = "greg.png", height = 175, width = 175),
                        br(),
                        a("Greg Ceccarelli",href = "https://www.linkedin.com/in/gregceccarelli")
                 ),
                 column(3, align="center",
                        img(src = "drew.png", height = 175, width = 175),
                        br(),
                        a("Drew Plant",href = "https://www.linkedin.com/in/drewplant")
                 ),
                 column(3, align="center",
                        img(src = "james.png", height = 175, width = 175),
                        br(),
                        a("James Gray",href = "https://www.linkedin.com/in/jamesgray")
                 ),
                 column(3)),
            
               h2("Features & Datasources"),
               tags$ul(
                 tags$li(strong("Implements:")),
                    tags$ul(
                      tags$li("A reactive leaftlet based mapping layer which automatically geocodes user input changes"),
                      tags$li("A floating route explorer with integrated ggplot2 graphics"),
                      tags$li("A dynamic data explorer which reveals the data corresponding to the currently plotted route")
                    ),
                br(),
                 tags$li(strong("Features a", em("significantly downsourced set of data"), " sourced from")),
                 tags$ul(
                   tags$li("Records of yellow & green trips provided by ", a("The New York Taxi & Limosine Corporation", href = "http://www.nyc.gov/html/tlc/html/passenger/records.shtml")),
                   tags$li("Records of Uber pickups provided through the ", a("Freedom of Information Law", href = "https://github.com/fivethirtyeight/uber-tlc-foil-response")),
                   tags$li("Records of NYC bike rentals provided by", a("Citibike", href = "https://www.citibikenyc.com/system-data"))
                 )
                 
               ),
               h3("Acknowledgements"),
               p("The team would like to thank", a("Tood W. Schneider",href = "http://toddwschneider.com/"), "for providing ample inspiration on his blog as well as a ", a("github", href= "https://github.com/toddwschneider/nyc-taxi-data")
                 ,"repo with scripts that made the data acqusition process fairly painless")
             )
           )
  )
)
)

#############################
## Load data for Server
#############################

load("data/alltrips.rda")

#############################
## Shiny Server
#############################

server <- function(input, output) {
  
  ## Grab list of geocoded information once in a reactive expression
  origin <- eventReactive(input$route, {geocode(input$origin, output = "all")})
  destination<- eventReactive(input$route, {geocode(input$destination, output = "all")})
  
  mapdist <- eventReactive(input$route, {round(ggmap::mapdist(input$origin, input$destination)$miles,2)})
  
  # Create formatted addresses for popup
  origin.fa <- reactive({paste0(sep = "",  "<b>Origin</b><br>"
                                , origin()$results[[1]]$formatted_address
                                , "<br><br><b>Trip Stats:</b><br>"
                                , "<b>Average Duration (Mins): </b>", round(mean(t()$trip_duration_seconds)/60,2), "<br>"
                                , "<b>Average Fare Cost ($): </b>", round(mean(t()$total_amount),2),"<br>"                 
                                , "<b>Average Number of Passengers: </b>", round(mean(t()$passenger_count),2),"<br>" 
                                , "<b>Distance from Origin to Destination (mi): </b>", mapdist() , "<br>"  
                                , "<br>"
                                # , "<a href='23.23.71.61:3838/routr/#tab-9677-2'>Explore the data!</a>"
                                # , "<a href='http://127.0.0.1:3728#tab-5529-2'>Explore the data!</a>" 
                                )})
  
  
  destination.fa <- reactive({paste0(sep = ""
                                     ,  "<b>Destination</b><br>", destination()$results[[1]]$formatted_address
                                     , "<br><br><b>Trip Stats:</b><br>"
                                     , "<b>Average Duration (Mins): </b>", round(mean(t()$trip_duration_seconds)/60,2), "<br>"
                                     , "<b>Average Fare Cost ($): </b>", round(mean(t()$total_amount),2),"<br>"                 
                                     , "<b>Average Number of Passengers: </b>", round(mean(t()$passenger_count),2),"<br>" 
                                     , "<b>Distance from Destination to Origin (mi): </b>", mapdist(), "<br>" 
                                     , "<br>" 
                                     # , "<a href='23.23.71.61:3838/routr/#tab-9677-2'>Explore the data!</a>"
                                     # , "<a href='http://127.0.0.1:3728#tab-5529-2'>Explore the data!</a>" 
                                     )})
  
  # Create and round lat long formatting based on selected input precsision
  precision <- reactive({as.numeric(as.character(cases(
    '50'=input$precision==zipPrecision[1],
    '100'=input$precision==zipPrecision[2],
    '200'=input$precision==zipPrecision[3])))})
  
  # Grab Lat/Long pairs 
  
  origin.long <- reactive({as.numeric(origin()$results[[1]]$geometry$location$lng)})
  origin.lat <- reactive({as.numeric(origin()$results[[1]]$geometry$location$lat)})
  
  destination.long <- reactive({as.numeric(destination()$results[[1]]$geometry$location$lng)})
  destination.lat <- reactive({as.numeric(destination()$results[[1]]$geometry$location$lat)})
  
  # Filter Data Set Based on Precision Selected By User
  
  t<- reactive({
    validate(
      need(!is.na(origin()), 'Please enter a valid NY address'),
      need(!is.na(destination()), 'Please enter a valid NY address')
    )
    
    p <- precision()
    olo <- origin.long()
    ola <- origin.lat()
    dlo <- destination.long()
    dla <- destination.lat()
    
    t <- alltrips %>%
      mutate(pickup_distance_away = gcd.slc(olo, ola, pickup_longitude, pickup_latitude)
            , dropoff_distance_away = gcd.slc(dlo, dla, dropoff_longitude, dropoff_latitude)
            ) %>%
      filter(pickup_distance_away <= p/1000, dropoff_distance_away <= p/1000) %>%
      select(type, vehicle_type, start_time, stop_time, trip_duration_seconds, passenger_count, trip_distance, total_amount)
    t
  })

  #Create route data frame based on user's input output pairs that updates and maps plots to be rendered on the map

  route_df <- eventReactive(input$route, {decodeLine(route(input$origin, input$destination, structure = "route",
                                         output = "all")$routes[[1]]$overview_polyline$points)})
  
  # Select a random map layer when a new route is chosen
  randomLayer <- reactive({if (isTRUE(input$randommap)) {
    sample(mapLayers,1)
  } else
  {
    mapLayers[1]
  }})

  # Beginning of Outputs
  
  # check state to render intial map
  observe({
  if (input$route == 0) {
    output$mymap <- renderLeaflet({
      leaflet() %>%
        setView(lng = -73.985428, lat = 40.748817, zoom = 14) %>%
        addProviderTiles(mapLayers[1], options = providerTileOptions(noWrap = TRUE))
    })
    
  # rending plotted map
    } else {
      validate(
        need(!is.na(origin()), 'Please enter a valid NY address'),
        need(!is.na(destination()), 'Please enter a valid NY address')
      )
      output$mymap <- renderLeaflet({
        leaflet() %>%
          setView(lng = -73.985428, lat = 40.748817, zoom = 10) %>%
          addProviderTiles(randomLayer(), options = providerTileOptions(noWrap = TRUE)) %>%
          addPolylines(route_df()$lon, route_df()$lat, fill = FALSE) %>%
          addMarkers(route_df()$lon[1], route_df()$lat[1], popup = origin.fa()) %>%
          addMarkers(route_df()$lon[length(route_df()$lon)], route_df()$lat[length(route_df()$lon)]
                     ,popup = destination.fa()) %>%
          addCircles(route_df()$lon[1], route_df()$lat[1], radius = precision()) %>%
          addCircles(route_df()$lon[length(route_df()$lon)], route_df()$lat[length(route_df()$lon)], radius = precision()) %>%
          fitBounds(lng1 = max(route_df()$lon),lat1 = max(route_df()$lat),
                    lng2 = min(route_df()$lon),lat2 = min(route_df()$lat))
      })}
  })
  
  observe({
    if (is.na(origin()) | is.na(destination())) {
      output$mymap <- renderLeaflet({
        leaflet() %>%
          setView(lng = -73.985428, lat = 40.748817, zoom = 14) %>%
          addProviderTiles(mapLayers[1], options = providerTileOptions(noWrap = TRUE))
      })
    }
  })
  
  
  output$route_count <- renderUI({
    if (nrow(t()) == 0)
      return(NULL)
    
    HTML(paste("<b>Number of Trips:</b> ",nrow(t()),"<br><br/>"))})
  
  output$boxtrips <- renderPlot({
    # If no trips are in view, don't plot
    if (nrow(t()) == 0)
      return(NULL)

    ggplot(t(), aes(x=vehicle_type, y=(trip_duration_seconds/60), color=vehicle_type)) + geom_boxplot() +
      ylab("Trip Duration (Min)") + xlab("Mode of Transport") + guides(fill=FALSE) + theme(legend.position="none")
    
  })
  
  output$pricedist <- renderPlot({
    # If no trips are in view, don't plot
    if (nrow(t()) == 0)
      return(NULL)
 
  ggplot(t(), aes(x=total_amount, colour=vehicle_type)) +
    geom_density() +
    geom_vline(data=t(), aes(xintercept=mean(total_amount),  colour=vehicle_type),
               linetype="dashed", size=1) + theme(legend.position="none") + xlab("Cost ($)") + ylab("Fraction of Trips")
  })
  
  output$triptable <- DT::renderDataTable({
    DT::datatable(t(), escape = FALSE)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste('route_dataset', '.csv', sep='') },
    content = function(file) {
      write.csv(t(), file)
    }
  )
}

#############################
## Run App
#############################

shinyApp(ui = ui, server = server)
