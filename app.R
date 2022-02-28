# StationCard 1.0
# Created by Gabriel Watson - watson@chesapeakecommons.org
# Git https://github.com/ChesapeakeCommons/StationCard
# Created 06.32.2021

suppressPackageStartupMessages({
library(shiny)
library(httr)
library(jsonlite)
library(leaflet)
library(dplyr)
library(plotly)
library(lubridate)
library(sf)
library(shinyjs)
library(ggplot2)
library(shinycssloaders)
library(shinyjqui)
library(sp)
library(rgeos)
library(stringr)
library(utils)
})



####### UI SIDE #######
## Sourcing css jquery and html styling ##

ui <- fluidPage(
  #css files are located in ./www/css/
  theme = "./css/styler_w1025.css",
  
  #this will load jQuery for us
  shinyjs::useShinyjs(),
  tags$head(HTML('<meta property="og:title" content="The Rock"/>')),
 
  
  #import html templates to <body> of shiny-made html doc.
  #css files are located in ./www/html/
  #Adding all visual components
  htmlTemplate("./www/html/main.htm",
               groupName       = textOutput("GroupName"),
               groupLogo       = uiOutput("GroupLogo"),
               map             = leafletOutput("Map"),
               statusDot       = uiOutput("StatusDot"),
               statusText      = uiOutput("StatusText"),
               sampleText      = uiOutput("SampleText"),
               parameterSelect = uiOutput("ParameterSelect"),
               prevStationLink = uiOutput("PrevStationLink"),
               nextStationLink = uiOutput("NextStationLink"),
               stationNameText = uiOutput("StationNameText"),
               latLong         = uiOutput("LatLong"),
               watershedName   = uiOutput("WatershedName"),
               text            = uiOutput("Text"),
               MoreInfoLink    = actionLink("MoreInfoLink", HTML('<img id="qr-icon" src = "./svg/qr_code_2_wrblue_24dp.svg" alt="QR Code"/>'),),
               v_tabsetPanel   = tabsetPanel(
                 tabPanel("Latest Measurement",uiOutput("GaugeTitle"), plotlyOutput("GaugePlot") %>% withSpinner()),
                 tabPanel("Trends",uiOutput("TrendsTitle"),plotOutput("TrendsPlot", click = "plot_click")%>% withSpinner())
               )               
  ),
  
  #Add js files to <head> of shiny-made html doc.
  #css files are located in ./www/js/
  #but may want to group html, js and css by componant and not script type
  
  tags$head(
    tags$script(src = "./js/stationCard.js", type="text/javascript"),
  ),
  tags$head(
    tags$script(src = "./js/mapExpand.js", type="text/javascript"),
  ),
  tags$head(
    tags$script(src = "./js/loading.js", type="text/javascript"),
  ),
  tags$head(
    tags$meta(name='viewport', content='width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0' ),
    
  ),
  
  tags$script(HTML("viewFunctions('heightSetter');"), type='text/javascript'),
  tags$script(HTML("viewFunctions('hideContent');"), type='text/javascript')
  

) 



### SERVER SIDE ### 
server <- function(input, output,session) {
  ###### ###### ###### ###### ###### ####
  ###### IMPORTS AND VAR DECLRATIONS ####
  ###### ###### ###### ###### ###### ####    
  
  #API Token 
  
  Token <- "YourTokenGoesHere"
  
  #Data Reactive 
  DatasetDataReactive <- reactiveValues(df = data.frame())
  StationDataReactive <- reactiveValues(df = data.frame())
  StationListDataReactive <- reactiveValues(df = data.frame())
  SelectedStation <- reactiveValues(X = NULL)
  SelectedStation2 <- reactiveValues(X = NULL)
  ParametersReactive <- reactiveValues(df = data.frame())
  SelectedParameter <- reactiveValues(X = NULL)
  SelectedDataset <- reactiveValues(X = NULL)
  UserLocation <- reactiveValues(df = data.frame())
  URL <- reactiveValues(X = "")
  epsg.32721 <- "+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs"
  norm_vec <- function(x) sqrt(sum(x^2))  

  #### We need to pin this support function at the top for sequencing ###
  #### Function for Finding Station Order Along Stream Segments  ####  
  OrderPointsAlongLine <- function(spatialLinesObject, spatialPointsObject) {
    lineCoords = as.data.frame(coordinates(spatialLinesObject))
    lineCoordsLength = nrow(lineCoords)
    
    # Make a buffer 
    spatialPointsObjectBuffer = gBuffer(spgeom = spatialPointsObject, width = 1, byid = TRUE)
    
    # Aux list and data frame
    cellsAndDistances = list()
    cd = data.frame('cell' = NA, 'dist' = NA, 'id' = NA)
    
    # Go sequentially through every line segment in the SpatialLine object looking for which SpatialPoints are intersected
    for(i in 1:(lineCoordsLength - 1)) {
      
      # Unommented: show iteration number
      # cat(paste("\n iteration", i, sep = ""))
      
      # Line segment points
      p0 = lineCoords[i,]
      p1 = lineCoords[i+1,]
      
      # Build line segment
      SL = SpatialLines(LinesList = list(Lines(Line(coords = rbind(p0,p1)), ID = paste(i, sep = ""))), proj4string = CRS(epsg.32721))
      
      # Find intersected points with line segment
      intersect = which(gIntersects(spgeom1 = spatialPointsObjectBuffer, spgeom2 = SL, byid = TRUE))
      
      # Points intersected
      p2 = spatialPointsObject[intersect,]
      
      p2Length = nrow(p2@coords)
      
      # If there is almost one point intersected
      if(p2Length != 0) {
        
        # If there are more than one intersected points
        for(j in 1:p2Length) {
          
          # Commented: show sub-iteration number
          # cat(paste("\n sub-iteration", j, sep = ""))
          cd[j,] = data.frame('cell' = intersect[j], 'dist' = norm_vec(coordinates(p2)[j,] - p0))
          cd$id[j] <- p2$id[j]
        }
      }
      # Order by distancee
      data.table::setorder(cd, dist) 
      cellsAndDistances[[i]] = cd
    }
    
    # Bind list into one data farme
    cellsAndDistancesBind = do.call("rbind", cellsAndDistances)
    
    # Remove duplicated
    cellsAndDistancesBind <- cellsAndDistancesBind[-which(duplicated(cellsAndDistancesBind$cell)),]
    
    StationOrder <- cellsAndDistancesBind %>%
      select(cell,id)%>%
      rename("Order" = cell)
    
    # Return
    return(StationOrder)
  }
  
  
  ###### ###### ###### ###### ###### ####
  ### END IMPORTS AND VAR DECLARTIONS ###
  ###### ###### ###### ###### ###### ####
  
  
  ###### ###### ######  ###
  ###### URL HANDLING #####
  ###### ###### ######  ###
  
  #### Handling of Key Value Pairs Upon Startup or Non Provided #### 
  observeEvent(getQueryString(),{
    
    # Get URL QueryString
    Query <- getQueryString()
    
    # Parse
    DatasetID <- Query$dataset
    StationID <- Query$station
    ParameterID <- Query$param
    
    
    #### Dataset Handling ######
    ### Calls station Request #### 
    
    ## Check to see if no Dataset
    if(is.null(SelectedDataset$X))
    {
      
      ## !!! COMMENT OUT BEFORE PUBLISHING !!! ###
      # If null, sets to 863 - need this for running on machine 
      # if(!is.null(DatasetID))
      #  {
      #  DatasetID <- as.numeric(DatasetID)
      #  }
      #  else
      #  {
      #   DatasetID <- 863
      #  }
      ## !!! COMMENT OUT BEORE PUBLISHING !!! ###

          SelectedDataset$X <- DatasetID
          DatasetDataReactive$df <- GetDataset(SelectedDataset$X)
          StationListDataReactive$df <- GetStations(DatasetID)
    }

    ### Station Handling ###
    # Check to see if no station
    if(is.null(SelectedStation$X))
    {
      #Startup Parameters request based on first station in list.
      ParametersReactive$df <- GetParameters(SelectedDataset$X,StationListDataReactive$df$id[1])
      
      # If a station has been provided
      if(!is.null(StationID))
      {
        # Gets the Query station ID
        StationID <- as.numeric(StationID)
        
        # If its in the station list, use it 
        if(StationID %in% StationListDataReactive$df$id)
        {
          SelectedStation$X <- StationID
          SelectedStation2$X <- StationID
        }
        # If not in station list, set to the first station and notify user
        else
        {
          showNotification("Not a valid StationID, defaulting to,", StationListDataReactive$df$name[1], type = "warning")
          SelectedStation$X <- StationListDataReactive$df$id[1]
          SelectedStation2$X <- StationListDataReactive$df$id[1]

        }
      }
      # If no station id has been provided, default to the first station
      else
      {
        SelectedStation$X <- StationListDataReactive$df$id[1]
        SelectedStation2$X <- StationListDataReactive$df$id[1]
      }
    }


    ### Parameter Handling ###
    ## Checks to see if no parameter
    if(is.null(SelectedParameter$X))
    {
      # If provided in url
      if(!is.null(ParameterID))
      {
        ParameterID <- as.numeric(ParameterID)
        
        # If its in the parameters list
        if(ParameterID %in% ParametersReactive$df$id)
        {
          SelectedParameter$X <- ParameterID
        }
        # If not in parameter list
        else
        {
          showNotification("Not a valid ParameterID, defaulting to,", ParametersReactive$df$name[1],  type = "warning")
          SelectedParameter$X <- ParametersReactive$df$id[1]
        }
      }
      # If not provided in URL
      else
      {
        SelectedParameter$X <- ParametersReactive$df$id[1]
      }
    }

    ## Updating a combined URL for the QR Codes
    URL$X <- paste0(session$clientData$url_hostname,session$clientData$url_pathname,session$clientData$url_search)
    
  })

  
  ###### ###### ######  #######
  ###### END URL HANDLING #####
  ###### ###### ######  #######
  
  
  
  ###### ###### ######  ###
  ###### API FUNCTIONS ####
  ###### ###### ######  ###
  
  ## GET DATASET ##
  ## Returns station data, and a dataset confirmation id
  GetDataset <- function(DatasetID)
  {
    Request <- httr::GET(paste0("https://api.waterreporter.org/datasets/",DatasetID,"?access_token=",Token), httr::config(ssl_verifypeer = 0L))   

    # Checking to see if requests is success 
    if(status_code(Request) == 200)
    {
      JsonResponseText <- content(Request,as="text")
      ParsedJSON <- fromJSON(JsonResponseText)
      
      # Checks to see if there is data 
      if(!is.null(ParsedJSON$parameter_count) && !is.null(ParsedJSON$sample_count))
      {
        JSONResponse <- data.frame(ParsedJSON$organization)
        DatasetData <- data.frame(JSONResponse$name,JSONResponse$logo_url)
        
       
        #Cleaning Var Names 
        colnames(DatasetData) <- gsub('JSONResponse.','',colnames(DatasetData))
        return(DatasetData)
      }
      #Notification for no data
      else
      {
        showNotification("Error - No Data. Please enter a valid dataset in the URL. Go to <WaterReporterHelpLink.org> for assistance", type = "error", duration = NA)
      }
    }
    #Notification for no data
    else
    {
      showNotification("Error - Invalid Dataset ID. Please enter a valid dataset in the URL. Go to <WaterReporterHelpLink.org> for assistance", type = "error", duration = NA)
    }
    
  
  ## GET STATIONS ##
  ## takes DatasetIDm, returns station list
  }
  GetStations <- function(DatasetID)
  { 
    Request <- GET(paste0("https://api.waterreporter.org/stations?sets=",DatasetID,"&geo_format=xy&access_token=",Token), httr::config(ssl_verifypeer = 0L))    
    
    ## Converting to Dataframe ## 
    JsonResponseText <- content(Request,as="text")
    ParsedJSON <- fromJSON(JsonResponseText)
    JSONResponse <- data.frame(ParsedJSON$features)
    
    ## Getting the columns we need ##
    Stations <- data.frame(JSONResponse$raw_id,JSONResponse$name,JSONResponse$id,JSONResponse$lat,JSONResponse$lng,JSONResponse$description,JSONResponse$huc_12,JSONResponse$reading_count)%>%
      mutate(across(where(is.factor),as.character))
    
    #Cleaning Var Names 
    colnames(Stations) <- gsub('JSONResponse.','',colnames(Stations))
    
    Stations <- Stations %>%
      mutate(id = as.character(id))%>%
      filter(!is.na(reading_count))
    
    #Filtering out stations with no data 
    
    ## Only runs ordering when there is more than one station
    if(nrow(Stations) > 1)
    {
    ###### Adding River Order #### 
    ### Creating a spatial points object 
    spatialPointsObject <- SpatialPointsDataFrame(coords = cbind(Stations$lng,Stations$lat), data = Stations,  proj4string = CRS(epsg.32721))
    
    # Create SpatialLines objects from SpatialPoints
    tmp <- spatialPointsObject@coords
    tmp <- as.data.frame(tmp)
    tmp <- tmp[order(tmp$coords.x1),]
    
    # Drawing lines between points 
    spatialPointsObject@coords <- as.matrix(tmp)
    line  <- Line(coords = spatialPointsObject@coords)
    lines <- Lines(slinelist = line, ID = "X")
    
    ## Creating spatial object 
    spatialLinesObject <- SpatialLines(LinesList = list(lines), proj4string = CRS(epsg.32721))
    
    # Calling OrderPointsAlong
    orderedPoints <- OrderPointsAlongLine(spatialLinesObject = spatialLinesObject, spatialPointsObject = spatialPointsObject)
    
    # Joining 
    OrderedPointsFrame  <- data.frame(coordinates(spatialPointsObject[orderedPoints$Order,]))
    
    OrderedPointsFrame <- data.frame(rownames(OrderedPointsFrame))
    
    OrderedPointsFrameID <- tibble::rowid_to_column(OrderedPointsFrame, "Order")%>%
      rename("OGRowID" = rownames.OrderedPointsFrame.)%>%
      mutate(OGRowID = as.numeric(as.character(OGRowID)))
    
    Stations <- tibble::rowid_to_column(Stations, "OGRowID")
    
    Stations <- left_join(Stations,OrderedPointsFrameID)
    }
    # Dummy order for when there is only one station
    else
    {
    Stations <- Stations %>%
                mutate(Order = 1)
    }
    return(Stations)
    
  }
  

  ## GET PARAMETERS ##
  ## Takes dataset id, and station id, returns list of parameters
  GetParameters <- function(DatasetID,StationID)
  {
    
    Request <- GET(paste0("https://api.waterreporter.org/parameters?dataset_id=",DatasetID,"&station_id=",StationID,"&access_token=",Token), httr::config(ssl_verifypeer = 0L))
    JsonResponseText <- content(Request,as="text")
    ParsedJSON <- fromJSON(JsonResponseText)
    JSONResponse <- data.frame(ParsedJSON$features)
    
    ## Getting the columns we need
    Parameters <- data.frame(JSONResponse$name,JSONResponse$id,JSONResponse$unit,JSONResponse$sample_count)%>%
      mutate(across(where(is.factor),as.character))
    
    #Cleaning Var names
    colnames(Parameters) <- gsub('JSONResponse.','',colnames(Parameters))
    
    #Removing pesky '(unit)' info when groups incorrectly put unit names in parameters
    Parameters <- Parameters %>%
      mutate(name = str_trim(gsub("\\(.*","",name),side = c("right")))
    
    ## Selecting distinct Parameter Names to avoid crashing.
    Parameters <- Parameters %>%
      filter(!is.na(sample_count))%>%
      distinct(name, .keep_all = TRUE)
    
    return(Parameters)
  }
  #### END PARAMETERS #### 
  
  
  
  #### GET READINGS ### 
  ## takes a Station ID and Parameter ID, returns readings
  GetReadings <- function(StationID,ParameterID)
  {
    Request <- GET(paste0("https://api.waterreporter.org/readings?station_id=",StationID,"&parameter_id=",ParameterID,"&limit=50&label=1&include_graph=0&access_token=",Token), httr::config(ssl_verifypeer = 0L))

    JsonResponseText <- content(Request,as="text")
    ParsedJSON <- fromJSON(JsonResponseText)
    JSONResponse <- data.frame(ParsedJSON$data)
    


    # Check if no data 
    if(nrow(JSONResponse) != 0)
    {
      #Check if no Color set to grey
      if(is.null(JSONResponse$color))
      {
        JSONResponse$color <- "#777777"
      }
      else
      {
      # If Color is missing in a value, change to grey
      JSONResponse <- JSONResponse %>%
                      rowwise() %>%
                      mutate(color = ifelse(is.null(color) || is.na(color),"#777777", color))
      }
    
      
      #Check if no label, set to X and handle in render       
      if(is.null(JSONResponse$label))
      {
        JSONResponse$label <- "X"
      }
      else
      {
        #If label is missing in a value, change to X and handle in render 
        JSONResponse <- JSONResponse %>%
                        rowwise() %>%
                        mutate(label = ifelse(is.null(label) || is.na(label),"X", label))
      }
      
      
      
        Readings <- data.frame(JSONResponse$collection_date,JSONResponse$value,JSONResponse$color,JSONResponse$label)%>%
        mutate(across(where(is.factor),as.character))
        colnames(Readings) <- gsub('JSONResponse.','',colnames(Readings))
    }
    #No Data handling
    else
    {
      Readings <- data.frame(matrix(ncol = 3, nrow = 1))
      headers <- c("collection_date","value","color")
      colnames(Readings) <- headers
      Readings$color <- "#ededee"
    }
    
    #Mutating the date to posixct here 
    Readings <- Readings %>%
      mutate(collection_date = as.POSIXct(collection_date, origin = "1970-01-01", tz = "UTC"))
    

    return(Readings)
  }
  #### END READINGS #### 
  
  ###### ###### ######  ###
  ##### END API FUNCTIONS #
  ###### ###### ######  ###
  
  
  
  
  ###### ###### ######  ##########
  ##### OBSERVE EVENT  FUNCTIONS #
  ###### ###### ######  ##########
  
  ### UPDATING KEY VALUE PAIR (QUERY) WHEN ANY INPUTS CHANGE 
  observe({
    QueryString <- paste0("?dataset=",SelectedDataset$X,"&station=",SelectedStation$X,"&parameter=",SelectedParameter$X)
    updateQueryString(QueryString, mode = "push")
  })
  
  # OBSERVING MAP MARKER CLICK AND CONVERTING TO THE NEW STATION
  observeEvent(input$Map_marker_click$id,ignoreInit = TRUE,{
    SelectedStation2$X <- input$Map_marker_click$id  
  })
  
  # OBSERVING NEW SELECTED STATION AND REDRAWING THE MAP ### 
  observeEvent(SelectedStation2$X,ignoreInit = TRUE,{
    ParametersReactive$df <- GetParameters(SelectedDataset$X,SelectedStation2$X)
  
    
    OldSelectedStation <- StationListDataReactive$df %>% 
      filter(id == SelectedStation$X)
    
    NewSelectedStation <- StationListDataReactive$df %>%
      filter(id == SelectedStation2$X)
    
    # Calling for new data
    StationDataReactive$df <- GetReadings(SelectedStation2$X,SelectedParameter$X)
    
    # Updating Map
    leafletProxy("Map")%>%
      removeMarker(layerId = SelectedStation$X)%>%
      removeMarker(layerId = SelectedStation2$X)%>%
      addCircleMarkers(data = OldSelectedStation, 
                       lng = ~lng, 
                       lat = ~lat, 
                       layerId = ~id, 
                       label = ~name, 
                       fillColor = "#4fc3f7", 
                       fillOpacity = 1, 
                       radius = 5, 
                       stroke = TRUE,
                       color =  "#4fc3f7",
                       weight = 20,
                       opacity = 0.5,
      )%>%
      addCircleMarkers(data = NewSelectedStation, 
                       lng = ~lng, 
                       lat = ~lat, 
                       layerId = ~id, 
                       label = ~name, 
                       fillColor = StationDataReactive$df$color[1], 
                       fillOpacity = 1, 
                       radius = 8, 
                       stroke = TRUE,
                       color =  StationDataReactive$df$color[1], 
                       weight = 20,
                       opacity = 0.5,
      )%>%
      flyTo(lng = NewSelectedStation$lng, lat = NewSelectedStation$lat, zoom = input$Map_zoom)
    
    
    ## Refreshing, setting the new old station
    SelectedStation$X <- SelectedStation2$X
  })
  
  
  
  ### OBSERVING PARAM SELECT AND CONVERTING TO REACTIVE. ALSO REDRAWING MAP ##
  observeEvent(input$ParamSelect,ignoreInit = TRUE,{
    req(StationDataReactive$df)
    
    ## Getting the selected Parameter
    SelectedParameter$X <- ParametersReactive$df %>%
      filter(name == input$ParamSelect)%>%
      select(id)%>%
      pull()
    
    ## Getting the Selected Station
    NewSelectedStation <- StationListDataReactive$df %>%
      filter(id == SelectedStation2$X)
    
    ## Getting the readings
    StationDataReactive$df <- GetReadings(SelectedStation$X,SelectedParameter$X)
    
    #Converting to static frame to avoid re rendering
    SelectedStationReadings <- StationDataReactive$df
    
    #Redrawing Map
    leafletProxy("Map")%>%
      removeMarker(layerId = SelectedStation2$X)%>%
      addCircleMarkers(data = NewSelectedStation, 
                       lng = ~lng, 
                       lat = ~lat, 
                       layerId = ~id, 
                       label = ~name, 
                       fillColor = SelectedStationReadings$color[1], 
                       fillOpacity = 1, 
                       radius = 8, 
                       stroke = TRUE,
                       color =  StationDataReactive$df$color[1], 
                       weight = 20,
                       opacity = 0.5,
      )
    
  })
  
  ## OBSERVING PREV STATION BUTTON ##
  observeEvent(input$PrevStation,{
    OrderNumber <- StationListDataReactive$df %>%
      filter(id == SelectedStation2$X)%>%
      pull(Order)
    
    #If its the first station go to the end of the list
    if(OrderNumber == 1)
    {
      SelectedStation2$X <- StationListDataReactive$df %>%
        filter(Order == nrow(StationListDataReactive$df))%>%
        pull(id)
    }
    # Otherwise go backwards
    else
    {
      SelectedStation2$X <- StationListDataReactive$df %>%
        filter(Order == OrderNumber - 1)%>%
        pull(id)
    }
  })
  
  
  ## OBSERVING NEXT STATION BUTTON ##
  observeEvent(input$NextStation,{
    OrderNumber <- StationListDataReactive$df %>%
      filter(id == SelectedStation2$X)%>%
      pull(Order)
    
    # Go forwards
    if(OrderNumber < nrow(StationListDataReactive$df))
    {
      SelectedStation2$X <- StationListDataReactive$df %>%
        filter(Order == OrderNumber + 1)%>%
        pull(id)
    }
    else
    # Otherwise go to the front of the list
    {
      SelectedStation2$X <- StationListDataReactive$df %>%
        filter(Order == 1)%>%
        pull(id)
    }
  })
  
  
  
  
  ###### ###### ######  ##############
  ##### END OBSERVE EVENT FUNCTIONS #
  ###### ###### ######  ##############
  
  
  
  ###### ###### ######  ######## ##
  ##### MAIN UI RENDER FUNCTIONS ##
  ###### ###### ###### ##### ### ##
  
  ### RENDERING PARAMETER SELECT #### 
  output$ParameterSelect <- renderUI({
    
    #Getting the selected parameter name
    SelectedParamName <- ParametersReactive$df %>%
      filter(id == SelectedParameter$X)%>%
      select(name)%>%
      pull()
    
    selectInput("ParamSelect","", choices = ParametersReactive$df$name, selected = SelectedParamName)
  })

  
  ### RENDING MAP ###
  output$Map <- renderLeaflet({
    
    ## Handling for if there is only one station, setting the non selected stations to the selected station and ...
    # then writing over in the leaflet call
    if(nrow(StationListDataReactive$df) > 1)
    {
    NonSelectedStations <- StationListDataReactive$df %>%
    filter(id != isolate(SelectedStation$X))
    }
    else
    {
    NonSelectedStations <- StationListDataReactive$df %>%
    filter(id == isolate(SelectedStation$X))
    }
    
    # Setting correct dataframe
    SelectedStation <- StationListDataReactive$df %>%
      filter(id == isolate(SelectedStation$X))
    
    # Isolating to prevent re rendering
    SelectedStationReadings <- isolate(StationDataReactive$df)
    
    # Drawing initial Map
    leaflet("Map")%>%
      setView(lng = SelectedStation$lng, lat = SelectedStation$lat, zoom = 11)%>%
      addProviderTiles("CartoDB.VoyagerLabelsUnder", group = "Streets")%>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite")%>%  
      addTiles(urlTemplate = "", attribution = HTML('<a href="https://ourcommoncode.org">Powered by The Commons</a>'))%>%
      addCircleMarkers(data = NonSelectedStations, 
                       lng = ~lng, 
                       lat = ~lat, 
                       layerId = ~id, 
                       label = ~name, 
                       fillColor = "#4fc3f7", 
                       fillOpacity = 1,
                       radius = 5,
                       stroke = TRUE,
                       color =  "#4fc3f7", 
                       weight = 20,
                       opacity = 0.5
      )%>%
      addCircleMarkers(data = SelectedStation, 
                       lng = ~lng, 
                       lat = ~lat, 
                       layerId = ~id, 
                       label = ~name, 
                       fillColor = SelectedStationReadings$color[1], 
                       fillOpacity = 1, 
                       radius = 8, 
                       stroke = TRUE,
                       color =  SelectedStationReadings$color[1],
                       weight = 20,
                       opacity = 0.5
      )%>%
      addLayersControl(
        baseGroups = c("Streets","Satellite"),
        position =c("bottomleft"), 
        options = layersControlOptions(collapsed = FALSE)
        
      )%>%
      
      htmlwidgets::onRender(paste("
      function(el,x) {
      
        map = this;
        
        viewFunctions('showContent');
        
        viewFunctions('moveQRLink');
        
        viewFunctions('setMapLoadFlag');
        
        viewFunctions('paramSelectObjectAdjuster');
        
      //  viewFunctions('enableDropDown');
        
         viewFunctions('onTick');
         
         viewFunctions('copyShinyLoader');
        
      }"))
    
  })
  
  
  ### GROUP NAME ####
  output$GroupName <- renderText({
    DatasetDataReactive$df$name
  })
  
  ## GROUP LOGO  ##
  output$GroupLogo <- renderUI({
    styled <- HTML('background-image: url(\"',DatasetDataReactive$df$logo_url,'\");')
    tagList(
      div(id='GroupLogoImage', style = styled)
    )
  })
  
  ## NEXT STATION BUTTON ##
  output$NextStationLink <- renderUI({
    actionLink("NextStation",HTML("<div class='station_next'><img id='next' src='./svg/arrow_right_white_24dp.svg' alt='down river'/></div>"))
  })
  
  ## PREVIOUS STATION BUTTON ##
  output$PrevStationLink <- renderUI({
    actionLink("PrevStation",HTML("<div class='station_prev'><img id='prev' src='./svg/arrow_left_white_24dp.svg' alt='up river'/></div>"))
  })
  
  ### STATUS DOT ###
  output$StatusDot <- renderUI({
    Color <- StationDataReactive$df$color[1]
    
    #Rendering
    tagList(
      tags$style(HTML(paste0("#StatusDot {
      width: 40px;
      height: 40px;
      border-radius: 50%;
      border-color: black;
      border-width: thick;
      background:",Color,";
  }"))))
  })
  
  ### STATUS DOT TEXT ###
  output$StatusText <- renderUI({
    req(StationDataReactive$df)

    #Creating sentence, setting to label if no threshold
    if(!is.null(StationDataReactive$df$label[1]) && StationDataReactive$df$label[1] == "X")
    {
      Average <- round(mean(StationDataReactive$df$value),2)
      Text <- ifelse(StationDataReactive$df$value[1] > Average,"Above Average","Below Average")
      Text <- ifelse(StationDataReactive$df$value[1] == Average,"Average",Text)
    }
    else
    {
      Text <- StationDataReactive$df$label[1]
    }

    # Rendering
    tagList(
      Text
          )
  })
  
  ## STATUS DOT COLLECTION DATE ##
  output$SampleText <- renderUI({
    req(StationDataReactive$df)
    
    SampleDate <- format(StationDataReactive$df$collection_date[1], format = "%D")
    SampleDateText <- ifelse(is.na(SampleDate),"No Data Available",paste("",SampleDate))
  })
  
  ## WATERSHED NAME ##
  output$WatershedName <- renderUI({
    
    Watershed <- StationListDataReactive$df %>%
      filter(id == SelectedStation$X)%>%
      select(huc_12)%>%
      pull()
    
    tagList(
      Watershed <- paste(Watershed,"Watershed."),
    )
  })
  
  ## LATITUDE LONGITUDE ##
  output$LatLong <- renderUI({
    Long <- StationListDataReactive$df %>%
      filter(id == SelectedStation$X) %>%
      select(lng)%>%
      pull()
    
    Lat <- StationListDataReactive$df %>%
      filter(id == SelectedStation$X) %>%
      select(lat)%>%
      pull()
    
    tagList(
      LatLong <- HTML("",
                      "<div id='#LatLong'>Lat:",Lat,"Long:",Long,"</div>"
      ),
    )
  })
  
  ## STATION NAME TEXT ##
  output$StationNameText <- renderUI({
    StationName <- StationListDataReactive$df %>%
      filter(id == SelectedStation$X) %>%
      select(name)%>%
      pull()
    
    tagList(
      StationName,
    )
  })
  
  
  #### LONG FORM SAMPLE TEXT#### 
  output$Text <- renderUI({
    req(input$ParamSelect)
    req(StationDataReactive$df)
   
    SampleDate <- format(StationDataReactive$df$collection_date[1], format = "%D")
  
    Unit <- ParametersReactive$df %>%
      filter(name == input$ParamSelect)%>%
      select(unit)%>%
      pull()
    
    Unit <- ifelse(is.na(Unit),"",Unit)
    
    EarliestSample <- format(StationDataReactive$df$collection_date[nrow(StationDataReactive$df)], format = "%D")
    
    Average <- round(mean(StationDataReactive$df$value),2)
    AverageText <- ifelse(StationDataReactive$df$value[1] > Average,"above average","below average")
    AverageText <- ifelse(StationDataReactive$df$value[1] == Average,"average",AverageText)
    AverageParen <- paste0("(",Average,").")
    if(abs(Average - StationDataReactive$df$value[1]) == 0)
    {
      AverageReading <- HTML("The ", input$ParamSelect, " is <b>average ",AverageParen,"</b>")
    }
    else
    {
      AverageReading <- HTML("The ", input$ParamSelect, " is<b>", round(abs(Average - StationDataReactive$df$value[1]),2),Unit,AverageText,AverageParen,"</b>")
    }
    
    
    DateRange <- paste0("(",nrow(StationDataReactive$df), " sample(s) between ", EarliestSample, " and ", SampleDate,")")
    
    tagList(
      
      
      
      HTML("<div id='AverageReading_DateRange'>"),HTML(AverageReading),HTML(DateRange), HTML("</div>"),
      
    )
  })
  
  
  #### GAUGE PLOT TITLE #### 
  output$GaugeTitle <- renderUI({
    req(input$ParamSelect)
    if(is.null(StationDataReactive$df$value[1]))
    {
      Text <- "Sorry, No Data Currently Available"  
    }
    else
    {
      Unit <- ParametersReactive$df %>%
        filter(name == input$ParamSelect)%>%
        select(unit)%>%
        pull()
      
      Unit <- ifelse(is.na(Unit),"",paste(" - ",Unit))
      
      Text <- paste0(input$ParamSelect,Unit)
    }
    tagList(
      h4(Text, align = "center")
    )
  })
  
  #### GAUGE PLOT  #### 
  output$GaugePlot <- renderPlotly({
    req(StationDataReactive$df)
    
    if(!is.null(StationDataReactive$df$value[1]))
    {
      ## Max value is the max form the last 5 readings 
      MaxValue <- max(StationDataReactive$df$value, na.rm = TRUE)
      
      ## Rendering
      p <- plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = StationDataReactive$df$value[1],
        type = "indicator",
        mode = "gauge+number",
        gauge = list(
          axis = list(range = list(0,MaxValue)),
          bar = list(color = StationDataReactive$df$color[1]))
      ) 
      
      ## Adjusting size
      p <- p %>% layout(margin = list(l=20,r=30,t=10,b=0)) %>% 
        layout(plot_bgcolor='transparent') %>% 
        layout(paper_bgcolor='transparent')  
      #  
      #will also accept paper_bgcolor='black' or paper_bgcolor='transparent'
    }
    
  })

  ## TRENDS TITLE ##
  output$TrendsTitle <- renderUI({
    req(input$ParamSelect)
   
      if(is.null(StationDataReactive$df$value[1]))
      {
      Text <- "Sorry, No Data Currently Available"  
      }
      else
      {
      Unit <- ParametersReactive$df %>%
        filter(name == input$ParamSelect)%>%
        select(unit)%>%
        pull()
      
      Unit <- ifelse(is.na(Unit),"",paste(Unit))

    
      if(nrow(nearPoints(StationDataReactive$df,input$plot_click, xvar = "collection_date", yvar = "value")) > 0)
      {
      click_value <- nearPoints(StationDataReactive$df,input$plot_click, xvar = "collection_date", yvar = "value")
      Unit <-  paste0(input$ParamSelect, ": ", round(click_value$value[1],2)," ", Unit, " on ", format(as.POSIXct(click_value$collection_date[1], origin = "1970-01-01"), format = "%D"))
      
      }
      else 
      {
      Unit <- paste0(input$ParamSelect, " - ",Unit)
      }
      
      }
    
    tagList(
      h4(Unit, align = "center"),
    )
  })
  
  ## TRENDS PLOT ### 
  output$TrendsPlot <- renderPlot({
    
    if(!is.null(StationDataReactive$df$value[1]))
    {
      Unit <- ParametersReactive$df %>%
        filter(name == input$ParamSelect)%>%
        select(unit)%>%
        pull()
      
      Unit <- ifelse(is.na(Unit),"",Unit)
      
      xlim <- as.POSIXct(c(min(StationDataReactive$df$collection_date),max(StationDataReactive$df$collection_date + days(1))),  origin = "1970-01-01")

      ggplot(data = StationDataReactive$df, aes_string(x=StationDataReactive$df$collection_date,y=StationDataReactive$df$value, stroke = .25))+
        geom_point(shape = 21, fill = StationDataReactive$df$color, color = "black", size = 5)+
        ylim(0,max(StationDataReactive$df$value, na.rm = TRUE))+
        ylab("")+
        scale_x_datetime(limits = xlim)+
        xlab("")+
        theme_linedraw()+
        theme(
      #    panel.background =  element_rect(fill = "transparent"),
     #     plot.background = element_rect(fill = "transparent", color = NA),
          axis.title = element_text(size=15),
          axis.text = element_text(size=15))
       
    }
  })
  
  
  ###### ###### ######  ######## ######
  ##### END MAIN UI RENDER FUNCTIONS ##
  ###### ###### ###### ##### ####### ##
  
  
  ###### ###### ######  ############
  ##### MODAL UI RENDER FUNCTIONS ##
  ###### ###### ###### ##### #### ##
  
  
  ### QR CODE FOR MODAL ## 
  output$QRCode <- renderUI({
    #Need to encode URL before sending to QR code generator
    URLEncoded <- URLencode(URL$X, reserved = TRUE, repeated = FALSE)
    
    tagList(
      tags$img(src = paste0("https://api.qrserver.com/v1/create-qr-code/?size=100x100&color=2e2e2e&charset-source=ISO-8859-1&data=",URLEncoded))
    )
  })
  
  ## GROUP LOGO FOR MODAL ##
  output$GroupLogo_Modal <- renderUI({
    styled <- HTML('background-image: url(\"',DatasetDataReactive$df$logo_url,'\");')
    tagList(
      div(id='GroupLogoImage_Modal', style = styled)
    )
  })
  
  ## GROUP NAME FOR MODAL ##
  output$GroupName_Modal <- renderText({
    DatasetDataReactive$df$name 
  })
  
  ## WATERSHED NAME FOR MODAL ##
  output$WatershedName_Modal <- renderUI({
    Watershed <- StationListDataReactive$df %>%
      filter(id == SelectedStation$X)%>%
      select(huc_12)%>%
      pull()
    tagList(
      Watershed <- paste(Watershed,"Watershed."),
    )
  })
  
  ## PARAMETER TITLE FOR MODAL ##
  output$ParameterTitle_Modal <- renderUI({
    req(input$ParamSelect)
    if(is.na(StationDataReactive$df$value[1]))
    {
      Text <- "Sorry, No Data Currently Available"  
    }
    else
    {
      Unit <- ParametersReactive$df %>%
        filter(name == input$ParamSelect)%>%
        select(unit)%>%
        pull()
      
      Unit <- ifelse(is.na(Unit),"",paste(" - ",Unit))
      
      Text <- paste0(input$ParamSelect,Unit)
    }
    tagList(
      h4(Text, align = "center")
    )
  })
  
  ## STATION NAME TEXT FOR MODAL ##
  output$StationNameText_Modal <- renderUI({
    StationName <- StationListDataReactive$df %>%
      filter(id == SelectedStation$X) %>%
      select(name)%>%
      pull()
    
    tagList(
      StationName,
    )
  })
  
  
  ### More Info Model ### 
  observeEvent(input$MoreInfoLink,{
    showModal(MoreInfoModal)
  })
  
  
  ## MORE INFO MODAL ##
  MoreInfoModal <- modalDialog(
    htmlTemplate("./www/html/modal_content.htm",
                 qrCode          = uiOutput("QRCode")%>%withSpinner(),
                 stationNameText_Modal = uiOutput("StationNameText_Modal"),
                 watershedName_Modal = uiOutput("WatershedName_Modal"),
                 groupName_Modal      = uiOutput("GroupName_Modal"),
                 groupLogo_Modal   = uiOutput("GroupLogo_Modal"),
                 parameterTitle_Modal  = uiOutput("ParameterTitle_Modal")
    ),
    easyClose = TRUE,
    footer = NULL,
    size = "s",
  )
  
  
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
    print(".")
  })
  
  ###### ###### ######  ################
  ##### END MODAL UI RENDER FUNCTIONS ##
  ###### ###### ###### ##### #### ######
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)
