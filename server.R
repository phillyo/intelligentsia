##########
# server #
##########

library(shiny)
library(rgdal)
library(leaflet)
library(sp)
library(plotly)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(ggmap)
library(xts)
library(shinyjs)
library(jsonlite)
library(urltools)
library(utils)
library(rvest)
library(stringr)
library(rgeos)
library(xml2)
library(selectr)
library(raster)
library(purrr)
library(RColorBrewer)
library(DT)
library(shinyBS)

#setwd("/Users/philipp/Google Drive/Capstone/App/shiny/intelligentsia")

# variables for testing
#CT <- 36047019500
#CT <- 36061010200
#input <- NULL
#input$addr <- "51 Kent Ave, Brooklyn, NY 11249"
#addr <- data.frame(lat = 40.7223311,
#                   lon = -73.9614495,
#                   GEOID = 36061010200)
#addrSearch <- "120 Willoughby Ave, Brooklyn"


# pretty-print function
format_metric <- function(x, type) {
    switch(type,
           currency = paste0("$", 
                             formatC(round(as.numeric(x), 0), 
                                     big.mark = ",", 
                                     digits = nchar(as.character(round(as.numeric(x), 0)))
                             )
           ),
           real = format(round(as.numeric(x), 1), 
                         nsmall = 1, big.mark = ","),
           int = formatC(as.numeric(x), big.mark = ",", 
                         digits = nchar(as.character(x))),
           year = round(as.numeric(x),0),
           pct = paste0(format(round(as.numeric(x) * 100, 1), 
                               nsmall = 1, big.mark = ","),"%"))
}

shinyServer(function(input, output) {
    
    ####################################################################
    ################------------------------------------################
    #                       neighborhood browser
    ################------------------------------------################
    ####################################################################
    
    ######################################
    # map plot
    ######################################
    
    GeoDFr <- reactive({
        GeoDF[GeoDF$rank <= input$topK,]
    })
    
    geoIDs <- reactive({
        GeoDF <- GeoDF[GeoDF$rank <= input$topK,]
        as.vector(GeoDF$GEOID)
    })
    
    CTlabels <- reactive({
        GeoDF <- GeoDF[GeoDF$rank <= input$topK,]
        sprintf(
            "<strong>CT: %s</strong><br/>Score: %g",
            GeoDF$GEOID, round(GeoDF$bin_20*100),1) %>% lapply(htmltools::HTML)  
    })
    
    # create color scheme for map
    bins <- c(seq(0,1,.2))
    pal <- colorBin("YlOrRd", domain = rev(GeoDF$bin_20), bins = rev(bins))

    output$map <- renderLeaflet({
        
        leaflet(GeoDFr()) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(-73.885343, 40.720403, 11) %>%
            addPolygons(layerId = geoIDs(),
                        weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.8,
                        color = NA,
                        fillColor = ~pal(bin_20),
                        highlightOptions = highlightOptions(
                            color = "white", weight = 2,
                            bringToFront = TRUE),
                        label = CTlabels(),
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal",
                                         padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")) %>% 
            leaflet::addLegend(pal = pal, 
                      values = ~bin_20, 
                      opacity = 0.7, 
                      title = NULL,
                      position = "bottomright")
    })
    
    ######################################
    # observe click events in map and 
    # get neighborhood details
    ######################################
    
    geo <- eventReactive(input$map_shape_click, {
        shinyjs::show("reactiveOutput6")
        shinyjs::show("reactiveOutput5")
        shinyjs::show("reactiveOutput4a1")
        shinyjs::show("reactiveOutput4a")
        shinyjs::show("reactiveOutput4")
        shinyjs::show("reactiveOutput3")
        shinyjs::show("reactiveOutput2a")
        shinyjs::show("reactiveOutput2b")
        shinyjs::show("reactiveOutput2c")
        shinyjs::show("reactiveOutput1")
        shinyjs::removeClass(class = "shinyjs-hide", selector = "hr")
        shinyjs::removeClass(class = "shinyjs-hide", selector = ".kpi-group")
        
        click <- input$map_shape_click
        print(click$id)
        as.numeric(click$id)
    })
    
    addrHood <- reactive({
        
        CT <- geo()
        
        CT <- CTshapes[CTshapes$GEOID == CT,]
        CT <- gCentroid(CT)
        
        # Set the projection of the SpatialPointsDataFrame
        # using the projection of the shapefile
        proj4string(CT) <- proj4string(hoods)
        
        o <- over(CT, hoods)
        print(o)
        o
    })
    
    #############################
    # neighborhood name
    output$hood <- renderText({
        hood <- addrHood()
        hood$ntaname
    })
    
    #############################
    # neighborhood info
    output$hoodInfo <- renderUI({
        n <- addrHood()
        
        g <- paste0("https://www.google.com/search?aq=f&gcx=w&sourceid=chrome&ie=UTF-8&q=wiki+",
                    n$ntaname, "+", str_replace_all(n$boro_name, "-", "+"), "&*") %>% 
            str_replace_all(" ", "+") %>%
            URLdecode()
        
        print(g)
        
        ghtml <- read_html(g)
        
        # get url to wikipedia article from google search
        w <- ghtml %>% html_node(xpath = "//h3[@class='r']/a") %>% 
            html_attr("href") %>%
            str_extract("http.+?(?=&)") %>% 
            URLdecode()
        
        print(w)
        
        whtml <- read_html(w)
        
        wtext <- whtml %>% 
            html_nodes(xpath = "//div[@id='mw-content-text']//p") %>% 
            html_text()
        
        wtext <- wtext[wtext != ""]
        
        wtext %<>% purrr::discard(function(x) substr(x,1,11) == "Coordinates")
        
        HTML(sprintf("<p>%s</p>", wtext[1:2])) %>% 
            paste0(
                HTML(sprintf("<small>Source: <a href='%s' target='_blank'>Wikipedia</a></small>", w)), 
                collapse="") %>% 
            lapply(htmltools::HTML)
    })
    
    #############################
    # zillow
    
    output$zillow1 <- renderPlotly({
        
        p %>% 
            dplyr::filter(GEOID == geo()) %>% 
            plot_ly(x = ~date) %>%
            add_lines(y = ~round(avg.z.sqft,2), 
                      line = list(shape = "linear", color = "#4fbfa8"),
                      name = "Zestimate") %>% 
            layout(xaxis = list(title = "",
                                showgrid = FALSE,
                                ticks = "",
                                fixedrange = TRUE,
                                gridcolor = "#a0a0a0"),
                   yaxis = list(title = "Average Zestimate per sqft",
                                fixedrange = TRUE, zeroline = TRUE, 
                                rangemode = "tozero",
                                ticks = "", gridcolor = "#a0a0a0"),
                   legend = list(x = .5, y = -.1, xanchor = "center",
                                 orientation = "h"),
                   annotations = list(text = "Source: Zillow", align = "left",
                                      x = 0, y = -.15, showarrow = FALSE,
                                      xref = "paper", yref = "paper"),
                   margin = list(b = 50, pad = 3),
                   font = list(family = "'Raleway', sans-serif"),
                   paper_bgcolor = "rgba(1,1,1,0)",
                   plot_bgcolor = "rgba(1,1,1,0)") %>% 
            config(displayModeBar = FALSE)
    })
    
    #############################
    # google search trends
    
    output$google <- renderPlotly({
        
        g %>% 
            dplyr::filter(shapeID == geo()) %>% 
            plot_ly(x = ~year) %>%
            add_bars(y = ~round(queries,0), 
                     type = "bar", marker = list(color = "#436983"),
                     name = "Google Searches") %>% 
            layout(xaxis = list(title = "",
                                showgrid = FALSE,
                                ticks = "",
                                fixedrange = TRUE,
                                gridcolor = "#a0a0a0"),
                   yaxis = list(title = "Search volume",
                                fixedrange = TRUE, zeroline = TRUE, 
                                rangemode = "tozero",
                                ticks = "", gridcolor = "#a0a0a0"),
                   legend = list(x = .5, y = -.1, xanchor = "center",
                                 orientation = "h"),
                   annotations = list(text = "Source: Google", align = "left",
                                      x = 0, y = -.15, showarrow = FALSE,
                                      xref = "paper", yref = "paper"),
                   margin = list(b = 50, pad = 3),
                   font = list(family = "'Raleway', sans-serif"),
                   paper_bgcolor = "rgba(1,1,1,0)",
                   plot_bgcolor = "rgba(1,1,1,0)") %>% 
            config(displayModeBar = FALSE)
    })
    
    #############################
    # wikipedia edits
    
    output$wiki <- renderPlotly({
        
        w %>% 
            dplyr::filter(shapeID == geo()) %>% 
            plot_ly(x = ~year) %>%
            add_bars(y = ~round(edits,0), 
                     type = "bar", marker = list(color = "#436983"),
                     name = "Wikipedia Article Edits") %>% 
            layout(xaxis = list(title = "",
                                showgrid = FALSE,
                                ticks = "",
                                fixedrange = TRUE,
                                gridcolor = "#a0a0a0"),
                   yaxis = list(title = "# of Article Edits",
                                fixedrange = TRUE, zeroline = TRUE, 
                                rangemode = "tozero",
                                ticks = "", gridcolor = "#a0a0a0"),
                   legend = list(x = .5, y = -.1, xanchor = "center",
                                 orientation = "h"),
                   annotations = list(text = "Source: Wikipedia", align = "left",
                                      x = 0, y = -.15, showarrow = FALSE,
                                      xref = "paper", yref = "paper"),
                   margin = list(b = 50, pad = 3),
                   font = list(family = "'Raleway', sans-serif"),
                   paper_bgcolor = "rgba(1,1,1,0)",
                   plot_bgcolor = "rgba(1,1,1,0)") %>% 
            config(displayModeBar = FALSE)
    })
    
    #############################
    # yelp
    
    output$yelp <- renderPlotly({
        yelpByCT %>% dplyr::filter(GEOID == geo()) %>%
            dplyr::arrange(date) %>%
            plot_ly(x = ~date) %>%
            add_lines(y = ~rollingReviews, 
                      line = list(shape = "linear", color = "#4fbfa8"),
                      name = "Reviews") %>% 
            add_lines(y = ~round(avgRating,2), yaxis = "y2",
                      line = list(shape = "linear", color = "#416983"),
                      name = "Rating") %>%
            layout(xaxis = list(title = "",
                                showgrid = FALSE,
                                ticks = "",
                                fixedrange = TRUE,
                                gridcolor = "#a0a0a0"),
                   yaxis = list(title = "# of Yelp reviews",
                                fixedrange = TRUE, zeroline = TRUE, 
                                ticks = "", gridcolor = "#a0a0a0",
                                rangemode = "tozero"),
                   yaxis2 = list(title = "Avg. Rating", side = "right",
                                 fixedrange = TRUE, zeroline = FALSE,
                                 showgrid = TRUE, autorange = TRUE,
                                 rangemode = "tozero", overlaying = "y"),
                   legend = list(x = .5, y = -.1, xanchor = "center",
                                 orientation = "h"),
                   annotations = list(text = "Source: Yelp", align = "left",
                                      x = 0, y = -.3, showarrow = FALSE,
                                      xref = "paper", yref = "paper"),
                   font = list(family = "'Raleway', sans-serif"),
                   margin = list(r = 40, b = 30, pad = 3),
                   paper_bgcolor = "rgba(1,1,1,0)",
                   plot_bgcolor = "rgba(1,1,1,0)") %>% 
            config(displayModeBar = FALSE)
    })
    
    #############################
    # taxitrips
    
    output$taxi <- renderPlotly({
        taxi %>% dplyr::filter(GEOID == geo()) %>%
            dplyr::arrange(date) %>%
            plot_ly(x = ~date) %>%
            add_lines(y = ~rollingTrips, 
                      line = list(shape = "linear", color = "#4fbfa8"),
                      name = "trips") %>%
            layout(xaxis = list(title = "",
                                showgrid = FALSE,
                                ticks = "",
                                fixedrange = TRUE,
                                gridcolor = "#a0a0a0"),
                   yaxis = list(title = "# of taxitrips",
                                fixedrange = TRUE, zeroline = TRUE, 
                                ticks = "", gridcolor = "#a0a0a0",
                                rangemode = "tozero"),
                   legend = list(x = .5, y = -.1, xanchor = "center",
                                 orientation = "h"),
                   annotations = list(text = "Source: NYC Taxi & Limousine Commission", align = "left",
                                      x = 0, y = -.15, showarrow = FALSE,
                                      xref = "paper", yref = "paper"),
                   margin = list(b = 50, pad = 3),
                   font = list(family = "'Raleway', sans-serif"),
                   paper_bgcolor = "rgba(1,1,1,0)",
                   plot_bgcolor = "rgba(1,1,1,0)") %>% 
            config(displayModeBar = FALSE)
    })
    
    #############################
    # donut chart
    
    output$donut <- renderPlotly({
        
        iScore <- round(pred[pred$ct == geo(),"bin_20"] * 100,0)
        
        color <- brewer.pal(9,"YlOrRd")[max(1,floor((iScore-1) / 10))]
        
        lw <- 0
        
        plot_ly(width = 80, height = 80,
                marker = list(colors = c(color, "rgba(0,0,0,0)"),
                              line = list(width = lw, color = "#444"))) %>%
            add_pie(values = c(iScore, 100 - iScore),
                    hole = .7, sort = FALSE, direction = "clockwise",
                    textposition = 'none') %>%
            layout(font = list(family = "'Raleway', sans-serif"),
                   autosize = TRUE,
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   paper_bgcolor = "rgba(1,1,1,0)",
                   plot_bgcolor = "rgba(1,1,1,0)",
                   hovermode = FALSE,
                   showlegend = FALSE,
                   margin = list(l = 0, r = 0, t = 5, b = 0),
                   annotations = list(text = paste0("<b>",iScore,"</b>"),
                                      align = "center",
                                      font = list(size = 28),
                                      showarrow = FALSE)) %>%
            config(displayModeBar = F)
    })
    
    kpis <- reactive({
        CT <- geo()
        pred[pred$ct == CT,]
    })
    
    
    output$kpi1 <- renderText({
        kpis <- kpis()
        format_metric(kpis$MHV, "currency")
    })
    
    output$kpi2 <- renderText({
        kpis <- kpis()
        format_metric(kpis$mhc, "currency")
    })
    
    output$kpi3 <- renderText({
        kpis <- kpis()
        kpis$myb
    })
    
    output$kpi4 <- renderText({
        yelpByCT %>% 
            filter(GEOID == geo()) %>% 
            top_n(12, date) %>% 
            summarise(mean(avgRating)) %>% 
            round(1) %>% 
            format_metric("real")
    })
    
    output$kpi5 <- renderText({
        kpis <- kpis()
        round(kpis$t10walk,1)
    })
    
    output$kpi6 <- renderText({
        kpis <- kpis()
        format_metric(kpis$trees, "int")
    })
    
    output$kpi7 <- renderText({
        kpis <- kpis()
        format_metric(kpis$taxi_share, "pct")
    })
    
    output$kpi8 <- renderText({
        kpis <- kpis()
        format_metric(kpis$pct_all_coll, "pct")
    })
    
    output$kpi9 <- renderText({
        kpis <- kpis()
        kpis$med_tage
    })
    
    output$kpi10 <- renderText({
        kpis <- kpis()
        format_metric(kpis$pct_family, "pct")
    })
    
    output$kpi11 <- renderText({
        kpis <- kpis()
        format_metric(kpis$pct_crime, "pct")
    })
    
    observe({
        kpis <- kpis()
        if (kpis$pct_occup < 0) {
            shinyjs::removeClass("kpi1", "pos-x")
            shinyjs::addClass("kpi1", "neg-x")
        } else {
            shinyjs::removeClass("kpi1", "neg-x")
            shinyjs::addClass("kpi1", "pos-x")
        }
    })
    
    ######################################
    # get properties from zillow
    
    output$propertiesForSale <- renderUI({
        
        CT <- geo()
        CT <- CTshapes[CTshapes$GEOID == CT,]
        CT <- gCentroid(CT)
        
        # Set the projection of the SpatialPointsDataFrame
        # using the projection of the shapefile
        proj4string(CT) <- proj4string(zillowHoods)
        
        o <- over(CT, zillowHoods)
        
        print(o)
        
        hood <- o$NAME
        
        
        n <- paste0("https://www.zillow.com/homes/for_sale/", o$REGIONID, "_rid/")
        
        print(n)
        
        nhtml <- read_html(n)
        
        captions <- nhtml %>% html_nodes(xpath = "//ul[@class='photo-cards']//div[@class='zsg-photo-card-caption']/h4") %>%
            html_text()
        
        if(length(captions) > 0) {
            fc <- captions == "Pre-Foreclosure"
            captions <- captions[!fc]
            
            # get URLs of properties
            urls <- nhtml %>% html_nodes(xpath = "//ul[@class='photo-cards']/li//a[contains(@class,'zsg-photo-card-overlay-link routable')]") %>%
                html_attr("href")
            urls <- urls[!fc]
            
            urls %<>% paste0("http://www.zillow.com", .)
            
            prices <- nhtml %>% html_nodes(xpath = "//ul[@class='photo-cards']//div[@class='zsg-photo-card-caption']//span[@class='zsg-photo-card-price']") %>%
                html_text()
            prices <- prices[!fc]
            
            info <- nhtml %>% html_nodes(xpath = "//ul[@class='photo-cards']//div[@class='zsg-photo-card-caption']//span[@class='zsg-photo-card-info']") %>%
                html_text()
            info <- info[!fc]
            
            imgs <- nhtml %>% html_nodes(xpath = "//ul[@class='photo-cards']//div[@class='zsg-photo-card-img']/img") %>%
                html_attr("src")
            imgs <- imgs[!fc]
            
            properties <- tibble(captions, prices, info, imgs, urls)
            
            properties %<>% dplyr::filter(substr(imgs,1,4) != "data")
            
            propertiesForSale <- sprintf("<a href='%s' target='_blank'>
                                         <figure>
                                         <img src='%s' alt='property image'>
                                         </figure>
                                         <div class='property-info'>
                                         <h4>%s</h4>
                                         <p class='property-price'>%s</p>
                                         <p class='property-details'>%s</p>
                                         </div>
                                         </a>", 
                                         properties$urls,
                                         properties$imgs,
                                         properties$captions,
                                         properties$prices,
                                         properties$info) %>% lapply(htmltools::HTML)
            
            return(propertiesForSale)
        } else {
            print("No Zillow results found.")
            return(htmltools::HTML("<div class='text-center'>
                                   <p>Unfortunately, we could not find any properties for you.</p>
                                   <p>View properties in New York City directly on <a href='https://www.zillow.com/homes/for_sale/New-York-City_rb/' target='_blank'>Zillow</a>.</p>
                                   </div>"))
        }
        })
    
    ####################################################################
    ################------------------------------------################
    #                        location comparison
    ################------------------------------------################
    ####################################################################
    
    ######################################
    # reactive values
    ######################################
    
    observeEvent(input$compare, {
        shinyjs::show("reactiveOutput7a")
        shinyjs::show("reactiveOutput7b")
        shinyjs::show("reactiveOutput8")
        shinyjs::show("reactiveOutput9")
        shinyjs::show("mapControls")
    })
    
    observeEvent(input$searchAddr1, {
        shinyjs::hide("reactiveOutput7a")
    })
    
    observeEvent(input$searchAddr2, {
        shinyjs::hide("reactiveOutput7b")
    })
    
    observe({
        if ((is.null(input$searchAddr1) || input$searchAddr1 == "") ||
            (is.null(input$searchAddr2) || input$searchAddr2 == "")) {
            shinyjs::disable("compare")
        } else {
            shinyjs::enable("compare")
        }
    })
    
    observe({
        if(input$showLayerSchools) {
            leafletProxy("mapLocation1") %>% showGroup("Schools")
            leafletProxy("mapLocation2") %>% showGroup("Schools")
        } else {
            leafletProxy("mapLocation1") %>% hideGroup("Schools")
            leafletProxy("mapLocation2") %>% hideGroup("Schools")
        }
    })
    
    observe({
        if(input$showLayerSubway) {
            leafletProxy("mapLocation1") %>% showGroup("Subway")
            leafletProxy("mapLocation2") %>% showGroup("Subway")
        } else {
            leafletProxy("mapLocation1") %>% hideGroup("Subway")
            leafletProxy("mapLocation2") %>% hideGroup("Subway")
        }
    })
    
    observe({
        if(input$showLayerYelp) {
            leafletProxy("mapLocation1") %>% showGroup("Yelp")
            leafletProxy("mapLocation2") %>% showGroup("Yelp")
        } else {
            leafletProxy("mapLocation1") %>% hideGroup("Yelp")
            leafletProxy("mapLocation2") %>% hideGroup("Yelp")
        }
    })
    
    ######################################
    # define functions
    ######################################
    
    #################
    # zillow graph
    
    zillowSmall <- function(x) {
        p %>% 
            dplyr::filter(GEOID == x$GEOID) %>% 
            plot_ly(x = ~date) %>%
            add_lines(y = ~round(avg.z.sqft,2), 
                      line = list(shape = "linear", color = "#4fbfa8"),
                      name = "Zestimate") %>% 
            layout(xaxis = list(title = "",
                                showgrid = FALSE,
                                ticks = "",
                                fixedrange = TRUE),
                   yaxis = list(title = "", fixedrange = TRUE, 
                                showticklabels = FALSE, showgrid = FALSE,
                                ticks = "", zeroline = TRUE, rangemode = "tozero"),
                   annotations = list(text = "Source: Zillow", align = "left",
                                      x = 0, y = -.5, showarrow = FALSE,
                                      xref = "paper", yref = "paper"),
                   font = list(family = "'Raleway', sans-serif"),
                   margin = list(l = 40, r = 40),
                   paper_bgcolor = "rgba(1,1,1,0)",
                   plot_bgcolor = "rgba(1,1,1,0)") %>% 
            config(displayModeBar = FALSE)
    }
    
    #################
    # get details for address
    
    location <- function(addrSearch) {
        g <- "https://maps.googleapis.com/maps/api/place/autocomplete/json?input="
        
        search <- gsub(" ", "+", addrSearch, fixed = T)
        key <- "&types=geocode&key=AIzaSyCh4AzBfyORUM-oJ2-0pEjaG-fZtj8q9cM"
        q <- paste0(g, search, key)
        print(paste0("Retrieving address from: ", q))
        r <- fromJSON(q)
        a <- r$predictions[[1]][1]
        
        print(a)
        
        addr <- data.frame(Longitude = geocode(a, source = "dsk")[1,1],
                           Latitude = geocode(a, source = "dsk")[1,2])
        
        print("Address coordinates")
        print(addr)
        
        # create spatial object
        coordinates(addr) <- ~ Longitude + Latitude
        
        # Set the projection of the SpatialPointsDataFrame
        # using the projection of the shapefile
        proj4string(addr) <- proj4string(CTshapes)
        o <- over(addr, CTshapes)
        
        h <- over(addr, hoods)
        
        x <- data.frame(GEOID = as.numeric(o[1,"GEOID"]),
                        lat = addr$Latitude,
                        lon = addr$Longitude,
                        hood = h$ntaname,
                        stringsAsFactors = FALSE)
        
        x %<>% left_join(pred, by = c("GEOID" = "ct"))
        
        print(paste("GEOID:",x$GEOID))
        print(paste("Intelligentsia score:",x$bin_20))
        
        x
    }
    
    #################
    # define map
    
    map <- function(addr) {
        
        # create bounding box for filtering data
        d <- .01
        bbox <- sapply(c(addr$lat,addr$lon), function(x) x + c(-1,1) * d)
        
        GeoDF[GeoDF$GEOID == addr$GEOID,] %>% leaflet() %>%
            setView(lng = addr$lon, lat = addr$lat, zoom = 14) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(
                color = "#03BCC0",
                weight = 1, smoothFactor = 0.5,
                fillOpacity = .3) %>%
            addCircleMarkers(~lon, ~lat,
                             data = addr, color = "#C10250") %>%
            addMarkers(~lon, ~lat,
                       data = subwayFlat 
                       , group = "Subway",
                       icon = icons(iconUrl = sprintf("www/images/subwayStations/%s.png", subwayFlat$Line),
                                    iconAnchorX = 7 - 16 * subwayFlat$offsetX, 
                                    iconAnchorY = 7 - 16 * subwayFlat$offsetY,
                                    iconWidth = 14, iconHeight = 14)) %>% 
            addPolylines(data = subwayLines, weight = 3, 
                         color = "#03BCC0", group = "Subway") %>%
            addCircleMarkers(data = schools[schools@coords[,2] > bbox[1,1] & schools@coords[,2] < bbox[2,1] &
                                                schools@coords[,1] > bbox[1,2] & schools@coords[,1] < bbox[2,2],],
                             group = "Schools",
                             color = "#FF5850", 
                             label = schoolNames, radius = 5, 
                             stroke = FALSE, fillOpacity = .8) %>% 
            addCircleMarkers(~lon, ~lat, data = 
                             yelp %>% filter(lat > bbox[1,1] & lat < bbox[2,1] &
                                             lon > bbox[1,2] & lon < bbox[2,2]), 
                             group = "Yelp", color = "#03BCC0", radius = 2) %>% 
            hideGroup(c("Schools", "Subway", "Yelp")) %>% 
            addLayersControl(
                overlayGroups = c("Schools", "Subway", "Yelp"))
    }
    
    #################
    # define donut
    
    donut <- function(x) {
        renderPlotly({
            
            iScore <- round(pred[pred$ct == x$GEOID,"bin_20"] * 100,0)
            
            color <- brewer.pal(9,"YlOrRd")[max(1,floor((iScore-1) / 10))]
            
            lw <- 0
            
            plot_ly(width = 80, height = 80,
                    marker = list(colors = c(color, "rgba(0,0,0,0)"),
                                  line = list(width = lw, color = "#444"))) %>%
                add_pie(values = c(iScore, 100 - iScore),
                        hole = .7, sort = FALSE, direction = "clockwise",
                        textposition = 'none') %>%
                layout(font = list(family = "'Raleway', sans-serif"),
                       autosize = TRUE,
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       paper_bgcolor = "rgba(1,1,1,0)",
                       plot_bgcolor = "rgba(1,1,1,0)",
                       hovermode = FALSE,
                       showlegend = FALSE,
                       margin = list(l = 0, r = 0, t = 5, b = 0),
                       annotations = list(text = paste0("<b>",iScore,"</b>"),
                                          align = "center",
                                          font = list(size = 28),
                                          showarrow = FALSE)) %>%
                config(displayModeBar = F)
        })
    }
    
    #################
    # location 1
    
    location1 <- reactive({
        location(input$searchAddr1)
    })
    
    output$hoodName1 <- renderText({
        x <- location1()
        x$hood
    })
    
    output$iScore1 <- renderText({
        x <- location1()
        if(is.na(x$bin_20)) {
            shinyjs::hide("hood-details1")
            shinyjs::hide("reactiveOutput8")
            return("This address is not located in a gentrifying census tract.")
        } else {
            return(paste0("Intelligentsia predicts the census tract of this location to gentrify with a probability of ", 
                          sprintf("%.1f",x$bin_20*100),"%."))
        }
    })
    
    output$zillowLocation1 <- renderPlotly({
        x <- location1()
        zillowSmall(x)
    })
    
    mapLocation1 <- eventReactive(input$compare, {
        addr <- location1()
        map(addr)
    })
    
    output$mapLocation1 <- renderLeaflet({
        mapLocation1()
    })
    
    output$donutLocation1 <- eventReactive(input$compare, {
        x <- location1()
        donut(x)
    })
    
    #################
    # location 2
    
    location2 <- reactive({
        location(input$searchAddr2)
    })
    
    output$hoodName2 <- renderText({
        x <- location2()
        x$hood
    })
    
    output$iScore2 <- renderText({
        x <- location2()
        if(is.na(x$bin_20)) {
            shinyjs::hide("hood-details2")
            shinyjs::hide("reactiveOutput8")
            return("This address is not located in a gentrifying census tract.")
        } else {
            return(paste0("Intelligentsia predicts the census tract of this location to gentrify with a probability of ", 
                          sprintf("%.1f",x$bin_20*100),"%."))
        }
    })
    
    output$zillowLocation2 <- renderPlotly({
        x <- location2()
        zillowSmall(x)
    })
    
    mapLocation2 <- eventReactive(input$compare, {
        addr <- location2()
        map(addr)
    })
    
    output$mapLocation2 <- renderLeaflet({
        mapLocation2()
    })
    
    output$donutLocation2 <- eventReactive(input$compare, {
        x <- location2()
        donut(x)
    })
    
    #################
    # CT comparison
    
    output$CTcomparisonChart <- renderPlotly({
        l1 <- location1()
        l2 <- location2()
        
        l <- c(l1$GEOID, l2$GEOID)
        
        cols <- c("pct_white", "pct_black", "pct_indian",
                  "pct_asian", "pct_pac", "pct_grad", "pct_family", 
                  "pct_married", "pct_renter", "pct_u34")
        cts <- pred[,c("ct",cols)] %>% data.frame() %>% filter(ct %in% l)
        cts <- cts[,cols]
        ct1 <- cts[1,]
        ct2 <- cts[2,]
        
        d <- ct2[,1:ncol(ct2)] - ct1[,1:ncol(ct1)]
        
        max(abs(d))
        
        if(max(abs(d)) > .6){
            o <- .2
        } else if(max(abs(d)) > .2) {
            o <- .1
        } else {
            o <- 0
        }
        
        d %<>% gather() %>% 
            filter(value < 0) %>% mutate(
                valueOffset = value - o,
                offset = -o) %>% 
            inner_join(vars, by = c("key" = "Data.Element")) %>% 
            bind_rows(
                d %>% gather() %>% 
                    filter(value >= 0) %>% mutate(
                        valueOffset = value + o,
                        offset = +o) %>% 
                    inner_join(vars, by = c("key" = "Data.Element"))
            )
        
        # format long labels (wrap long labels with <br> tag)
        wrap_strings <- function(vector_of_strings, width){
            as.character(
                sapply(vector_of_strings,
                       function(x){
                           paste(strwrap(x, width), collapse="<br>")
                       })
            )
        }
        
        d$Description %<>% wrap_strings(20)
        
        r <- round(max(abs(range(d$valueOffset)))+.05,1)
        r <- pretty(range(-r,r))
        rLabels <- abs(r)
        rLabels[rLabels != 0] <- as.numeric(rLabels[rLabels != 0]) - o
        rLabels <- sprintf("%.0f", abs(rLabels * 100))
        rLabels %<>% paste0("%")
        rLabels[rLabels == "0%"] <- ""
        
        d %>% slice(match(cols, key)) %>% 
            plot_ly(showlegend = FALSE) %>% 
            add_trace(x = ~offset, y = ~key, type = "bar", marker = list(color = "rgba(1,1,1,0)")) %>% 
            add_trace(x = ~value,  y = ~key, type = "bar", marker = list(color = "#436983")) %>% 
            add_text(x = 0, y = ~key, text = ~d$Description, type = "scatter", 
                     hoverinfo = "text", mode = "text",
                     textfont = list(family = "'Raleway', sans-serif", 
                                     size = 14, color = "#hhh")) %>% 
            layout(barmode = "stack", hovermode = FALSE,
                   xaxis = list(title = "", ticks = "",
                                range = c(-max(r),max(r)),
                                fixedrange = TRUE, 
                                zeroline = TRUE, zerolinecolor = "#f2fcff",
                                tickmode = "array",
                                tickvals = r,
                                ticktext = rLabels),
                   yaxis = list(title = "", fixedrange = TRUE,
                                showticklabels = FALSE),
                   shapes = list(list(x0 = -o, x1 = -o, y0 = -1, y1 = nrow(d),
                                      line = list(color = "#adadad")),
                                 list(x0 =  o, x1 =  o, y0 = -1, y1 = nrow(d),
                                      line = list(color = "#adadad"))),
                   annotations = list(
                       list(text = "Source: ACS", align = "left",
                            x = 0, y = -.3, showarrow = FALSE,
                            xref = "paper", yref = "paper"),
                       list(text = paste("Higher value in",l1$hood), 
                            font = list(size = 16),
                            align = "center", xref = "paper",
                            yref = "paper", xanchor = "center",
                            x = .25, y = 1.1, showarrow = FALSE),
                       list(text = paste("Higher value in",l2$hood), 
                            font = list(size = 16),
                            align = "center", xref = "paper",
                            yref = "paper", xanchor = "center",
                            x = .75, y = 1.1, showarrow = FALSE)
                   ),
                   margin = list(l = 10, r = 10, t = 80, b = 80, pad = 5),
                   font = list(family = "'Raleway', sans-serif"),
                   paper_bgcolor = "rgba(1,1,1,0)",
                   plot_bgcolor = "rgba(1,1,1,0)") %>% 
            config(displayModeBar = FALSE)
    })
    
    output$CTcomparisonTable <- DT::renderDataTable({
        l1 <- location1()
        l2 <- location2()
        
        l <- c(l1$GEOID, l2$GEOID)
        
        cols <- c("ct","MHV","HPI","med_rent","myb","occupied","pct_renter",
                  "ylp_avg","taxi_share","pct_taxi","t10treepc",
                  "mhc","MHI","pct_all_coll","pct_family","med_tage",
                  "MHV_neigh")
        
        cts <- pred[,cols] %>% data.frame() %>% filter(ct %in% l)
        cts$ct %<>% paste("CT:",.)
        ct1 <- cts[1,]
        ct2 <- cts[2,]
        
        t <- ct1 %>% gather("key","value1",2:length(ct1)) %>% dplyr::select(key, value1) %>% 
            bind_cols(
                ct2 %>% gather("key","value2",2:length(ct2)) %>% dplyr::select(value2)
            ) %>% 
            inner_join(vars, by = c("key" = "Data.Element")) %>% 
            slice(match(cols, key)) %>% 
            dplyr::select(value1, Description, value2, type)
        
        for(r in 1:nrow(t)){
            t[r,"value1"] <- format_metric(t[r,"value1"], t[r,"type"])
            t[r,"value2"] <- format_metric(t[r,"value2"], t[r,"type"])
        }
        
        t
        
        t %>% 
            dplyr::select(value2, Description, value1) %>% 
            datatable(colnames = c("","Metric",""),
                      rownames = FALSE,
                      filter = "none",
                      options = list(
                          paging = FALSE, searching = FALSE,
                          sort = FALSE, info = FALSE,
                          columnDefs = list(list(className = 'dt-center', targets = 0:2,
                                                 width = '200px', targets = c(2)))))
        })
    
})
