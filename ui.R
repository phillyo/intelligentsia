#   ____________________________________________________________________________
#   UI                                                                      ####

library(shiny)
library(leaflet)
library(plotly)
library(shinyjs)
library(shinyBS)

source("appParts.R")
source("readData.R")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Colors                                                                  ####

#C10250 purple
#03BCC0 green
#D2D945 yellow/green
#FCB040 orange
#FF5850 red
#436983 hipster blue

shinyUI(navbarPage("Intelligentsia",
                   theme = "style/style.css",
                   footer = includeHTML("footer.html"),
                   fluid = TRUE, collapsible = TRUE,
                   
                   # ----------------------------------
                   # tab panel 1 - Home
                   tabPanel("Home",
                            includeHTML("home.html"),
                            tags$script(src = "plugins/fittext_1.2/jquery.fittext.js"),
                            tags$script(src = "plugins/scripts.js"),
                            tags$script(src = "plugins/fittext.js")
                   ),
                   
                   # ----------------------------------
                   # tab panel 2 - Neighborhood Browser
                   tabPanel("Neighborhood Browser",
                            neighborhoodDescription(),
                            tags$script(src = "plugins/fittext.js"),
                            includeHTML("scrollToTop.html")
                   ),
                   
                   # ----------------------------------
                   # tab panel 3 - Location Comparison
                   tabPanel("Location Comparison",
                            propertyComparison(),
                            tags$script(src = "plugins/fittext.js")
                   ),
                   
                   # ----------------------------------
                   # tab panel 4 - About
                   tabPanel("About",
                            includeHTML("about.html"),
                            shinyjs::useShinyjs(),
                            tags$head(
                                tags$link(rel = "stylesheet", 
                                          type = "text/css", 
                                          href = "plugins/carousel.css"),
                                tags$link(rel = "stylesheet", 
                                          type = "text/css", 
                                          href = "plugins/font-awesome-4.3.0/css/font-awesome.css"),
                                tags$script(src = "plugins/holder.js"),
                                tags$script(src = "plugins/fittext.js")
                            ),
                            tags$style(type="text/css",
                                       ".shiny-output-error { visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"
                            )
                   )
                   
))