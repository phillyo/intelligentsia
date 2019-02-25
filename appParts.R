#   ____________________________________________________________________________
#   Neighborhood Browser                                                    ####

neighborhoodDescription <- function() {
    tagList(
        div(class = "container",
            h1("Neighborhood Browser", class = "title fit-h1"),
            p("You are new to New York City or real estate investment? Use Intelligentsia's neighborhood browser to identify pockets of opportunity in the city."),
            p("Use the map to browse New York City's gentrifying census tracts. Click on any one of them to get more detailed information. Use the slider to show only the top k tracts."),
            fluidRow(
                column(7,
                       sliderInput("topK","Show top k census tracts",
                                   10, max(pred$rank), 50, 10, width = "100%"),
                       leafletOutput("map", height = 600)
                       ),
                hidden(column(5, class = "hood-info", id = "reactiveOutput1",
                       h1(textOutput("hood"), class = "heading"),
                       htmlOutput("hoodInfo")
                       ))
            ),
            hidden(
                div(class = "kpi-group",
                    fluidRow(style = "margin-top: 10px;",
                                    id = "reactiveOutput2a",
                             column(3,
                                    div(plotlyOutput("donut", height = "100%"), align = "center"),
                                    h3("Intelligentsia Score", class = "kpi-name")
                             ),
                             column(3,
                                    h2(textOutput("kpi1"), class = "kpi"),
                                    h3("Median Home Value", class = "kpi-name")
                             ),
                             column(3,
                                    h2(textOutput("kpi2"), class = "kpi"),
                                    h3("Median Housing Cost", class = "kpi-name")
                             ),
                             column(3,
                                    h2(textOutput("kpi3"), class = "kpi"),
                                    h3("Year built (median)", class = "kpi-name")
                             )
                    ),
                    fluidRow(id = "reactiveOutput2b",
                             column(3,
                                    h2(textOutput("kpi4"), class = "kpi"),
                                    HTML("<h3 class='kpi-name'>Avg. Yelp Rating<sup>1</sup></h3>")
                             ),
                             column(3,
                                    h2(textOutput("kpi5"), class = "kpi"),
                                    HTML("<h3 class='kpi-name'>Walkability Index<sup>2</sup></h3>")
                             ),
                             column(3,
                                    h2(textOutput("kpi6"), class = "kpi"),
                                    HTML("<h3 class='kpi-name'># of Trees<sup>3</sup></h3>")
                             ),
                             column(3,
                                    h2(textOutput("kpi7"), class = "kpi"),
                                    HTML("<h3 class='kpi-name'>% Share of Taxicab Rides at Night<sup>4</sup></h3>")
                             )
                    ),
                    fluidRow(id = "reactiveOutput2c",
                             column(3,
                                    h2(textOutput("kpi8"), class = "kpi"),
                                    h3("% College Education", class = "kpi-name")
                             ),
                             column(3,
                                    h2(textOutput("kpi9"), class = "kpi"),
                                    h3("Median Age", class = "kpi-name")
                             ),
                             column(3,
                                    h2(textOutput("kpi10"), class = "kpi"),
                                    h3("% Family Households", class = "kpi-name")
                             ),
                             column(3,
                                    h2(textOutput("kpi11"), class = "kpi"),
                                    h3("% Crime", class = "kpi-name")
                             )
                    ),
                    fluidRow(column(12,tags$small("Sources: U.S. Census Bureau, 1. Yelp, 2. BEH, 3. NYC Open Data, 4. NYC TLC")))
                ),
                hr(),
                fluidRow(id = "reactiveOutput3",
                    column(12,
                           h2("Development of real estate prices"),
                           div(plotlyOutput("zillow1", width = "100%"), align = "center", 
                               class = "chart", width = "100%")
                           )
                ),
                hr(),
                fluidRow(id = "reactiveOutput4",
                         column(12,
                                h2("Yelp reviews over time"),
                                div(plotlyOutput("yelp", height = "420px"), align = "center",
                                    class = "chart")
                                )
                ),
                hr(),
                fluidRow(id = "reactiveOutput4a1",
                         column(12,h2("Online Activities"))),
                fluidRow(id = "reactiveOutput4a",
                         column(6,
                                h3("Google Search Trends"),
                                div(plotlyOutput("google"), align = "center",
                                    class = "chart")
                                ),
                         column(6,
                                h3("Wikipedia Edits"),
                                div(plotlyOutput("wiki"), align = "center",
                                    class = "chart")
                         )
                ),
                hr(),
                fluidRow(id = "reactiveOutput5",
                         column(12,
                                h2("Taxi trips over time"),
                                div(plotlyOutput("taxi"), align = "center", 
                                    class = "chart")
                                )
                ),
                hr(),
                fluidRow(id = "reactiveOutput6",
                         column(12,
                                h2("Properties currently for sale on Zillow"),
                                div(htmlOutput("propertiesForSale"),
                                    class = "property-card-container"))
                         )
            )
        )
    )
}


#   ____________________________________________________________________________
#   Location Comparison                                                     ####

legend <- "<div class='legend-custom'>
               <div class='legend-group'>
                   <div class='legend-element-label' color='location'></div>
                   <div class='legend-element-name'>Property</div>
               </div>
               <div class='legend-group'>
                   <div class='legend-element-label' color='yelp'></div>
                   <div class='legend-element-name'>Yelp</div>
               </div>
               <div class='legend-group'>
                   <div class='legend-element-label' color='schools'></div>
                   <div class='legend-element-name'>Schools</div>
               </div>
           </div>"

propertyComparison <- function() {
    
    sampleLocations <- c("20 Gerry St, New York",
                         "38 Harrison Ave, New York",
                         "1862 Cornelia St, New York",
                         "243-245 E 118th St, New York",
                         "517 W 147th St, New York")
    
    sampleLocations <- sample(sampleLocations,2)
    
    tagList(
        div(class = "container",
            h1("Location Comparison", class = "title fit-h1"),
            #tags$script(src = "plugins/fittext.js"),
            p("You have already identified two locations of interest, but cannt decide which one to invest in? Let us help make your final decision."),
            p("Enter the addresses of two properties below and click the button to compare the locations in terms of gentrification potential."),
            p("Click the button to compare two sample locations or enter search addresses of your own!"),
            fluidRow(
                column(4,
                       div(class = "addrSearch",
                           textInput("searchAddr1", 
                                     value = sampleLocations[1],
                                     placeholder = "Enter address...",
                                     label = NA),
                           class = "search")
                       ),
                column(4, class="text-center", 
                       disabled(actionButton("compare", width = "75%",
                                    class = "btn-primary", style = "margin: 20px 0 20px 0;",
                                    HTML("&laquo; Compare locations &raquo;")))
                       ),
                column(4,
                       div(class = "addrSearch",
                           textInput("searchAddr2", 
                                     value = sampleLocations[2],
                                     placeholder = "Enter address...",
                                     label = NA),
                           class = "search"
                       )
                )
            ),
            hidden(
                div(id = "mapControls",
                    fluidRow(style = "margin-bottom: 15px; text-align: center;",
                        column(12,
                               bsButton("showLayerSchools", "Schools", style = "info", size = "small", type = "toggle"),
                               bsButton("showLayerSubway", "Subway", style = "info", size = "small", type = "toggle"),
                               bsButton("showLayerYelp", "Yelp", style = "info", size = "small", type = "toggle")
                               )
                        )
                )
            ),
            fluidRow(
                column(6,
                       hidden(div(id = "reactiveOutput7a",
                           leafletOutput("mapLocation1"),
                           HTML(legend),
                           h2(textOutput("hoodName1")),
                           p(textOutput("iScore1", inline = TRUE), style = "height: 60px;"),
                           div(id = "hood-details1",
                               h4("Monthly development of real estate prices"),
                               plotlyOutput("zillowLocation1", height = "150px", width = "100%")
                               )
                           ))
                       ),
                column(6,
                       hidden(div(id = "reactiveOutput7b",
                           leafletOutput("mapLocation2"),
                           HTML(legend),
                           h2(textOutput("hoodName2")),
                           p(textOutput("iScore2", inline = TRUE), style = "height: 60px;"),
                           div(id = "hood-details2",
                               h4("Monthly development of real estate prices"),
                               plotlyOutput("zillowLocation2", height = "150px", width = "100%")
                               )
                           ))
                       )
            ),
            fluidRow(
                column(12,
                       
                       hidden(div(id = "reactiveOutput8",
                                  hr(),
                                  plotlyOutput("CTcomparisonChart", height = "750px"))
                       ))
            ),
            fluidRow(
                column(12,
                       hidden(div(id = "reactiveOutput9", 
                                  style = "margin-top: 30px; font-size: 1.5em;",
                                  hr(),
                                  DT::dataTableOutput("CTcomparisonTable"))
                       ))
            )
        )
    )
}