# Example from https://github.com/Appsilon/crossfilter-demo

library(shiny)
library(magrittr)
library(dplyr)
library(leaflet)
library(rgbif)
library(leaflet.extras)
library(readxl)
library(shinyWidgets)

df.inv <- read_excel("SpeciesList.xlsx", na = "NA")
df.inv.p <- df.inv[df.inv$CategoriaAmbitoAplicacion=="Peninsular",]
df.inv.p <- df.inv.p[df.inv.p$is.DefinedSpecies,]

totaloccs <- read.csv("totaloccs_filtered.csv", na = "NA")
invtotaloccs<- totaloccs[totaloccs$Status=="Invasive",]
pottotaloccs<- totaloccs[totaloccs$Status=="Potentially invasive in Spain",]

ui <- navbarPage("InvPot", id="nav",
                 tabPanel("Interactive map",
                          div(class="outer",
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css"),
                                includeScript("gomap.js")
                              ),

                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("map", width="100%", height="100%"),

                              # Shiny versions prior to 0.11 should use class = "modal" instead.
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 500, height = "auto",

                                            h2("Distribution of Invasive and Potentially invasive plants in Spain"),
                                            pickerInput("species", label = "Species:",
                                                        choices = list("All", "All invasive", "All potentially invasive",
                                                                       `Invasive` = unique(invtotaloccs$SpeciesName),
                                                                       `Potentially invasive` = unique(pottotaloccs$SpeciesName))),
                                                        options = list(`live-search` = TRUE),

                                            textOutput("selected_status", tags$h4),
                                            textOutput("tot_regs_number", container = tags$h4),
                                            textOutput("din_regs_number", container = tags$h4),
                                            conditionalPanel("input.species == 'All'|| input.species == 'All invasive' || input.species == 'All potentially invasive'",
                                                             DT::dataTableOutput("tbl"))
                              ),

                              tags$div(id="cite", 'Data from MITECO & GBIF (June 2022) processed by Erola Fenollosa'
                              )
                          )
                 ),
                 conditionalPanel("false", icon("crosshair")),
                 tabPanel("About",
                          textOutput("about", container = tags$h4)
                 )
)


server <- shinyServer(function(input, output) {
  # cal fer que scientificName sigui l'input:
  selectedData <- reactive({
    if (input$species == "All invasive"){
      invtotaloccs
    } else if (input$species == "All") {
      totaloccs
    } else if (input$species == "All potentially invasive"){
      pottotaloccs
    } else {
      #selectedspeciesName <- input$species

      selectedspeciesName <- totaloccs[totaloccs$SpeciesName==input$species,] # Importo del csv en comptes de descarregar les occs online de gbif
      selectedspeciesName<- selectedspeciesName[complete.cases(selectedspeciesName), ]
      unique(selectedspeciesName)

      #pen<- occ_data(scientificName = selectedspeciesName,  hasCoordinate = TRUE, country= "ES")
      #myspecies_coords <- pen$data[ , c("decimalLongitude", "decimalLatitude")]
      #cord <- data.frame (lon = myspecies_coords$decimalLongitude, lat=myspecies_coords$decimalLatitude)
      #cord[complete.cases(cord), ]
      #unique(cord)
    }
  })


  output$map <- leaflet::renderLeaflet({
    regs <- occ_count(taxonKey = name_backbone(name=input$species)$speciesKey, georeferenced = TRUE, country = "ES")
    if (regs < 1) {

      leaflet() %>% addProviderTiles(providers$Stamen.TonerLite, # pot ser també un altre mapa base: %>% addProviderTiles(providers$Stamen.Toner)
                                     options = providerTileOptions(noWrap = TRUE)) %>%
        addProviderTiles(providers$Stamen.Toner) %>%
        setView(lng = 5, lat = 40, zoom = 5.3)
    } else{
      leaflet(data = selectedData()) %>%
        addProviderTiles(providers$Stamen.TonerLite, # pot ser també un altre mapa base: %>% addProviderTiles(providers$Stamen.Toner)
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addCircleMarkers(~lon, ~lat, radius = 3, color = "brown", stroke = FALSE, fillOpacity = 0.5) %>%
        addProviderTiles(providers$Stamen.Toner) %>%
        addHeatmap(group="heat", lng=~lon, lat=~lat, max=.6, blur = 60) %>%
        setView(lng = 5, lat = 40, zoom = 5.3)
    }
  })

  in_bounding_box <- function(data, lat, lon, bounds) {
   data %>%
      dplyr::filter(lat > bounds$south & lat < bounds$north & lon < bounds$east & lon > bounds$west)
  }

  data_map <- reactive({
    if (is.null(input$map_bounds)){
      selectedData()
    } else {
      bounds <- input$map_bounds
        in_bounding_box(selectedData(), lat, lon, bounds)
    }
  })

  # Taula amb les observacions que actualment es veuen al mapa:
  # output$tbl <- DT::renderDataTable({
  #    DT::datatable(data_map(), extensions = "Scroller", style = "bootstrap", class = "compact", width = "100%",
   #                 options = list(deferRender = TRUE, scrollY = 300, scroller = TRUE,  dom = 'tp'))})

   #Taula només per quan mostrem mapes totals:
   output$tbl <- DT::renderDataTable({

     df.datamap <- data_map() # cal afegir les columnes de SpeciesName i Status !!!!!!!!!!!!!!!!

     top <- data.frame(head(df.datamap %>% count(SpeciesName, Status, sort=TRUE), 10))

      DT::datatable(top, extensions = "Scroller", style = "bootstrap", class = "compact", width = "100%",
                   options = list(deferRender = TRUE, scrollY = 300, scroller = TRUE,  dom = 'tp'))})

  # show the selected species status
  output$selected_status <- renderText({

    if (input$species == "All invasive"){
      paste("Status: Invasive species")
    } else if (input$species == "All"){
      paste("Status: Invasive and Potentially invasive species")
    } else if (input$species == "All potentially invasive"){
      paste("Status: Potentially invasive species")
    } else {
      paste("Status: ", df.inv.p[df.inv.p$SpeciesName==input$species,]$Status)
    }
  })

  # Show the number of registres
  output$tot_regs_number <- renderText({

    if (input$species == "All invasive"){
      paste("Total Nº of records: ", nrow(invtotaloccs))
    } else if (input$species == "All"){
      paste("Total Nº of records: ", nrow(totaloccs))
    }   else if (input$species == "All potentially invasive"){
      paste("Total Nº of records: ", nrow(pottotaloccs))
    } else {
      #regs <- occ_count(taxonKey = name_backbone(name=input$species)$speciesKey, georeferenced = TRUE, country = "ES")
      regs <- nrow(selectedData())
      paste("Total Nº of records: ", regs)
    }
  })



  # the dynamic number of registres
  output$din_regs_number <- renderText({
      paste("Nº of registres in view: ", nrow(data_map()))
  })

  # the about text
  output$about <- renderText({
    paste("INFORMACIO i REFERENCIES, DATA CREACIO, CREDITS, LLICÈNCIES, NOTES/ASSUMPCIONS")
  })


})

# Run the application
shinyApp(ui = ui, server = server)
