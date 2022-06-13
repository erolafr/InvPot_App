# Required packages
library(shiny)
library(magrittr)
library(dplyr)
library(leaflet)
library(rgbif)
library(leaflet.extras)
library(readxl)
library(shinyWidgets)


# Load the data. See information in the about tab
totaloccs<- read.csv("Data/totaloccs_filtered.csv", na = "NA")

# create subdatasets for those invasive and potentially invasive species, acording to their Status
invtotaloccs<- totaloccs[totaloccs$Status=="Invasive",]
pottotaloccs<- totaloccs[totaloccs$Status=="Potentially invasive in Spain",]

ui <- navbarPage("InvPot", id="nav",
                 tabPanel("Interactive map",
                          div(class="outer",
                              tags$head(includeCSS("Styles/styles.css"),includeScript("Styles/gomap.js")),
                              leafletOutput("map", width="100%", height="100%"),
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
                 tabPanel("About", p("This visualization was created in June 2022 to visualize the current distribution of invasive and potentially invasive plant species in Spain. Invasive and potentially invasive plant species distribution in Spain. Plant Invasive species list is defined by the", a("Spanish catalogue of invasive exotic species by the MITECO. ", href="https://www.miteco.gob.es/es/biodiversidad/temas/conservacion-deespecies/especies-exoticas-invasoras/ce-eei-catalogo.aspx", target="_blank"), a("Potentially invasive plant species list", href="www.miteco.gob.es/es/biodiversidad/temas/conservacion-de-especies/especiesexoticas-invasoras/ce-eei-iea-listado.aspx", target="_blank"), "has been downloaded from the MITECO. 
Both lists are public and available in the respective webpages. Species occurrence data has been downloaded in June 2022 from", a("GBIF.", href="https://www.gbif.org/", target="_blank")),
  hr(), 
                          p("Note that regarding several potentially invasive plant species are available, only a 38.19% (i.e. 148 species) have actually records in GBIF. The rest of the potential invasive species may not be present in the peninsula, or just not yet registered. Moreover, the potentially invasive plant species list includes 354 plant taxa that do not correspond to a single species register but a genus such as", em("Asparagus"), "spp., thus indicating that some species from the genus Asparagus are potentially invasive in Spain, but which species is yet to be determined. Those taxa have not been considered for this map as not all species from the genus may be potentially invasive and some of them may eventually be rather native in Spain, such as", em("Asparagus acutifolius.")),
                          p("Data preprocessing includes joining the invasive and potentially invasive datasets considering the species status, identify which are full species and filtering those taxons that are not defined species, delete duplicated GBIF occurrences and remove species that do not have registers in Spain by GBIF at the date."),
  hr(),                           
  
  splitLayout( verticalLayout(  p("Whole data and code can be found here:", a("https://github.com/erolafr/InvPot_App.", href="https://github.com/erolafr/InvPot_App", target="_blank") ),
                          p("More information on invasive species can be found in:", a("https://www.cabi.org/isc/ .", href="https://www.cabi.org/isc/ ", target="_blank") ),
  p("Visualization created by:", a("Dr. Erola Fenollosa", href="https://erolafenollosa.weebly.com/", target="_blank") )), 
  img(src='erola.png', height = '100px', width = '100px', align = "center"))

                 )
)


server <- shinyServer(function(input, output) {
   
   # Input the species name selected 
    selectedData <- reactive({
    if (input$species == "All invasive"){
      invtotaloccs
    } else if (input$species == "All") {
      totaloccs
    } else if (input$species == "All potentially invasive"){
      pottotaloccs
    } else {
      selectedspeciesName <- totaloccs[totaloccs$SpeciesName==input$species,] 
      selectedspeciesName<- selectedspeciesName[complete.cases(selectedspeciesName), ]
      unique(selectedspeciesName)
      
      # In case we would like a live version (downloaded at the time we select a species):
      #selectedspeciesName <- input$species
      #pen<- occ_data(scientificName = selectedspeciesName,  hasCoordinate = TRUE, country= "ES")
      #myspecies_coords <- pen$data[ , c("decimalLongitude", "decimalLatitude")]
      #cord <- data.frame (lon = myspecies_coords$decimalLongitude, lat=myspecies_coords$decimalLatitude)
      #cord[complete.cases(cord), ]
      #unique(cord)
    }
  })

  # output map
  output$map <- leaflet::renderLeaflet({
    regs <- occ_count(taxonKey = name_backbone(name=input$species)$speciesKey, georeferenced = TRUE, country = "ES")
    if (regs < 1) {

      leaflet() %>% addProviderTiles(providers$Stamen.TonerLite, # Or; providers$Stamen.Toner
                                     options = providerTileOptions(noWrap = TRUE)) %>%
        addProviderTiles(providers$Stamen.Toner) %>%
        setView(lng = 5, lat = 40, zoom = 5.3)
    } else{
      leaflet(data = selectedData()) %>%
        addProviderTiles(providers$Stamen.TonerLite, 
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addCircleMarkers(~lon, ~lat, radius = 3, color = "brown", stroke = FALSE, fillOpacity = 0.5) %>%
        addProviderTiles(providers$Stamen.Toner) %>%
        addHeatmap(group="heat", lng=~lon, lat=~lat, max=.6, blur = 60) %>%
        setView(lng = 5, lat = 40, zoom = 5.3)
    }
  })

  #A function to select the data within the view bounders
  in_bounding_box <- function(data, lat, lon, bounds) {
   data %>%
      dplyr::filter(lat > bounds$south & lat < bounds$north & lon < bounds$east & lon > bounds$west)
  }

  # reactive expression to capture the data found within the actual view
  data_map <- reactive({
    if (is.null(input$map_bounds)){
      selectedData()
    } else {
      bounds <- input$map_bounds
        in_bounding_box(selectedData(), lat, lon, bounds)
    }
  })

   #Conditional output of a table showing species occurences count only for Total Maps:
   output$tbl <- DT::renderDataTable({
     df.datamap <- data_map() 
     top <- data.frame(head(df.datamap %>% count(SpeciesName, Status, sort=TRUE), 10))
      DT::datatable(top, extensions = "Scroller", style = "bootstrap", class = "compact", width = "100%",
                   options = list(deferRender = TRUE, scrollY = 300, scroller = TRUE,  dom = 'tp'))})

  # Outout to show the selected species status
  output$selected_status <- renderText({

    if (input$species == "All invasive"){
      paste("Status: Invasive species")
    } else if (input$species == "All"){
      paste("Status: Invasive and Potentially invasive species")
    } else if (input$species == "All potentially invasive"){
      paste("Status: Potentially invasive species")
    } else {
      paste("Status: ", unique(totaloccs[totaloccs$SpeciesName==input$species,]$Status))
      
      
    }
  })

  # Output to Show the total number of registres
  output$tot_regs_number <- renderText({

    if (input$species == "All invasive"){
      paste("Total Nº of records: ", nrow(invtotaloccs))
    } else if (input$species == "All"){
      paste("Total Nº of records: ", nrow(totaloccs))
    }   else if (input$species == "All potentially invasive"){
      paste("Total Nº of records: ", nrow(pottotaloccs))
    } else {
      #regs <- occ_count(taxonKey = name_backbone(name=input$species)$speciesKey, georeferenced = TRUE, country = "ES") # a live version
      regs <- nrow(selectedData())
      paste("Total Nº of records: ", regs)
    }
  })

  # Output to show the dynamic number of registres (within the current view)
  output$din_regs_number <- renderText({
      paste("Nº of registres in view: ", nrow(data_map()))
  })

  
})

# Run the application
shinyApp(ui = ui, server = server)
