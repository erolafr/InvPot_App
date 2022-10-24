# InvPot_App
Invasive and potential invasive distribution in Spain - Shiny App

This visualization was created in June 2022 to visualize the current distribution of invasive and potentially invasive plant species in Spain. Invasive and potentially invasive plant species distribution in Spain. Plant Invasive species list is defined by the Spanish catalogue of invasive exotic species by the MITECO. Potentially invasive plant species list has been downloaded from the MITECO. Both lists are public and available in the respective webpages. Species occurrence data has been downloaded in June 2022 from GBIF.

Note that regarding several potentially invasive plant species are available, only a 38.19% (i.e. 148 species) have actually records in GBIF. The rest of the potential invasive species may not be present in the peninsula, or just not yet registered. Moreover, the potentially invasive plant species list includes 354 plant taxa that do not correspond to a single species register but a genus such as Asparagus spp., thus indicating that some species from the genus Asparagus are potentially invasive in Spain, but which species is yet to be determined. Those taxa have not been considered for this map as not all species from the genus may be potentially invasive and some of them may eventually be rather native in Spain, such as Asparagus acutifolius.

Data preprocessing includes joining the invasive and potentially invasive datasets considering the species status, identify which are full species and filtering those taxons that are not defined species, delete duplicated GBIF occurrences and remove species that do not have registers in Spain by GBIF at the date.

More information on invasive species can be found in: https://www.cabi.org/isc/ .

Visualization created by: Dr. Erola Fenollosa

App: https://erolafenollosa.shinyapps.io/Invasive_And_Potential_Plant_Distribution_Spain/
