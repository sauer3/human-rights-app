library(shiny)
library(RColorBrewer)
library(leaflet)
library(shinyWidgets)
library(rgdal)
library(dplyr)
library(ggplot2)
library(viridis)
library(maptools)
library(gpclib)
gpclibPermit()

df <- read.csv("data/final_for_viz.csv")
hudoc_shp <- readOGR("data","ne_50m_admin_0_countries")

article_order <- c("2: Right to life",
                   "3: Prohibition of torture",
                   "4: Prohibition of slavery",
                   "5: Right to liberty and security", 
                   "6: Right to a fair trial",
                   "7: No punishment without law",
                   "8: Respect for private life",
                   "9: Freedom of thought",
                   "10: Freedom of expression",
                   "11: Freedom of assembly",
                   "12: Right to marry",
                   "13: Right to an effective remedy",
                   "14: Prohibition of descrimination",
                   "18: Convention rights restricted for purposes other than those prescribed in the Convention", 
                   "25: Right to come before the commission",
                   "34: Exercise right of individual petition",                                                    
                   "38: Contracting state obligations to furnish necessary facilities for investigation",
                   "46: Obligation to abide by final judgement",
                   "P1: Protection of property",                                                                  
                   "P4: freedom from imprisonment for debt",                                                      
                   "P7: Procedural guarantees for Aliens before expulsion",
                   "P6: Prohibition of death penalty",                                                             
                   "P12: Prohibition of descrimination"                                                          
)

# ui ----------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Human Rights Violations"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "yearInput",
                  label = "Year",
                  min = min(df$year, na.rm = TRUE),
                  max = max(df$year, na.rm = TRUE),
                  value = c(min, max),
                  step = 1,
                  sep = ""),
      
      checkboxGroupInput("groupInput", "",
                         choices = c("Violations" = "violation" , "Non-violations" = "non-violation"),
                         selected = "violation"),
      
      # add article selection with select all/deselect all option
      pickerInput(
        inputId = "articleInput",
        label = "Article Selection",
        choices = c(article_order),
        selected = c(article_order),
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      ),
      
      # Add ability to select "and" or "or" for article selection
      strong("Filter Functionality"),
      switchInput(inputId = "andor", 
                  value = FALSE,
                  onLabel = "AND", # Text on the left side of the switch (TRUE)
                  offLabel = "OR", # Text on the right side of the switch (FALSE).
                  onStatus = "red",
                  offStatus = "blue"),
      p('Choose how you want articles filtered. For example, if you selected article 2 and article 3,
        "AND" would give you cases with both article 2 and 3 violations, "OR" would give you cases with either article
        2 or 3 violations.',
        style = "font-family:font-si11pt")
    ),
    
    mainPanel(leafletOutput("map", height = "600px", width = "700px"),
              downloadButton("downloadData", "Download Data"))
    
    )
  )


# server -----------------------------------------------------------

server <- function(input, output){
  
  # Map plot of selected data --------
  output$map <- renderLeaflet({
    if (input$andor == FALSE){
      df_summarize <- df %>%
        filter(year >= input$yearInput[1],
               year <= input$yearInput[2],
               Judgement_type %in% input$groupInput,
               label %in% input$articleInput)
    }
    else {
      df_summarize <- df %>%
        filter(year >= input$yearInput[1],
               year <= input$yearInput[2],
               Judgement_type %in% input$groupInput,
               all_labels == paste0(
                 input$articleInput[order(nchar(input$articleInput), input$articleInput)], 
                 collapse = ","))
               #all_labels == paste0(input$articleInput, collapse = ","))
    }
    
    map_df <- df_summarize %>%
      group_by(Country, Application_Number) %>%
      summarise(number_of_cases = n()) %>%
      group_by(Country) %>%
      summarise(number_of_cases = n())
    
    hudoc_shp2 <- sp::merge(hudoc_shp, map_df, by.x="NAME_EN", by.y="Country")
    hudoc_shp2@data$id <- rownames(hudoc_shp2@data)
    shp <- fortify(hudoc_shp2, region = "id")
    hudoc_shp_df <- merge(shp, hudoc_shp2@data, by="id")
    hudoc_shp_df <- hudoc_shp_df[c(1:8, 102)]
    
    # Numeric palette
    rng <- range(0, hudoc_shp2$number_of_cases, na.rm = TRUE)
    # pal <- colorNumeric(palette="viridis", domain=rng, 
    #                     na.color="transparent")
    pal <- colorNumeric(palette="OrRd", domain=rng, 
                        na.color="transparent")
    
    # Prepare the text for tooltips
    mytext <- paste(
      "Country: ", hudoc_shp2@data$NAME_EN,"<br/>", 
      "Number of Cases: ", hudoc_shp2@data$number_of_cases, 
      sep="") %>%
      lapply(htmltools::HTML)
    
  # Final Map
   leaflet(hudoc_shp2) %>%
     setView(lat=50, lng=11 , zoom=3) %>%
     addProviderTiles(providers$Esri.WorldShadedRelief) %>%
     addPolygons(
       fillColor = ~pal(number_of_cases), 
       stroke=TRUE, 
       fillOpacity = 0.9, 
       color="white", 
       weight=0.7,
       label = mytext,
       highlight = highlightOptions(
         weight = 2,
         color = "#666",
         fillOpacity = 0.6,
         bringToFront = TRUE),
       labelOptions = labelOptions(
         style = list("font-weight" = "normal", padding = "3px 8px"),
         textsize = "13px",
         direction = "auto")) %>%
     addLegend(pal=pal, values=~number_of_cases, opacity=0.9, 
               title = "Number of Cases", position = "bottomleft")
  })
  
#  Downloadable csv of selected dataset --------------
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$groupInput, input$yearInput[1], "-" ,input$yearInput[2], ".csv", sep="")
    },
    content = function(file) {
      if (input$andor == FALSE){
        df_table <- df %>%
          filter(year >= input$yearInput[1],
                 year <= input$yearInput[2],
                 Judgement_type %in% input$groupInput,
                 label %in% input$articleInput)
      }
      else {
        df_table <- df %>%
          filter(year >= input$yearInput[1],
                 year <= input$yearInput[2],
                 Judgement_type %in% input$groupInput,
                 all_labels == paste0(
                   input$articleInput[order(nchar(input$articleInput), input$articleInput)], 
                   collapse = ","))
      }
      write.csv(df_table, file)
    })

}

shinyApp(ui = ui, server = server)
