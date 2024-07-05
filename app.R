##########################################################################
# Project: Culebra Water Quality Dashboard
# 
# Code Development by: Mike Wessel, Inferential Consulting,LLC
# Inferential.consulting@gmail.com

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button in the upper right.
#

# Github Repo: https://github.com/mikewessel/NOAA-Culebra-LBSP/analysis
#
##########################################################################


library(shinyWidgets)
library(shiny) 
library(dplyr)
library(ggplot2)
library(conflicted)
library(lubridate)
library(leaflet)
library(plotly)

conflicts_prefer(dplyr::filter, dplyr::lag)

#### Grab the data

params = read.csv("CulParams.csv") |> 
  mutate(ParameterUnits = paste0(Parameter, " (", Units, ")"))

dat <- readRDS("CulWQ_clean-2024-05-18.rds") %>%
  # μ character wasn't read correctly from CulParams.csv so removed it
  mutate(Param = ifelse(Param == "Surface.Conductivity..μS.cm.", "Surface.Conductivity",
                        ifelse(Param == "Bottom.Conductivity..μS.cm.", "Bottom.Conductivity", Param))) %>% 
  select(-Units) %>%
  arrange(Location, Site.ID, Date, Param) %>%
  left_join(params)

points <- readRDS("Cul_wqpoints.rds") %>% 
  arrange(Location, Site.ID)

# Join location and wq file
bubbles <- left_join(dat, points, by = c("Location", "Site.ID"))   

# Generate medians for map bubble plots
meddat <- bubbles %>%
  group_by(Location, Site.ID, Param, ParameterUnits, Latitude, Longitude) %>%
  summarize(med_value = median(Result,na.rm=TRUE), .groups = 'drop')

# Shiny UI
ui <- fluidPage(
  titlePanel("Culebra Water Quality Dashboard"),
  tabsetPanel(
    tabPanel("Timeseries Plots", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = 'Locsel', label = 'Location', choices = c('Nearshore', 'Watershed'),
                             selected = 'Nearshore'),
                 selectInput(inputId = 'Levelsel', label = 'Sample Level', choices = 'Surface',
                             selected = 'Surface'),
                 selectInput(inputId = 'Parmsel', label = 'Parameter', choices = 'Turbidity (ntu)',
                             selected = 'Turbidity (ntu)')),         
               mainPanel(plotlyOutput('plo'))
             )
    ),
    
    tabPanel("Map Plots", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = 'Parmsel2', label = 'Select Parameter', choices = 'Turbidity (ntu)',
                             selected = 'Turbidity (ntu)')
               ),         
               mainPanel(leafletOutput(outputId = 'map'))
             )
    )
  )
)


# Shiny server
server <- function(input, output, session){
  
  datSub1 <- reactive({
    filter(dat, Location == input$Locsel)
  })
  
  observe({
    sl = sort(unique(datSub1()$SampleLevel), decreasing = TRUE)
    updateSelectInput(session, 'Levelsel', choices = sl)
  })
  
  datSub2 <- reactive({
    filter(datSub1(), SampleLevel == input$Levelsel)
  })
  
  observe({
    parms = sort(unique(datSub2()$ParameterUnits))
    updateSelectInput(session, 'Parmsel', choices = parms)
  })
  
  datSub3 <- reactive({
    filter(datSub2(), ParameterUnits == input$Parmsel)
  })
  
  output$plo <- renderPlotly({
    p = ggplot(datSub3(), aes(x = Date, y = Result, color = Site.ID)) + 
      geom_point() +
      geom_smooth(method = "loess", fill = NA) + 
      # ylim(mn, mx) +
      labs(title = "Trend in Water Quality over Time", x = "", y = input$Parmsel, color = "Site") +
      theme_bw()
    
    ggplotly(p)
  })
  
  # reactive to pull the selected input parameter 
  filteredData <- reactive({
    meddat[meddat$Param==input$Parmsel2,]
  })
  
  mypalette <- reactive({
    colorBin(palette = "YlOrBr", domain =filteredData()$med_value , bins = 6)
  })
  
  basemap <- reactive(
    leaflet() %>%
      addTiles()%>%
      addProviderTiles("Esri.WorldImagery")%>%
      setView(lat=18.313,lng=-65.273,zoom=13)%>%
      addCircles(data=filteredData(),lng = ~ Longitude, lat = ~ Latitude,popup=~paste(as.character(filteredData()$Site.ID),"<br>",as.character(filteredData()$med_value),sep=" "),
                 fillColor = ~mypalette()(med_value), fillOpacity = 0.7, color="white", radius=120, stroke=FALSE, 
                 label = filteredData()$Site.ID,
                 labelOptions = labelOptions(noHide = T, textOnly=TRUE, textsize = "15px",direction = "top",
                                             style = list(
                                               "color" = "white")))%>%
      addLegend("topright", pal = mypalette(), values = filteredData()$med_value,bins=6, title = "Median Value")  
  )
  
  
  output$map <- renderLeaflet({basemap()})
  
  
  observeEvent(
    filteredData(),
    leafletProxy("map")%>%
      clearShapes()%>%
      addCircles(data=filteredData(),lng = ~ Longitude, lat = ~ Latitude,popup=~paste(as.character(filteredData()$Site.ID),"<br>",as.character(filteredData()$med_value),sep=" "),
                 fillColor = ~mypalette()(med_value), fillOpacity = 0.7, color="black", radius=120, stroke=FALSE,
                 labelOptions = labelOptions(noHide = T, textOnly=TRUE, textsize = "15px",direction = "top",
                                             style = list(
                                               "color" = "white")))
  )
}

# run app
shinyApp(ui = ui, server = server)  

#library(rsconnect)
#rsconnect::deployApp('C:/Wessel/Git_projects/SBEP_Temp/shinytemp/')
