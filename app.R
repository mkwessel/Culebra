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
library(bslib)

conflicts_prefer(dplyr::filter, dplyr::lag)

#### Grab the data

params = read.csv("CulParams.csv") |> 
  mutate(ParameterUnits = paste0(Parameter, " (", Units, ")"))

dat <- readRDS("CulWQ_clean-2024-05-18.rds") %>%
  # μ character wasn't read correctly from CulParams.csv so removed it
  mutate(Param = ifelse(Param == "Surface.Conductivity..μS.cm.", "Surface.Conductivity",
                        ifelse(Param == "Bottom.Conductivity..μS.cm.", "Bottom.Conductivity", Param))) %>% 
  select(-Units) %>%
  arrange(Location, Site.ID, Date) %>%
  left_join(params) %>%
  filter(Parameter != "Turbidity hach") # no non missing values

points <- readRDS("Cul_wqpoints.rds") %>% 
  arrange(Location, Site.ID)

# Shiny UI
ui <- page_fillable(
  title = "Culebra WQ",
  layout_columns(
    h4("Culebra Water Quality Dashboard"),
    h4(" "),
    actionButton("DTshow", "Show Filtered Data"),
    col_widths = c(6, 4, 2)
  ),
  layout_columns(
    selectInput(inputId = 'Locsel', label = 'Location', choices = c('Nearshore', 'Watershed'), selected = 'Nearshore'),
    selectInput(inputId = 'Levelsel', label = 'Sample Level', choices = NULL),
    selectInput(inputId = 'Parmsel', label = 'Parameter', choices = NULL)
  ),
  layout_columns(
    card(full_screen = TRUE, leafletOutput('map')),
    card(full_screen = TRUE, plotlyOutput('plo'))
  )
)

# Shiny server
server <- function(input, output, session){
  
  datSub1 <- reactive({
    filter(dat, Location == input$Locsel)
  })
  
  observeEvent(input$Locsel, {
    sl = sort(unique(datSub1()$SampleLevel), decreasing = TRUE)
    freezeReactiveValue(input, "Levelsel")
    updateSelectInput(session, 'Levelsel', choices = sl)
  })
  
  datSub2 <- reactive({
    req(input$Levelsel)
    filter(datSub1(), SampleLevel == input$Levelsel)
  })
  
  observe({
    parms = sort(unique(datSub2()$ParameterUnits))
    freezeReactiveValue(input, "Parmsel")
    updateSelectInput(session, 'Parmsel', choices = parms)
  })
  
  datSub3 <- reactive({
    req(input$Parmsel)
    filter(datSub2(), ParameterUnits == input$Parmsel)
  })
  
  output$table <- DT::renderDataTable({
    datSub3()
  }, extensions = "Buttons",
  options = list(searching = TRUE, bPaginate = FALSE, info = TRUE, scrollX = TRUE,
                 dom = "Bfrtip", buttons = c("copy", "csv", "excel")))
  
  observeEvent(input$DTshow, {
    showModal(modalDialog(
      DT::dataTableOutput("table"),
      easyClose = TRUE,
      footer = NULL, 
      size = "l"
    ))
  })
  
  output$plo <- renderPlotly({
    p = ggplot(datSub3(), aes(x = Date, y = Result, color = Site.ID)) + 
      geom_point() +
      geom_smooth(method = "loess", fill = NA) + 
      labs(x = "", y = input$Parmsel, color = "Site") +
      theme_bw()
    
    ggplotly(p)
  })
  
  datBubble <- reactive({
    left_join(datSub3(), points, by = c("Location", "Site.ID")) %>% 
      group_by(Site.ID, Latitude, Longitude, Parameter, Units) %>%
      summarize(MedValue = median(Result, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Popup = paste(Parameter, "<br>", MedValue, Units))
  })
  
  mypalette <- reactive({
    colorNumeric(palette = "YlOrBr", domain = datBubble()$MedValue)
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(attributionControl = FALSE)) %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      setView(lat=18.313, lng=-65.273, zoom=13)
  })
  
  observe({
    leafletProxy("map")%>%
      clearShapes() %>%
      clearControls() %>%
      addCircleMarkers(data = datBubble(), lng = ~Longitude, lat = ~Latitude, label = ~Site.ID, popup = ~Popup,
                       fillColor = ~mypalette()(MedValue), fillOpacity = 0.7, color = "black", stroke = FALSE) %>%
      addLegend("bottomright", pal = mypalette(), values = datBubble()$MedValue, title = paste("Median<br>", input$Parmsel))  
  })
  
}

# run app
shinyApp(ui = ui, server = server)  

#library(rsconnect)
#rsconnect::deployApp('C:/Wessel/Git_projects/SBEP_Temp/shinytemp/')
