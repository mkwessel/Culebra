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
library(tidyverse)
library(dplyr)
library(ggplot2)
library(conflicted)
library(lubridate)
library(leaflet)
library(RColorBrewer)

conflicts_prefer(dplyr::filter, dplyr::lag)

#### Grab the data

dat<-readRDS("CulWQ_clean-2024-05-18.rds")%>%arrange(Location,Site.ID,Date,Param)
points<-readRDS("Cul_wqpoints.rds")%>%arrange(Location,Site.ID)

# Join location and wq file
bubbles<-full_join(dat,points,by=c("Location","Site.ID"))   

# Generate medians for map bubble plots
meddat<-bubbles%>%
  group_by(Location,Site.ID,Param,Latitude,Longitude)%>%
  summarize(med_value=median(Result,na.rm=TRUE),.groups='drop')


#########################################
# Functions
#########################################
# timeseries plot function
#########################################

# plotting function which takes tidied data and filters based on ui selections
plotit <- function(Locc,Params,datin){
  mod <- dat%>%
    filter(Location %in% Locc &  Param %in% Params)
  
  mn<-min(mod$Result)-5
  mx=max(mod$Result)+5
  
  P <- ggplot(mod, aes(x = Date, y = Result,group=Site.ID, color = Site.ID)) + geom_point()+
    geom_smooth(method = "loess", fill = NA)+ ylim(mn,mx)+
    labs(
      title = "Trend in Water Quality over Time",
      x = "Date",
      y = "Value"
    ) 
  
  P + theme_bw()
  P
  
}

# List of unique values for input selection

Stat <- unique(dat$Site.ID)
Loc <-unique(dat$Location)
Parms <-unique(dat$Param)

# test secondary selection 
pullit<-dat%>%
  filter(Location=="Nearshore")
nsparam<-unique(pullit$Param)

pullit<-dat%>%
  filter(Location=="Watershed")
wsparam<-unique(pullit$Param)



# Shiny UI
ui <- fluidPage(
  titlePanel("Culebra Water Quality Dashboard"),
  tabsetPanel(
    tabPanel("Timeseries Plots", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = 'Locsel', label = 'Select Location', choices = Loc,
                             selected = 'Nearshore'),
                 selectInput(inputId = 'Parmsel', label = 'Select Parameter', choices = Parms,
                             selected = 'Surf.Turb')),         
               mainPanel( plotOutput('plo'))
             )
    ),
    
    tabPanel("Map Plots", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = 'Parmsel2', label = 'Select Parameter', choices = Parms,
                             selected = 'Surf.Turb')
               ),         
               mainPanel(leafletOutput(outputId = 'map'))
             )
    )
  )
)


# Shiny server
server <- function(input, output,session){
  output$plo <- renderPlot({
    plotit(
      Locc = input$Locsel,
      Params = input$Parmsel,
      datin = dat)
  })
  
  #should be a function to pass based on selection of location  
  #  observe({
  #    updateSelectInput(session, "Parmsel", choices = nsparam)
  #  }) 
  
  
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
ggplot2::theme_set(ggplot2::theme_bw())
shinyApp(ui = ui, server = server)  

#library(rsconnect)
#rsconnect::deployApp('C:/Wessel/Git_projects/SBEP_Temp/shinytemp/')
