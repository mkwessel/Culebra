
page_sidebar(
  title = "Culebra Water Quality Dashboard",
  window_title = "Culebra WQ",
  sidebar = sidebar(
    width = 300,
    selectInput(inputId = 'environment', label = "Environment", choices = c('Nearshore', 'Watershed'), selected = 'Nearshore'),
    conditionalPanel(condition = 'input.environment == "Nearshore"',
                     selectInput(inputId = 'level', label = 'Sample Level', 
                                 choices = c("Surface", "Bottom", "N/A"))),
    conditionalPanel(condition = 'input.panel != "Tile Plot"',
                     selectInput(inputId = 'parameter', label = 'Parameter', choices = NULL)),
    conditionalPanel(condition = 'input.environment == "Nearshore"',
                     pickerInput(inputId = "group", label = "Treatment Group", multiple = TRUE, 
                                 choices = names(ns_grp_colors), selected = names(ns_grp_colors))),
    pickerInput(inputId = "stations", label = "Stations", multiple = TRUE, choices = ns_stations, selected = unlist(ns_stations),
                options = list(`actions-box` = TRUE, size = 8)),
    sliderTextInput(inputId = "date_range", label = "Date Range", choices = c("Jan 2023", "May 2024"), 
                    selected = c("Jan 2023", "May 2024")),
    conditionalPanel(condition = 'input.panel == "Tile Plot" | input.panel == "Map"',
                     selectInput(inputId = "stat", "Statistic", choices = c("Minimum", "Median", "Mean", "Maximum"),
                                 selected = "Median")),
    conditionalPanel(condition = 'input.panel == "Table"',
                     downloadButton("downloadFilteredData", "Download Filtered Data"),
                     downloadButton("downloadAllData", "Download All Data"))
  ),
  navset_card_underline(
    id = "panel",
    title = "All data are provisional and subject to revision",
    nav_panel("Time Series Plot", plotlyOutput("tsPlot")),
    nav_panel("Box Plot", plotlyOutput("boxPlot")),
    nav_panel("Tile Plot", plotlyOutput("tilePlot")),
    nav_panel("Map", leafletOutput("map")),
    nav_panel("Table", DT::dataTableOutput("table")),
  )
)