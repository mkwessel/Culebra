
page_sidebar(
  title = "Culebra Water Quality Dashboard",
  window_title = "Culebra WQ",
  sidebar = sidebar(
    width = 300,
    selectInput(inputId = 'environment', label = "Environment", choices = c('Nearshore', 'Watershed'), selected = 'Nearshore'),
    pickerInput(inputId = "stations", label = "Stations", multiple = TRUE, choices = ns_stations, selected = unlist(ns_stations),
                options = list(`actions-box` = TRUE, size = 8)),
    conditionalPanel(condition = 'input.environment == "Nearshore"',
                     selectInput(inputId = 'level', label = 'Sample Level', 
                                 choices = c("Surface", "Bottom", "N/A"))),
    selectInput(inputId = 'parameter', label = 'Parameter', choices = NULL),
    conditionalPanel(condition = 'input.panel == "Tile Plot" | input.panel == "Map"',
                     selectInput(inputId = "stat", "Statistic", choices = c("Minimum", "Median", "Mean", "Maximum"),
                                 selected = "Median"))
  ),
  navset_card_underline(
    id = "panel",
    title = "All data are provisional and subject to revision",
    nav_panel("Time Series Plot", plotlyOutput("tsPlot")),
    nav_panel("Box Plot", plotlyOutput("boxPlot")),
    nav_panel("Tile Plot", p("Coming soon...")),
    nav_panel("Map", leafletOutput("map"))
  )
  # layout_columns(
  # 
  #   actionButton("show_dt", "Show Filtered Data", class = "m-4"), # m-4 increases margins to align with other inputs in that row
  #   col_widths = c(3, 3, 3, 3)
  # ),
)