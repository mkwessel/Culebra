
page_sidebar(
  title = "Culebra Water Quality Dashboard",
  window_title = "Culebra WQ",
  sidebar = sidebar(
    selectInput(inputId = 'environment', label = "Environment", choices = c('Nearshore', 'Watershed'), selected = 'Nearshore'),
    conditionalPanel(condition = 'input.environment == "Nearshore"',
                     selectInput(inputId = 'level', label = 'Sample Level', 
                                 choices = c("Surface", "Bottom", "N/A"))),
    selectInput(inputId = 'parameter', label = 'Parameter', choices = NULL)
  ),
  navset_card_underline(
    nav_panel("Time Series Plot", plotlyOutput("tsPlot")),
    nav_panel("Box Plot", p("Coming soon...")),
    nav_panel("Tile Plot", p("Coming soon...")),
    nav_panel("Map", leafletOutput("map"))
  )
  # layout_columns(
  # 
  #   actionButton("show_dt", "Show Filtered Data", class = "m-4"), # m-4 increases margins to align with other inputs in that row
  #   col_widths = c(3, 3, 3, 3)
  # ),
)