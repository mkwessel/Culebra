
page_fillable(
  title = "Culebra WQ",
  h4("Culebra Water Quality Dashboard"),
  layout_columns(
    selectInput(inputId = 'Locsel', label = 'Location', choices = c('Nearshore', 'Watershed'), selected = 'Nearshore'),
    conditionalPanel(condition = 'input.Locsel == "Nearshore"',
                     selectInput(inputId = 'Levelsel', label = 'Sample Level', 
                                 choices = c("Surface", "Bottom", "N/A"))),
    selectInput(inputId = 'Parmsel', label = 'Parameter', choices = NULL),
    actionButton("DTshow", "Show Filtered Data", class = "m-4"), # m-4 increases margins to align with other inputs in that row
    col_widths = c(3, 3, 3, 3)
  ),
  layout_columns(
    card(full_screen = TRUE, leafletOutput('map')),
    card(full_screen = TRUE, plotlyOutput('plo'))
  )
)