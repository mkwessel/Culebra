
page_navbar(
  title = "NOAA Culebra LBSP",
  window_title = "Culebra LBSP",
  id = "nav",
  sidebar = sidebar(
    id = "sidebar",
    width = 320,
    open = FALSE,
    conditionalPanel(
      condition = 'input.nav == "Dashboard"',
      selectInput(inputId = 'dataset', label = "Dataset", 
                  choices = c("Routine Water Quality", "Special Nutrient Collection", "Seagrass"), 
                  selected = "Routine Water Quality"),
      conditionalPanel(condition = 'input.dataset != "Seagrass"',
                       selectInput(inputId = 'location', label = "Location", 
                                   choices = c("Nearshore", "Watershed"), selected = "Nearshore")),
      conditionalPanel(condition = 'input.dataset != "Seagrass" & input.location == "Nearshore"',
                       selectInput(inputId = 'level', label = 'Sample Level', 
                                   choices = c("Surface", "Bottom", "N/A"))),
      conditionalPanel(condition = 'input.panel != "Tile Plot"',
                       selectInput(inputId = 'parameter', label = 'Parameter', choices = NULL)),
      conditionalPanel(condition = 'input.dataset == "Seagrass" | input.location == "Nearshore"',
                       pickerInput(inputId = "group", label = "Treatment Group", multiple = TRUE, 
                                   choices = names(ns_grp_colors), selected = names(ns_grp_colors))),
      pickerInput(inputId = "stations", label = "Stations", multiple = TRUE, choices = ns_stations, 
                  selected = unlist(ns_stations), options = list(`actions-box` = TRUE, size = 8)),
      conditionalPanel(condition = 'input.dataset != "Seagrass"',
                       sliderTextInput(inputId = "date_range", label = "Date Range", choices = c("Jan 2023", "May 2024"), 
                                       selected = c("Jan 2023", "May 2024"))),
      conditionalPanel(condition = 'input.dataset == "Seagrass"',
                       radioButtons("sg_year", label = "Year", inline = TRUE, 
                                    choices = c(2022, 2014), selected = 2022)),
      conditionalPanel(condition = 'input.panel == "Bar Plot" | input.panel == "Tile Plot" | input.panel == "Map"',
                       selectInput(inputId = "stat", "Statistic", choices = c("Minimum", "Median", "Maximum"),
                                   selected = "Median")),
      downloadButton("downloadFilteredData", "Download Filtered Data"),
      downloadButton("downloadAllData", "Download All Data")
    )
  ),
  nav_panel("About",
            card(
              max_height = "190px",
              HTML('<p>Welcome to the Culebra Land Based Sources of Pollution (LBSP) Dashboard. 
              This dashboard displays results of environmental monitoring data 
              associated with the National Oceanic and Atmospheric Administration’s (NOAA) 
              efforts to reduce LBSP threats to the island of Culebra and its nearshore environment 
              (<a href="https://the-culebra-project-horsleywitten.opendata.arcgis.com/">Culebra LBSP Ridge to Reef Monitoring Program</a>). 
              The dashboard is designed to allow the user to examine specific datasets of interest 
              (i.e., Water Quality, Nutrients, Seagrass). The user chooses a subset of data for display by selecting Location, 
              Sample Level, Parameter, Treatment Group, and Station and adjusting the date range 
              to specific temporal periods of interest. These interactive features allow for 
              data exploration and quality control checks. The dashboard is continuously updated as new 
              data are entered into the data portal. We hope you find this dashboard useful in exploring environmental 
              monitoring data being collected on the island of Culebra.</p>')
            ),
            layout_column_wrap(
              card(
                p("The display options in the dashboard include:"),
                HTML('<ul>
            <li>Time series plots of the raw values of the selected parameter by station.</li>
            <li>Box plots displaying the distribution of the raw values of the selected parameter by treatment group or station.</li>
            <li>Bar plots of the summary values of the selected parameter by station.</li>
            <li>Tile plots that display all parameters on a relative scale across stations based on the summary values.</li>
            <li>Georeferenced bubble plots of the summary values of the selected parameter at each station.</li>
            <li>Tables displaying the raw values for the selected subset of data.</li>
                 </ul>'),
                p("Hovering the cursor over the plots provides the data value at the location of the cursor. 
                            All data shown in the plots is available for download via buttons at the bottom of the sidebar.")
              ),
              card(HTML('<center><img src="Screenshot.png", alt = "Screenshot of Culebra LBSP Dashboard", 
                                         width = "665.6", height = "383.5"></center>'))
            )
            
            
  ), 
  nav_panel("Dashboard",
            navset_card_underline(
              id = "panel",
              title = "All data are provisional and subject to revision",
              nav_panel("Time Series Plot", plotlyOutput("tsPlot")),
              nav_panel("Box Plot", plotlyOutput("boxPlot")),
              nav_panel("Bar Plot", plotlyOutput("barPlot")),
              nav_panel("Tile Plot", plotlyOutput("tilePlot")),
              nav_panel("Map", leafletOutput("map")),
              nav_panel("Table", DT::dataTableOutput("table")),
            )
  )
)