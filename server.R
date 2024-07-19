shinyServer(function(input, output, session) {
  
  rv <- reactiveValues(last_param = NULL)
  
  statFN <- reactive({
    fn = c("Minimum" = min, "Median" = median,
           "Mean" = mean, "Maximum" = max)
    fn[[input$stat]]
  })
  
  # Filter ------------------------------------------------------------------
  
  datSub1 <- reactive({
    if (input$environment == "Watershed"){
      ws
    } else {
      filter(ns, SampleLevel == input$level & Group %in% input$group)
    }
  })
  
  observe({
    params = sort(unique(datSub1()$Parameter))
    if (is.null(rv$last_param) || input$parameter != rv$last_param) rv$last_param = input$parameter
    sel = if (rv$last_param %in% params) rv$last_param else params[1]
    updateSelectInput(session, 'parameter', choices = params, selected = sel)
    
    if (input$environment == "Watershed"){
      stns = ws_stations
      sel = ws_stations
    } else {
      stns = lapply(input$group, function(x) sort(ns_grps$Station[ns_grps$Group == x])) |> 
        setNames(input$group)
      sel = unlist(stns)
    }
    freezeReactiveValue(input, "stations")
    updatePickerInput(session, "stations", choices = stns, selected = sel)
  })
  
  datSub2 <- reactive({
    filter(datSub1(), Station %in% input$stations)
  })
  
  minDate <- reactive({
    req(datSub2())
    floor_date(min(datSub2()$Date, na.rm = TRUE), unit = "month")
  })
  
  maxDate <- reactive({
    req(datSub2())
    ceiling_date(max(datSub2()$Date, na.rm = TRUE), unit = "month")
  })
  
  dateSeq <- reactive({
    req(nrow(datSub2()) > 0)
    seq(from = minDate(), to = maxDate(), by = "1 month")
  })
  
  dateLab <- reactive({
    req(nrow(datSub2()) > 0)
    format(dateSeq(), "%b %Y")
  })
  
  observe({
    freezeReactiveValue(input, "date_range")
    updateSliderTextInput(session, inputId = "date_range", choices = dateLab(),
                          selected = dateLab()[dateSeq() %in% c(minDate(), maxDate())])
  })
  
  selDates <- reactive({
    req(input$date_range)
    c(dateSeq()[dateLab() == input$date_range[1]],
      dateSeq()[dateLab() == input$date_range[2]])
  })
  
  datSub3 <- reactive({
    filter(datSub2(), Date >= selDates()[1] & Date <= selDates()[2])
  })
  
  datSub4 <- reactive({
    req(input$parameter)
    filter(datSub3(), Parameter == input$parameter)
  })
  
  # Time Series Plot --------------------------------------------------------
  
  output$tsPlot <- renderPlotly({
    if (input$environment == "Watershed"){
      p = ggplot(datSub4(), aes(x = Date, y = Value, color = Station)) +
        scale_color_manual(values = ws_colors)
    } else {
      p = ggplot(datSub4(), aes(x = Date, y = Value, color = Group, linetype = Station)) +
        scale_color_manual(values = ns_grp_colors)
    }
    p = p + 
      geom_point() +
      geom_line() +
      labs(x = "", y = input$parameter) +
      theme_bw()
    
    ggplotly(p)
  })
  
  # Box Plot ----------------------------------------------------------------
  
  output$boxPlot <- renderPlotly({
    if (input$environment == "Watershed"){
      p = ggplot(datSub4(), aes(x = Station, y = Value, fill = Station, col = Station)) +
        scale_color_manual(values = ws_colors) +
        scale_fill_manual(values = ws_colors)
    } else {
      p = ggplot(datSub4(), aes(x = Group, y = Value, fill = Group, col = Group)) +
        scale_color_manual(values = ns_grp_colors) +
        scale_fill_manual(values = ns_grp_colors)
    }
    
    p = p + 
      geom_boxplot(alpha = 0.3) +
      labs(x = "", y = input$parameter) +
      theme_bw()
    
    ggplotly(p)
  })
  
  # Tile Plot ---------------------------------------------------------------
  
  datSub3Summ <- reactive({
    # includes all parameters
    if (input$environment == "Watershed") {
      ds = group_by(datSub3(), Station, Parameter) 
    } else {
      ds = group_by(datSub3(), Group, Station, Parameter) 
    }
    summarise(ds, Value = statFN()(Value, na.rm = TRUE))
  })
  
  tileSumm <- reactive({
    # includes all parameters
    if (input$environment == "Watershed") {
      ds = group_by(datSub3(), Station, Parameter) 
    } else {
      ds = group_by(datSub3(), Group, Station, GroupStation, Parameter) 
    }
    summarise(ds, Value = statFN()(Value, na.rm = TRUE)) |> 
      group_by(Parameter) |> 
      mutate(PropMax = Value/max(Value, na.rm = TRUE))
  })
  
  output$tilePlot <- renderPlotly({
    if (input$environment == "Watershed"){
      p = ggplot(tileSumm(), aes(y = Station, x = Parameter, fill = PropMax))
    } else {
      p = ggplot(tileSumm(), aes(y = GroupStation, x = Parameter, fill = PropMax))
    }
    p = p +
      geom_tile() +
      scale_fill_viridis_c() +
      scale_y_discrete(limits = rev) +
      labs(x = "", y = "", fill = "") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 0.5))
    
    ggplotly(p)
  })
  
  # Map ---------------------------------------------------------------------
  
  mapData <- reactive({
    if (input$environment == "Watershed") {
      ds = group_by(datSub4(), Station, Parameter) 
    } else {
      ds = datSub4() |> 
        left_join(data.frame(Group = names(ns_grp_colors),
                             Color = unname(ns_grp_colors))) |> 
        group_by(Group, Station, Parameter, Color) 
    }
    summarise(ds, Value = statFN()(Value, na.rm = TRUE)) |>
      mutate(Popup = paste(Parameter, "<br>", Value)) |> 
      left_join(station_locations) 
  })
  
  wsPalette <- reactive({
    colorNumeric(palette = "Spectral", domain = mapData()$Value, reverse = TRUE)
  })
  
  polyColor <- reactive({
    if (is.null(input$map_tile) || input$map_tile == "Topo") "black" else "white" 
  })
  
  nsSymbols <- reactive({
    md = mapData()
    makeSymbolsSize(values = md$Value, shape = 'circle', color = md$Color,
                    fillColor = md$Color, opacity = 0.7, baseSize = 20)
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(attributionControl = FALSE)) |>
      addTiles() |>
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") |>
      setView(lat = 18.31, lng = -65.28, zoom = 14) |>
      addLayersControl(baseGroups = c("Topo", "Imagery"),
                       options = layersControlOptions(collapsed = FALSE)) |>
      # https://stackoverflow.com/a/64578945
      htmlwidgets::onRender("
      function(el, x) {
        var myMap = this;
        myMap.on('baselayerchange',
          function (e) {
            Shiny.onInputChange('map_tile', e.layer.groupname)
        })}")
  })
  
  observe({
    input$panel # take a dependency on panel changes
    leafletProxy("map")|>
      clearShapes() |>
      clearMarkers() |>
      clearControls() |>
      addPolygons(data = drainages, label = ~Name, color = polyColor(), weight = 1, fillOpacity = 0.05)
    
    if (input$environment == "Watershed"){
      leafletProxy("map")|>
        addCircleMarkers(data = mapData(), lng = ~Longitude, lat = ~Latitude, label = ~Station, popup = ~Popup,
                         fillColor = ~wsPalette()(Value), fillOpacity = 0.9, stroke = FALSE) |> 
        addLegend("bottomright", pal = wsPalette(), values = mapData()$Value, 
                  title = paste(input$stat, "<br>", input$parameter))
    }
    if (input$environment == "Nearshore"){
      md = mapData()
      leafletProxy("map")|>
        addMarkers(data = md, icon = nsSymbols(), lng = ~Longitude, lat = ~Latitude, label = ~Station, popup = ~Popup) |>
        addLegendSize(position = "bottomleft", values = md$Value, color = "black", fillColor = "black",
                      opacity = 0.7, title = input$parameter, shape = "circle", orientation = "horizontal", 
                      numberFormat = function(x) prettyNum(x, big.mark = ",", scientific = FALSE, digits = 3),
                      breaks = 4) |> 
        addLegend(title = "Treatment Group", position = "bottomright",
                  colors = ns_grp_colors, labels = names(ns_grp_colors))
    }
    
    
  })
  
  # Table/Download -------------------------------------------------------------------
  
  output$table <- DT::renderDataTable({
    if (input$environment == "Watershed") datSub3() else select(datSub3(), -GroupStation)
  }, options = list(searching = TRUE, bPaginate = TRUE, info = TRUE, scrollX = TRUE))
  
  output$downloadFilteredData <- downloadHandler(
    filename = function() {
      paste0("CulebraWQ-FilteredData-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(datSub3(), file, row.names = FALSE)
    }
  )
  
  output$downloadAllData <- downloadHandler(
    filename = function() {
      paste0("CulebraWQ-AllData-", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(list("Nearshore" = select(ns, -GroupStation), "Watershed" = ws), file)
    }
  )
  
})