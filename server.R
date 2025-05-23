shinyServer(function(input, output, session) {
  
  statFN <- reactive({
    fn = c("Minimum" = min, "Median" = median,
           "Mean" = mean, "Maximum" = max)
    fn[[input$stat]]
  })
  
  # Reactive Values ---------------------------------------------------------

  rv <- reactiveValues(ws = NULL,
                       sg = NULL,
                       nut = NULL,
                       nut_ws = NULL,
                       nut_ns = NULL,
                       last_param = NULL)
  
  observe({
    if (input$dataset == "Seagrass"){
      if (is.null(rv$sg)) rv$sg = get_sg(sg_url, sg_params, ns_grps)
    } 
    if (input$dataset == "Routine Water Quality" & input$location == "Watershed"){
      if (is.null(rv$ws)) rv$ws = prep_ws(ws_stations)
    }
    if (input$dataset == "Special Nutrient Collection"){
      if (is.null(rv$nut)) rv$nut = get_nut(nut_url)
    }
    if (input$dataset == "Special Nutrient Collection" & input$location == "Watershed"){
      if (is.null(rv$nut_ws)) rv$nut_ws = get_nut_ws(rv$nut, ws_stations)
    }
    if (input$dataset == "Special Nutrient Collection" & input$location == "Nearshore"){
      if (is.null(rv$nut_ns)) rv$nut_ns = get_nut_ns(rv$nut, ns_grps)
    }
  })
  
  observe({
    input$panel
    if (input$dataset == "Seagrass"){
      if (input$panel == "Time Series Plot") nav_select("panel", "Box Plot")
      nav_hide("panel", target = "Time Series Plot")
    } else {
      nav_show("panel", target = "Time Series Plot")
    }
  })
  
  # Filter ------------------------------------------------------------------
  
  datSub1 <- reactive({
    out = NULL
    if (input$dataset == "Seagrass") out = rv$sg[rv$sg[["Year"]] == input$sg_year,]
    if (input$dataset == "Routine Water Quality" & input$location == "Watershed") out = rv$ws
    if (input$dataset == "Routine Water Quality" & input$location == "Nearshore"){
      out = ns[ns[["SampleLevel"]] == input$level & ns[["Group"]] %in% input$group,]
    }
    if (input$dataset == "Special Nutrient Collection" & input$location == "Watershed") out = rv$nut_ws
    if (input$dataset == "Special Nutrient Collection" & input$location == "Nearshore"){
      out = rv$nut_ns[rv$nut_ns[["SampleLevel"]] == input$level & rv$nut_ns[["Group"]] %in% input$group,]
    }
    out
  })
  
  observe({
    req(datSub1())
    params = sort(unique(datSub1()$Parameter))
    if (is.null(rv$last_param) || input$parameter != rv$last_param) rv$last_param = input$parameter
    if (input$dataset == "Routine Water Quality" & input$level == "N/A") {
      dp = "Normalized KdPAR" 
    } else {
      dp = default_params[[input$dataset]]
    } 
    sel = if (rv$last_param %in% params) rv$last_param else dp
    updateSelectInput(session, 'parameter', choices = params, selected = sel)
    
    if (input$dataset == "Seagrass" | input$location == "Nearshore"){
      stns = lapply(input$group, function(x) sort(ns_grps$Station[ns_grps$Group == x])) |> 
        setNames(input$group)
      sel = unlist(stns)
    } else {
      stns = ws_stations
      sel = ws_stations
    }
    freezeReactiveValue(input, "stations")
    updatePickerInput(session, "stations", choices = stns, selected = sel)
  })
  
  datSub2 <- reactive({
    dfx = datSub1()
    dfx[dfx[["Station"]] %in% input$stations,]
  })
  
  minDate <- reactive({
    req(datSub2(), input$dataset != "Seagrass")
    floor_date(min(datSub2()[["Date"]], na.rm = TRUE), unit = "month")
  })
  
  maxDate <- reactive({
    req(datSub2(), input$dataset != "Seagrass")
    ceiling_date(max(datSub2()[["Date"]], na.rm = TRUE), unit = "month")
  })
  
  dateSeq <- reactive({
    req(nrow(datSub2()) > 0, input$dataset != "Seagrass")
    seq(from = minDate(), to = maxDate(), by = "1 month")
  })
  
  dateLab <- reactive({
    req(nrow(datSub2()) > 0, input$dataset != "Seagrass")
    format(dateSeq(), "%b %Y")
  })
  
  observe({
    freezeReactiveValue(input, "date_range")
    updateSliderTextInput(session, inputId = "date_range", choices = dateLab(),
                          selected = dateLab()[dateSeq() %in% c(minDate(), maxDate())])
  })
  
  selDates <- reactive({
    req(input$date_range, input$dataset != "Seagrass")
    c(dateSeq()[dateLab() == input$date_range[1]],
      dateSeq()[dateLab() == input$date_range[2]])
  })
  
  datSub3 <- reactive({
    req(input$dataset != "Seagrass")
    dfx = datSub2()
    dfx[dfx[["Date"]] >= selDates()[1] & dfx[["Date"]] <= selDates()[2],]
  })
  
  datSub4 <- reactive({
    req(input$parameter)
    dfx = if (input$dataset == "Seagrass") datSub2() else datSub3()
    dfx[dfx[["Parameter"]] == input$parameter,]
  })
  
  # Time Series Plot --------------------------------------------------------
  
  output$tsPlot <- renderPlotly({
    req(input$dataset != "Seagrass")
    if (input$location == "Watershed"){
      p = ggplot(datSub4(), aes(x = Date, y = Value, color = Station)) +
        scale_color_manual(values = ws_colors)
    } else {
      p = ggplot(datSub4(), aes(x = Date, y = Value, color = Group, linetype = Station)) +
        scale_color_manual(values = ns_grp_colors) +
        scale_linetype_manual(values = ns_grp_lts)
    }
    p = p + 
      geom_point() +
      geom_line(alpha = 0.6) +
      labs(x = "", y = input$parameter) +
      theme_bw()
    
    ggplotly(p)
  })
  
  # Box Plot ----------------------------------------------------------------
  
  output$boxPlot <- renderPlotly({
    if (input$dataset == "Seagrass" | input$location == "Nearshore"){
      p = ggplot(datSub4(), aes(x = Group, y = Value, fill = Group, col = Group)) +
        scale_color_manual(values = ns_grp_colors) +
        scale_fill_manual(values = ns_grp_colors)
    } else {
      p = ggplot(datSub4(), aes(x = Station, y = Value, fill = Station, col = Station)) +
        scale_color_manual(values = ws_colors) +
        scale_fill_manual(values = ws_colors)
    }
    
    p = p + 
      geom_boxplot(alpha = 0.3) +
      labs(x = "", y = input$parameter) +
      scale_x_discrete(limits = rev) +
      coord_flip() +
      theme_bw()
    
    ggplotly(p)
  })
  
  # Bar Plot ----------------------------------------------------------------
  
  barSumm <- reactive({
    if (input$dataset == "Seagrass" | input$location == "Nearshore") {
      ds = group_by(datSub4(), Station, Group, GroupStation) 
    } else {
      ds = group_by(datSub4(), Station)
    }
    summarise(ds, Value = statFN()(Value, na.rm = TRUE))
  })
  
  output$barPlot <- renderPlotly({
    if (input$dataset == "Seagrass" | input$location == "Nearshore"){
      p = ggplot(barSumm(), aes(y = Station, x = Value, fill = Group)) +
        scale_fill_manual(values = ns_grp_colors)
    } else {
      p = ggplot(barSumm(), aes(y = Station, x = Value, fill = Station)) +
        scale_fill_manual(values = ws_colors)
    }
    p = p +
      geom_col() +
      scale_y_discrete(limits = rev) +
      labs(x = input$parameter, y = "") +
      theme_bw() 
    
    ggplotly(p)
  })
  
  # Tile Plot ---------------------------------------------------------------
  
  tileSumm <- reactive({
    dfx = if (input$dataset == "Seagrass") datSub2() else datSub3()
    # includes all parameters
    if (input$dataset == "Seagrass" | input$location == "Nearshore") {
      ds = group_by(dfx, Group, Station, GroupStation, Parameter) 
    } else {
      ds = group_by(dfx, Station, Parameter)
    }
    summarise(ds, Value = statFN()(Value, na.rm = TRUE)) |> 
      group_by(Parameter) |> 
      mutate(Percentile = percentile(Value))
  })
  
  output$tilePlot <- renderPlotly({
    p = ggplot(tileSumm(), aes(y = Station, x = Parameter, fill = Percentile, label = Value)) +
      geom_tile() +
      scale_y_discrete(limits = rev) +
      scale_fill_gradient2(mid = "#f7f7f7", low = scales::muted("blue"), high = scales::muted("red"), midpoint = 50) +
      labs(x = "", y = "") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 0.5))

    ggplotly(p)
  })
  
  # Map ---------------------------------------------------------------------
  
  mapData <- reactive({
    req(datSub4())
    if (input$dataset == "Seagrass" | input$location == "Nearshore") {
      ds = datSub4() |> 
        left_join(data.frame(Group = names(ns_grp_colors),
                             Color = unname(ns_grp_colors))) |> 
        group_by(Group, Station, Parameter, Color) 
    } else {
      ds = group_by(datSub4(), Station, Parameter) 
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
    makeSymbolsSize(values = md$Value, 
                    shape = 'circle', 
                    color = "black",
                    weight = 1,
                    opacity = 1,
                    fillColor = md$Color,
                    fillOpacity = 0.8, 
                    baseSize = 20)
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(attributionControl = FALSE)) |>
      addTiles() |>
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo",
                       options = providerTileOptions(noWrap = TRUE,
                                                     maxNativeZoom = 18,
                                                     maxZoom = 20)) |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery",
                       options = providerTileOptions(noWrap = TRUE,
                                                     maxNativeZoom = 18,
                                                     maxZoom = 20)) |>
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
    drain = if (input$dataset == "Seagrass") drainages[["Nearshore"]] else drainages[[input$location]]
    leafletProxy("map")|>
      clearShapes() |>
      clearMarkers() |>
      clearControls() |>
      addPolygons(data = drain, label = ~Name, color = polyColor(), weight = 1, fillOpacity = 0.05)
    
    if (input$dataset == "Seagrass" | input$location == "Nearshore"){
      md = mapData()
      leafletProxy("map")|>
        addMarkers(data = md, icon = nsSymbols(), lng = ~Longitude, lat = ~Latitude, label = ~Station, popup = ~Popup) |>
        addLegendSize(position = "bottomleft", values = md$Value, color = "black", fillColor = "black",
                      opacity = 0.7, title = input$parameter, shape = "circle", orientation = "horizontal", 
                      numberFormat = function(x) prettyNum(x, big.mark = ",", scientific = FALSE, digits = 3),
                      breaks = 4) |> 
        addLegend(title = "Treatment Group", position = "bottomright",
                  colors = ns_grp_colors, labels = names(ns_grp_colors))
    } else {
      leafletProxy("map")|>
        addCircleMarkers(data = mapData(), 
                         lng = ~Longitude, 
                         lat = ~Latitude, 
                         label = ~Station, 
                         popup = ~Popup,
                         radius = 6,
                         color = "black",
                         weight = 1,
                         opacity = 1,
                         fillColor = ~wsPalette()(Value), 
                         fillOpacity = 0.8) |> 
        addLegend("bottomright", pal = wsPalette(), values = mapData()$Value, 
                  title = paste(input$stat, "<br>", input$parameter))
    }
  })
  
  # Table/Download -------------------------------------------------------------------
  
  tableDownload <- reactive({
    tmp = if (input$dataset == "Seagrass") datSub4() else mutate(datSub4(), Date = as.character(Date))
    if (input$dataset == "Seagrass" | input$location == "Nearshore") select(tmp, -GroupStation) else tmp
  })
  
  output$table <- DT::renderDataTable({
    tableDownload()
  }, options = list(searching = TRUE, bPaginate = TRUE, info = TRUE, scrollX = TRUE))
  
  loc <- reactive({
    if (input$dataset == "Seagrass") "" else paste0(input$location, "-")
  })
  
  output$downloadFilteredData <- downloadHandler(
    filename = function() {
      paste0("Culebra-FilteredData-", input$dataset, "-", loc(), Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(tableDownload(), file, row.names = FALSE)
    }
  )
  
  output$downloadAllData <- downloadHandler(
    filename = function() {
      paste0("Culebra-AllData-", input$dataset, "-", loc(), Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # previously was saving each dataset with two tabs (nearshore and watershed)
      # but if watershed hasn't been fetched would need to do that here
      # converting Date column to text b/c otherwise it will produce a file with UTC time zone, which could lead to confusion
      if (input$dataset == "Routine Water Quality" & input$location == "Nearshore"){
        writexl::write_xlsx(mutate(select(ns, -GroupStation), Date = as.character(Date)), file)
      } 
      if (input$dataset == "Routine Water Quality" & input$location == "Watershed"){
        writexl::write_xlsx(mutate(rv$ws, Date = as.character(Date)), file)
      } 
      if (input$dataset == "Special Nutrient Collection" & input$location == "Nearshore") {
        writexl::write_xlsx(mutate(select(rv$nut_ns, -GroupStation), Date = as.character(Date)), file)
      } 
      if (input$dataset == "Special Nutrient Collection" & input$location == "Watershed") {
        writexl::write_xlsx(mutate(rv$nut_ws, Date = as.character(Date)), file)
      } 
      if (input$dataset == "Seagrass") {
        writexl::write_xlsx(select(rv$sg, -GroupStation), file)
      } 
    }
  )
  
})