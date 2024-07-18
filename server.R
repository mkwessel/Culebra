shinyServer(function(input, output, session) {
  
  datSub1 <- reactive({
    if (input$environment == "Watershed"){
      ws
    } else {
      filter(ns, SampleLevel == input$level)
    }
  })
  
  observe({
    if (input$environment == "Watershed"){
      stns = ws_stations
      sel = ws_stations
    } else {
      stns = ns_stations
      sel = unlist(ns_stations)
    }
    freezeReactiveValue(input, "stations")
    updatePickerInput(session, "stations", choices = stns, selected = sel)
  })
  
  datSub2 <- reactive({
    filter(datSub1(), Station %in% input$stations)
  })
  
  observe({
    parms = sort(unique(datSub2()$Parameter))
    freezeReactiveValue(input, "parameter")
    updateSelectInput(session, 'parameter', choices = parms)
  })
  
  datSub3 <- reactive({
    req(input$parameter)
    filter(datSub2(), Parameter == input$parameter)
  })
  
  output$table <- DT::renderDataTable({
    select(datSub3(), -Qual)
  }, extensions = "Buttons",
  options = list(searching = TRUE, bPaginate = FALSE, info = TRUE, scrollX = TRUE,
                 dom = "Bfrtip", buttons = c("copy", "csv", "excel")))
  
  observeEvent(input$show_dt, {
    showModal(modalDialog(
      DT::dataTableOutput("table"),
      easyClose = TRUE,
      footer = NULL, 
      size = "l"
    ))
  })
  
  output$tsPlot <- renderPlotly({
    if (input$environment == "Watershed"){
      p = ggplot(datSub3(), aes(x = Date, y = Value, color = Station))
    } else {
      p = ggplot(datSub3(), aes(x = Date, y = Value, color = Group, group = Station)) +
        scale_color_manual(values = ns_grp_colors)
    }
    p = p + 
      geom_point() +
      geom_line() +
      labs(x = "", y = input$parameter) +
      theme_bw()

    ggplotly(p)
  })
  
  datBubble <- reactive({
    left_join(datSub3(), points, by = join_by(Station)) |> 
      group_by(Station, Latitude, Longitude, Parameter) |>
      summarize(MedValue = median(Value, na.rm = TRUE), .groups = 'drop') |>
      mutate(Popup = paste(Parameter, "<br>", MedValue))
  })
  
  mypalette <- reactive({
    colorNumeric(palette = "YlOrBr", domain = datBubble()$MedValue)
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(attributionControl = FALSE)) |>
      addTiles() |>
      addProviderTiles(providers$Esri.WorldImagery) |>
      setView(lat=18.313, lng=-65.273, zoom=13)
  })
  
  observe({
    leafletProxy("map")|>
      clearMarkers() |>
      clearControls() |>
      addCircleMarkers(data = datBubble(), lng = ~Longitude, lat = ~Latitude, label = ~Station, popup = ~Popup,
                       fillColor = ~mypalette()(MedValue), fillOpacity = 0.7, color = "black", stroke = FALSE) |>
      addLegend("bottomright", pal = mypalette(), values = datBubble()$MedValue, title = paste("Median<br>", input$parameter))  
  })
  
})