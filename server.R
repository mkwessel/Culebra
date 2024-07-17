shinyServer(function(input, output, session) {
  
  datSub1 <- reactive({
    filter(dat, Location == input$environment)
  })
  
  datSub1 <- reactive({
    if (input$environment == "Watershed"){
      ws
    } else {
      filter(ns, SampleLevel == input$level)
    }
    
  })
  
  observe({
    parms = sort(unique(datSub1()$Parameter))
    freezeReactiveValue(input, "parameter")
    updateSelectInput(session, 'parameter', choices = parms)
  })
  
  datSub2 <- reactive({
    req(input$parameter)
    filter(datSub1(), Parameter == input$parameter)
  })
  
  output$table <- DT::renderDataTable({
    select(datSub2(), -Qual)
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
    p = ggplot(datSub2(), aes(x = Date, y = Value, color = Station)) + 
      geom_point() +
      geom_line() +
      labs(x = "", y = input$parameter) +
      theme_bw()
    
    ggplotly(p)
  })
  
  datBubble <- reactive({
    left_join(datSub2(), points, by = join_by(Station)) |> 
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