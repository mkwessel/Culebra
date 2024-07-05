shinyServer(function(input, output, session) {
  
  datSub1 <- reactive({
    filter(dat, Location == input$Locsel)
  })
  
  observeEvent(input$Locsel, {
    sl = sort(unique(datSub1()$SampleLevel), decreasing = TRUE)
    freezeReactiveValue(input, "Levelsel")
    updateSelectInput(session, 'Levelsel', choices = sl)
  })
  
  datSub2 <- reactive({
    req(input$Levelsel)
    filter(datSub1(), SampleLevel == input$Levelsel)
  })
  
  observe({
    parms = sort(unique(datSub2()$ParameterUnits))
    freezeReactiveValue(input, "Parmsel")
    updateSelectInput(session, 'Parmsel', choices = parms)
  })
  
  datSub3 <- reactive({
    req(input$Parmsel)
    filter(datSub2(), ParameterUnits == input$Parmsel)
  })
  
  output$table <- DT::renderDataTable({
    datSub3()
  }, extensions = "Buttons",
  options = list(searching = TRUE, bPaginate = FALSE, info = TRUE, scrollX = TRUE,
                 dom = "Bfrtip", buttons = c("copy", "csv", "excel")))
  
  observeEvent(input$DTshow, {
    showModal(modalDialog(
      DT::dataTableOutput("table"),
      easyClose = TRUE,
      footer = NULL, 
      size = "l"
    ))
  })
  
  output$plo <- renderPlotly({
    p = ggplot(datSub3(), aes(x = Date, y = Result, color = Site)) + 
      geom_point() +
      geom_line() +
      # geom_smooth(method = "loess", fill = NA) + 
      labs(x = "", y = input$Parmsel) +
      theme_bw()
    
    ggplotly(p)
  })
  
  datBubble <- reactive({
    left_join(datSub3(), points, by = c("Location", "Site")) %>% 
      group_by(Site, Latitude, Longitude, Parameter, Units) %>%
      summarize(MedValue = median(Result, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Popup = paste(Parameter, "<br>", MedValue, Units))
  })
  
  mypalette <- reactive({
    colorNumeric(palette = "YlOrBr", domain = datBubble()$MedValue)
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(attributionControl = FALSE)) %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      setView(lat=18.313, lng=-65.273, zoom=13)
  })
  
  observe({
    leafletProxy("map")%>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(data = datBubble(), lng = ~Longitude, lat = ~Latitude, label = ~Site, popup = ~Popup,
                       fillColor = ~mypalette()(MedValue), fillOpacity = 0.7, color = "black", stroke = FALSE) %>%
      addLegend("bottomright", pal = mypalette(), values = datBubble()$MedValue, title = paste("Median<br>", input$Parmsel))  
  })
  
})