observe({
  # if (input$nCondition | input$pCorr) {
  if (input$nHiera) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "dis")
  }
})

output$dis_group <- renderUI({
  pickerInput(
    inputId = "dis_group", label = "Select Samples:", choices = dds()$samples %>% as.character,
    selected = dds()$samples %>% as.character,
    multiple = T, width = "100%", options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 5)
  )
})

output$dis_color_pal <- renderPlot({
  par(mar=c(0,0,0,0))
  display.brewer.pal(n = 9, name = input$dis_color)
})

sampleDists <- eventReactive(input$plot_dis, {
  data <- assay(trans_value())[, input$dis_group]
  sampleDists <- data %>% t %>% dist
  sampleDistMatrix <- as.matrix(sampleDists)
  rownames(sampleDistMatrix) <- trans_value()[, input$dis_group]$samples
  colnames(sampleDistMatrix) <- trans_value()[, input$dis_group]$samples
  colors <- colorRampPalette( rev(brewer.pal(9, input$dis_color)) )(100)
  pheatmap(sampleDistMatrix, show_colnames = input$dis_colname,
           clustering_distance_rows=sampleDists, clustering_distance_cols=sampleDists,
           col=colors, fontsize = input$dis_fontsize, display_numbers = input$dis_number,
           fontsize_number = input$dis_fontsize_number, treeheight_row = input$dis_treeheight_row,
           treeheight_col = input$dis_treeheight_col)
})

output$sampleDists <- renderPlot({
  sampleDists()
})

output$dis_plotUI <- renderUI({
  withSpinner(plotOutput("sampleDists", width = paste0(input$dis_plot_width, "%"), height = paste0(input$dis_plot_height, "px")))
})

output$dis_Pdf <- downloadHandler(
  filename = function()  {paste0("Sample distances heatmap",".pdf")},
  content = function(file) {
    p <- sampleDists()
    ggsave(file, p, width = input$dis_width, height = input$dis_height)
  }
)
