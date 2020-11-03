observe({
  # if (input$nCondition | input$pCorr) {
  if (input$nPCA | input$pdis) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "hiera")
  }
})

output$hiera_group <- renderUI({
  pickerInput(
    inputId = "hiera_group", label = "Select Samples:", choices = dds()$samples %>% as.character,
    selected = dds()$samples %>% as.character,
    multiple = T, width = "100%", options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 5)
  )
})

topVarGene_heatmap <- eventReactive(input$plot_hiera, {
  topVarGenes <- trans_value()[, input$hiera_group] %>% assay %>% rowVars %>% order(decreasing=TRUE) %>% head(input$hiera_topn)
  topVarAssay <- assay(trans_value())[topVarGenes, input$hiera_group]
  # topVarAssay <- topVarAssay - rowMeans(topVarAssay)
  annotation_col = data.frame(Samples = colData(trans_value()[, input$hiera_group])$condition)
  rownames(annotation_col) = colData(trans_value()[, input$hiera_group])$samples
  color = colorRampPalette(strsplit(input$hiera_color, ",")[[1]])(100)
  if (isTRUE(input$hiera_annotation)) {
    pheatmap(topVarAssay, col=color,
             annotation_col=annotation_col,
             cutree_rows = input$hiera_cutree,
             cutree_cols = input$hiera_cutree_cols,
             cluster_rows = input$hiera_cluster_rows,
             scale = "row", fontsize = input$hiera_fontsize,
             show_rownames = F, show_colnames=input$hiera_colname,
             treeheight_row = input$hiera_treeheight_row,
             treeheight_col = input$hiera_treeheight_col,
             angle_col = input$hiera_angle %>% as.integer)
  }else {
    pheatmap(topVarAssay, col=color,
             cutree_rows = input$hiera_cutree,
             cutree_cols = input$hiera_cutree_cols,
             cluster_rows = input$hiera_cluster_rows,
             scale = "row", fontsize = input$hiera_fontsize,
             show_rownames = F, show_colnames=input$hiera_colname,
             treeheight_row = input$hiera_treeheight_row,
             treeheight_col = input$hiera_treeheight_col,
             angle_col = input$hiera_angle %>% as.integer)
  }
})

output$topVar_Plot <- renderPlot({
  topVarGene_heatmap()
})

output$hiera_plotUI <- renderUI({
  withSpinner(plotOutput("topVar_Plot", width = paste0(input$hiera_plot_width, "%"), height = paste0(input$hiera_plot_height, "px")))
})

output$hiera_Pdf <- downloadHandler(
  filename = function()  {paste0("topVarGene heatmap",".pdf")},
  content = function(file) {
    p <- topVarGene_heatmap()
    ggsave(file, p, width = input$hiera_width, height = input$hiera_height)
  }
)