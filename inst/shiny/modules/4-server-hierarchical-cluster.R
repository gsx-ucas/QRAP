observe({
  # if (input$nCondition | input$pCorr) {
  if (input$nPCA | input$pdis) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "hiera")
  }
})

output$hiera_samples <- renderUI({
  pickerInput(
    inputId = "hiera_samples", label = "Select Samples:", choices = dds()$samples %>% as.character,
    selected = dds()$samples %>% as.character, multiple = T, width = "100%", 
    options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 5)
  )
})

output$hiera_ancol <- renderUI({
  pickerInput(
    inputId = "hiera_ancol", label = "Select Varables as column annotation:",
    choices = colnames(dds()@colData)[!colnames(dds()@colData) %in% c("sizeFactor", "replaceable", "samples")],
    selected = "condition", multiple = T, width = "100%", 
    options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 5)
  )
})

topVarGene_heatmap <- eventReactive(input$plot_hiera, {
  topVarGenes <- trans_value()[, input$hiera_samples] %>% 
    SummarizedExperiment::assay() %>% MatrixGenerics::rowVars() %>% order(decreasing=TRUE) %>% head(input$hiera_topn)
  topVarAssay <- SummarizedExperiment::assay(trans_value())[topVarGenes, input$hiera_samples]
  
  color = grDevices::colorRampPalette(strsplit(input$hiera_color, ",")[[1]])(100)
  
  if (!is.null(input$hiera_ancol)) {
    annotation_col <- as.data.frame(row.names = input$hiera_samples, trans_value()@colData[input$hiera_samples, input$hiera_ancol])
    colnames(annotation_col) <- input$hiera_ancol
    annotation_colors <- set_anno_color(anno_row = NULL, anno_col = annotation_col)
  }else {
    annotation_col <- NA
    annotation_colors <- NA
  }

  pheatmap::pheatmap(topVarAssay, col=color,
           annotation_col=annotation_col,
           annotation_colors = annotation_colors,
           cutree_rows = input$hiera_cutree,
           cutree_cols = input$hiera_cutree_cols,
           cluster_rows = input$hiera_cluster_rows,
           fontsize_col = input$hiera_fontsize_col,
           scale = "row", fontsize = input$hiera_fontsize,
           show_rownames = F, show_colnames=input$hiera_colname,
           clustering_distance_rows = input$hiera_dist_method,
           clustering_distance_cols = input$hiera_dist_method,
           clustering_method = input$hiera_hclust_method,
           treeheight_row = input$hiera_treeheight_row,
           treeheight_col = input$hiera_treeheight_col,
           angle_col = input$hiera_angle %>% as.integer)
})

output$topVar_Plot <- renderPlot({
  topVarGene_heatmap()
})

output$hiera_plotUI <- renderUI({
  shinycssloaders::withSpinner(plotOutput("topVar_Plot", width = paste0(input$hiera_plot_width, "%"), height = paste0(input$hiera_plot_height, "px")))
})

output$hiera_Pdf <- downloadHandler(
  filename = function()  {paste0("topVarGene heatmap",".pdf")},
  content = function(file) {
    p <- topVarGene_heatmap()
    ggplot2::ggsave(file, p, width = input$hiera_width, height = input$hiera_height)
  }
)
