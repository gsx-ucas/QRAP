

output$wgcna_exp_trait <- renderUI({
  pickerInput(
    inputId = "wgcna_exp_trait", label = "Select trait to order samples:",
    choices = colnames(colData(dds()))[!colnames(colData(dds())) %in% c("sizeFactor", "replaceable", "samples")],
    selected = colnames(colData(dds()))[!colnames(colData(dds())) %in% c("sizeFactor", "replaceable", "samples")][1],
    multiple = F, width = "100%", options = list(`live-search` = TRUE, size = 5)
  )
})

output$wgcna_exp_module <- renderUI({
  pickerInput(
    inputId = "wgcna_exp_module", label = "Select interested module:",
    choices = moduleColors() %>% unique, selected = (moduleColors() %>% unique)[1],
    multiple = F, width = "100%", options = list(`live-search` = TRUE, size = 5)
  )
})

wgcna_expression <- eventReactive(input$wgcna_plot_exp, {
  sampleTable <- as.data.frame(colData(dds()))[rownames(datExpr()), ]

  sampleTable <- sampleTable[order(sampleTable[, input$wgcna_exp_trait]), ]

  sampleTable <- lapply(sampleTable[, input$wgcna_exp_trait] %>% unique, function(x){
    sampleTable[sampleTable[, input$wgcna_exp_trait] == x, ]
  }) %>% bind_rows()

  module_genes <- names(moduleColors())[moduleColors() == input$wgcna_exp_module]
  expression_df <- as.data.frame(assay(trans_value()))[module_genes, rownames(sampleTable)]

  if (input$wgcna_exp_ptype == "Pheatmap") {
    annotation_col = data.frame(row.names = rownames(sampleTable), V1 = sampleTable[, input$wgcna_exp_trait])
    colnames(annotation_col) <- input$wgcna_exp_trait

    color = colorRampPalette(strsplit(input$wgcna_hiera_color, ",")[[1]])(100)
    pheatmap(expression_df,
             scale = "row", show_rownames = F,treeheight_row = 20,
             annotation_col = annotation_col, cluster_cols = F, col = color,
             fontsize_col = input$wgcna_hiera_fontsize_col,
             fontsize = input$wgcna_hiera_fontsize, angle_col = input$wgcna_hiera_angle)
  }else {
    MEs0 = moduleEigengenes(datExpr(), moduleColors())$eigengenes
    MEs = orderMEs(MEs0)[rownames(sampleTable), ]

    ggplot(data = NULL)+
      geom_bar(aes(x = factor(rownames(MEs), levels = rownames(MEs)), y = MEs[, paste0("ME", input$wgcna_exp_module)]), stat = "identity", fill = input$wgcna_exp_module)+
      labs(x = "array samples", y = "eigengene expression")+
      theme_classic()+
      theme(text = element_text(size = input$wgcna_bar_cex),
            axis.title = element_text(size = input$wgcna_bar_lab),
            axis.text = element_text(size = input$wgcna_bar_axis, angle = input$wgcna_bar_ang, hjust = 1))
  }
})

output$wgcna_expression <- renderPlot({
  wgcna_expression()
})

output$wgcna_expressionUI <- renderUI({
  withSpinner(plotOutput("wgcna_expression", width = paste0(input$wgcna_expression_width, "%"), height = paste0(input$wgcna_expression_height, "px")))
})

output$wgcna_exp_Pdf <- downloadHandler(
  filename = function()  {paste0("WGCNA_Expression_Visualization",".pdf")},
  content = function(file) {
    pdf(file, width = input$wgcna_exp_width, height = input$wgcna_exp_height)
    wgcna_expression()
    dev.off()
  }
)
