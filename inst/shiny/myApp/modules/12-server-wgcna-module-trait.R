observe({
  if (input$nWGCNA_3 | input$pGprofiler) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "wgcna-4")
  }
})

## -------------------------------- Module-Traits Relationship ---------------------------------------##
moduleColors <- reactive({
  moduleColors = labels2colors(net()$colors)
  names(moduleColors) <- names(net()$colors)
  return(moduleColors)
})


moduleTraitCor <- eventReactive(input$plot_mtrs, {
  traitDataTab <- traitDataTab()
  MEs0 = moduleEigengenes(datExpr(), moduleColors())$eigengenes
  MEs = orderMEs(MEs0)
  moduleTraitCor = cor(MEs, traitDataTab, use = "p");
  return(moduleTraitCor)
})

moduleTraitPvalue <- eventReactive(input$plot_mtrs, {
  nSamples = nrow(datExpr());
  moduleTraitPvalue = corPvalueStudent(moduleTraitCor(), nSamples);
  return(moduleTraitPvalue)
})

textMatrix <- eventReactive(input$plot_mtrs, {
  # Will display correlations and their p-values
  textMatrix = paste(signif(moduleTraitCor(), 2), "\n(",
                     signif(moduleTraitPvalue(), 1), ")", sep = "");
  dim(textMatrix) = dim(moduleTraitCor())
  return(textMatrix)
})

output$module_showRows <- renderUI({
  pickerInput(
    inputId = "module_showRows", label = "Select Modules to Show:",
    choices = paste0("ME", moduleColors() %>% unique),
    selected = paste0("ME", moduleColors() %>% unique),
    multiple = T, width = "100%", options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 5)
  )
})

output$module_showCols <- renderUI({
  pickerInput(
    inputId = "module_showCols", label = "Select Modules to Show:",
    choices = traitDataTab() %>% colnames, selected = traitDataTab() %>% colnames,
    multiple = T, width = "100%", options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 5)
  )
})

LabeledHeatmap <- eventReactive(input$plot_mtrs, {
  traitDataTab <- traitDataTab()
  MEs0 = moduleEigengenes(datExpr(), moduleColors())$eigengenes
  MEs = orderMEs(MEs0)

  color = colorRampPalette(strsplit(input$module_colors, ",")[[1]])(100)

  row_idx <- seq(1, length(names(MEs)))
  showRows <- row_idx[names(MEs) %in% input$module_showRows]

  col_idx <- seq(1, length(names(traitDataTab)))
  showCols <- col_idx[names(traitDataTab) %in% input$module_showCols]

  par(mar=c(10,10,1,1))

  # Display the correlation values within a heatmap plot
  labeledHeatmap(Matrix = moduleTraitCor(), xLabels = names(traitDataTab), yLabels = names(MEs),
                 ySymbols = names(MEs), textMatrix = textMatrix(), colorLabels = FALSE, colors = color,
                 setStdMargins = F, cex.text = input$cex_text, xLabelsAngle = 45, yColorWidth = input$yColorWidth,
                 yColorOffset = 0.005, font.lab.x = input$font_lab %>% as.integer, font.lab.y = input$font_lab  %>% as.integer,
                 showRows = showRows, showCols = showCols, main = paste("Module-trait relationships"))
})

output$mtrs_heatmap <- renderPlot({
  LabeledHeatmap()
})

output$mtrs_heatmapUI <- renderUI({
  withSpinner(plotOutput("mtrs_heatmap", width = paste0(input$wgcna_heatmap_width, "%"), height = paste0(input$wgcna_heatmap_height, "px")))
})

output$mtrs_heatmap_Pdf <- downloadHandler(
  filename = function()  {paste0("WGCNA_Module-Trait_Relationships_Heatmap",".pdf")},
  content = function(file) {
    pdf(file, width = input$mtrs_heatmap_width, height = input$mtrs_heatmap_height)
    traitDataTab <- traitDataTab()
    MEs0 = moduleEigengenes(datExpr(), moduleColors())$eigengenes
    MEs = orderMEs(MEs0)

    color = colorRampPalette(strsplit(input$module_colors, ",")[[1]])(100)

    row_idx <- seq(1, length(names(MEs)))
    showRows <- row_idx[names(MEs) %in% input$module_showRows]

    col_idx <- seq(1, length(names(traitDataTab)))
    showCols <- col_idx[names(traitDataTab) %in% input$module_showCols]

    par(mar=c(10,10,1,1))
    # Display the correlation values within a heatmap plot
    labeledHeatmap(Matrix = moduleTraitCor(), xLabels = names(traitDataTab), yLabels = names(MEs),
                   ySymbols = names(MEs), textMatrix = textMatrix(), colorLabels = FALSE, colors = color,
                   setStdMargins = F, cex.text = input$cex_text, xLabelsAngle = 45, yColorWidth = input$yColorWidth,
                   yColorOffset = 0.005, font.lab.x = input$font_lab %>% as.integer, font.lab.y = input$font_lab  %>% as.integer,
                   showRows = showRows, showCols = showCols, zlim = c(-1, 1), main = paste("Module-trait relationships"))
    dev.off()
  }
)

