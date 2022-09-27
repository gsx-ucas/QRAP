observe({
  if (input$nWGCNA_3 | input$pGprofiler) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "wgcna-4")
  }
})

## -------------------------------- Module-Traits Relationship ---------------------------------------##
# moduleColors <- reactive({
#   moduleColors = WGCNA::labels2colors(net()$colors)
#   names(moduleColors) <- names(net()$colors)
#   return(moduleColors)
# })


moduleTraitCor <- eventReactive(input$plot_mtrs, {
  traitDataTab <- traitDataTab()
  MEs0 = WGCNA::moduleEigengenes(datExpr(), moduleColors())$eigengenes
  MEs = WGCNA::orderMEs(MEs0)
  moduleTraitCor = WGCNA::cor(MEs, traitDataTab, use = "p");
  return(moduleTraitCor)
})

moduleTraitPvalue <- eventReactive(input$plot_mtrs, {
  nSamples = nrow(datExpr());
  moduleTraitPvalue = WGCNA::corPvalueStudent(moduleTraitCor(), nSamples);
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
    choices = paste0("ME", moduleColors() %>% unique), selected = paste0("ME", moduleColors() %>% unique),
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
  MEs0 = WGCNA::moduleEigengenes(datExpr(), moduleColors())$eigengenes
  MEs = WGCNA::orderMEs(MEs0)

  color = colorRampPalette(strsplit(input$module_colors, ",")[[1]])(100)

  if (input$WGCNA_Heatmap_method == 'labeledHeatmap (WGCNA function)') {
    row_idx <- seq(1, length(names(MEs)))
    showRows <- row_idx[names(MEs) %in% input$module_showRows]

    col_idx <- seq(1, length(names(traitDataTab)))
    showCols <- col_idx[names(traitDataTab) %in% input$module_showCols]

    par(mar=c(10,10,1,1))

    # Display the correlation values within a heatmap plot
    WGCNA::labeledHeatmap(Matrix = moduleTraitCor(), xLabels = names(traitDataTab), yLabels = names(MEs),
                   ySymbols = names(MEs), textMatrix = textMatrix(), colorLabels = FALSE, colors = color,
                   setStdMargins = F, cex.text = input$cex_text, xLabelsAngle = 45, yColorWidth = input$yColorWidth,
                   yColorOffset = 0.005, font.lab.x = input$font_lab %>% as.integer, font.lab.y = input$font_lab  %>% as.integer,
                   showRows = showRows, showCols = showCols, main = paste("Module-trait relationships"))
  }else {
    anno_row <- data.frame(row.names = rownames(moduleTraitCor()),
                           ` ` = rownames(moduleTraitCor()) %>% stringr::str_remove("ME"), check.names = F, fix.empty.names = F)
    re_color <- eval(parse(text = paste0("c(", paste(paste0(anno_row$` `, " = ", "'", anno_row$` `, "'"), collapse = ","), ")")))

    pheat_map_data <- moduleTraitCor()
    textMatrix <- textMatrix()
    colnames(textMatrix) <- colnames(pheat_map_data)
    rownames(textMatrix) <- rownames(pheat_map_data)

    if (!is.null(input$module_showRows)) {
      textMatrix <- textMatrix[rownames(pheat_map_data) %in% input$module_showRows, ]
      pheat_map_data <- pheat_map_data[rownames(pheat_map_data) %in% input$module_showRows, ]
    }
    if (!is.null(input$module_showCols)) {
      textMatrix <- textMatrix[ ,colnames(pheat_map_data) %in% input$module_showCols]
      pheat_map_data <- pheat_map_data[ ,colnames(pheat_map_data) %in% input$module_showCols]
    }

    max_col_value <- max(abs(pheat_map_data))
    pheatmap::pheatmap(pheat_map_data, breaks = seq(-max_col_value, max_col_value, 2 * max_col_value / 100),
             color = color, display_numbers = textMatrix, annotation_row = anno_row, annotation_legend = F,
             annotation_colors = list(` ` = re_color), cluster_rows = input$WGCNA_heatmap_cluster_rows,
             cluster_cols = input$WGCNA_heatmap_cluster_cols, fontsize = input$WGCNA_heatmap_fontsize,
             fontsize_col = input$WGCNA_heatmap_fontsize_col, fontsize_number = input$WGCNA_heatmap_fontsize_num,
             treeheight_row = 15, treeheight_col = 15, angle_col = "315")
  }
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
    MEs0 = WGCNA::moduleEigengenes(datExpr(), moduleColors())$eigengenes
    MEs = WGCNA::orderMEs(MEs0)

    color = colorRampPalette(strsplit(input$module_colors, ",")[[1]])(100)

    if (input$WGCNA_Heatmap_method == 'labeledHeatmap (WGCNA function)') {
      row_idx <- seq(1, length(names(MEs)))
      showRows <- row_idx[names(MEs) %in% input$module_showRows]

      col_idx <- seq(1, length(names(traitDataTab)))
      showCols <- col_idx[names(traitDataTab) %in% input$module_showCols]

      par(mar=c(10,10,1,1))

      # Display the correlation values within a heatmap plot
      WGCNA::labeledHeatmap(Matrix = moduleTraitCor(), xLabels = names(traitDataTab), yLabels = names(MEs),
                     ySymbols = names(MEs), textMatrix = textMatrix(), colorLabels = FALSE, colors = color,
                     setStdMargins = F, cex.text = input$cex_text, xLabelsAngle = 45, yColorWidth = input$yColorWidth,
                     yColorOffset = 0.005, font.lab.x = input$font_lab %>% as.integer, font.lab.y = input$font_lab  %>% as.integer,
                     showRows = showRows, showCols = showCols, main = paste("Module-trait relationships"))
    }else {
      anno_row <- data.frame(row.names = rownames(moduleTraitCor()),
                             ` ` = rownames(moduleTraitCor()) %>% stringr::str_remove("ME"), check.names = F, fix.empty.names = F)
      re_color <- eval(parse(text = paste0("c(", paste(paste0(anno_row$` `, " = ", "'", anno_row$` `, "'"), collapse = ","), ")")))

      pheat_map_data <- moduleTraitCor()
      textMatrix <- textMatrix()
      colnames(textMatrix) <- colnames(pheat_map_data)
      rownames(textMatrix) <- rownames(pheat_map_data)

      if (!is.null(input$module_showRows)) {
        textMatrix <- textMatrix[rownames(pheat_map_data) %in% input$module_showRows, ]
        pheat_map_data <- pheat_map_data[rownames(pheat_map_data) %in% input$module_showRows, ]
      }
      if (!is.null(input$module_showCols)) {
        textMatrix <- textMatrix[ ,colnames(pheat_map_data) %in% input$module_showCols]
        pheat_map_data <- pheat_map_data[ ,colnames(pheat_map_data) %in% input$module_showCols]
      }

      max_col_value <- max(abs(pheat_map_data))
      pheatmap::pheatmap(pheat_map_data, breaks = seq(-max_col_value, max_col_value, 2 * max_col_value / 100),
                         color = color, display_numbers = textMatrix, annotation_row = anno_row, annotation_legend = F,
                         annotation_colors = list(` ` = re_color), cluster_rows = input$WGCNA_heatmap_cluster_rows,
                         cluster_cols = input$WGCNA_heatmap_cluster_cols, fontsize = input$WGCNA_heatmap_fontsize,
                         fontsize_col = input$WGCNA_heatmap_fontsize_col, fontsize_number = input$WGCNA_heatmap_fontsize_num,
                         angle_col = "315")
    }
    dev.off()
  }
)