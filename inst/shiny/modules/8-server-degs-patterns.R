observe({
  if (input$nDEA | input$pEpv) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "degsp")
  }
})

## ----------------------------------------
## running DEGs Pattern

output$degsp_group <- renderUI({
  pickerInput("degsp_group", "Groups Of Differential Expressed Genes:",
              choices = dir("DEGs") %>% stringr::str_remove_all(".csv"),
              selected = dir("DEGs") %>% stringr::str_remove_all(".csv"),
              width = "100%", multiple = T, options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 5))
})

observeEvent(input$get_DEGs,{
  updatePickerInput(
    session = session, inputId = "degsp_group",
    choices = dir("DEGs") %>% stringr::str_remove_all(".csv"),
    selected = dir("DEGs") %>% stringr::str_remove_all(".csv")
  )
})

degsp_object <- eventReactive(input$run_degsp, {
  withProgress(message = "", min = 0, max = 1, value = 0, {
    if (!isTRUE(input$degsp_switch) | !file.exists("Cache/des_patterns.rds")) {
      conditions <- strsplit(input$degsp_group, "_vs_") %>% unlist %>% unique
      sampleTable <- subset_Tab(dds(), conditions)

      # sampleTable <- as.data.frame(colData(dds()))[dds()$condition %in% conditions, ]
      # rownames(sampleTable) <- sampleTable$samples
      incProgress(0.2, detail = "Extract differential genes ...")
      GeneList <- load.DEGs(input$degsp_group)
      DeGenes <- lapply(GeneList, function(x){
        rownames(x)
      }) %>% unlist %>% unique()

      incProgress(0.2, detail = "Subset transformed exprs table ...")
      DeAssay <- assay(trans_value())[DeGenes, sampleTable$samples %>% as.character]
      incProgress(0.4, detail = "Calculating co-expression genes, this will take a while ...")
      if (dim(DeAssay)[1] < input$degsp_minc) {
        des_patterns <- degPatterns(ma = DeAssay, metadata = sampleTable,reduce = input$degsp_reduce,
                                    scale = input$degsp_scale, minc = dim(DeAssay)[1] / 2, time = "condition", plot = F)
      }else {
        des_patterns <- degPatterns(ma = DeAssay, metadata = sampleTable,reduce = input$degsp_reduce,
                                    scale = input$degsp_scale, minc = input$degsp_minc, time = "condition", plot = F)
      }
      saveRDS(des_patterns, "Cache/des_patterns.rds")
    }else {
      des_patterns <- readRDS("Cache/des_patterns.rds")
    }
  })
  return(des_patterns)
})

observeEvent(input$run_degsp, {
  js$collapse("degsp_run")
  degsp_object()
  shinyalert(title = "Run of degPatterns finished!", type = "success")
})

## -----------------------------------------
## plotting DEGs Pattern

output$degsp_cluster <- renderUI({
  selectInput(
    inputId = "degsp_cluster",
    label = "Select cluster to plot:",
    choices = degsp_object()$normalized$cluster %>% unique %>% as.character,
    selected = degsp_object()$normalized$cluster %>% unique %>% as.character,
    width = "100%",
    multiple = T
  )
})

output$degsp_order <- renderUI({
  selectInput(
    inputId = "degsp_order", label = "Condition plotting order:",
    choices = degsp_object()$summarise$condition %>% unique %>% as.character,
    selected = degsp_object()$summarise$condition %>% unique %>% as.character,
    width = "100%", multiple = T
  )
})

degsp_plot <- eventReactive(input$plot_degsp, {
  if (input$degsp_type == "BoxPlot") {
    data <- degsp_object()$normalized
    data <- data[data$condition %in% input$degsp_order, ]
    data$condition <- factor(data$condition, levels = input$degsp_order)
    data <- data[data$cluster %in% as.numeric(input$degsp_cluster), ]
    p <- QRseq::degPlotCluster(table = data, time = "condition", color = "colored", angle = 45,
                         points = input$degsp_points, boxes = input$degsp_boxes, lines = input$degsp_lines,
                         facet_col = input$degsp_cols, facet_scales = input$degsp_scales, cluster_order = input$degsp_cluster)
    if (nchar(input$degsp_ggText != 0)) {
      add_funcs <- strsplit(input$degsp_ggText, "\\+")[[1]]
      p <- p + lapply(add_funcs, function(x){
        eval(parse(text = x))
      })
    }
    return(p)
  }else {
    sampleTable <- subset_Tab(dds(), input$degsp_order)
    df <- degsp_object()$df[degsp_object()$df$cluster %in% as.numeric(input$degsp_cluster), ]
    data <- as.data.frame(assay(trans_value()))[, rownames(sampleTable)]
    order_ids <- df$cluster %>% order
    annotable <- data.frame(row.names = df$genes[order_ids], deg_cluster = df$cluster[order_ids])
    annotable$deg_cluster <- factor(as.character(annotable$deg_cluster), levels = as.character(annotable$deg_cluster) %>% unique)
    data <- data[rownames(annotable), ] %>% as.data.frame()
    col_ids <- trans_value()$samples[trans_value()$condition %in% input$degsp_order] %>% as.character
    data <- data[, col_ids]
    color = colorRampPalette(strsplit(input$degsp_color, ",")[[1]])(100)
    if (isTRUE(input$degsp_annoRow)) {
      pheatmap(data, color = color, scale = "row",
               cluster_cols = F,show_rownames = F,
               annotation_row = annotable,
               cluster_rows = input$degsp_cluster_rows,
               show_colnames = input$degsp_colname,
               treeheight_row = input$degsp_treeheight_row,
               fontsize = input$degsp_fontsize,
               angle_col = input$degsp_angle)
    }else {
      pheatmap(data, color = color, scale = "row",
               cluster_cols = F,show_rownames = F,
               cluster_rows = input$degsp_cluster_rows,
               show_colnames = input$degsp_colname,
               treeheight_row = input$degsp_treeheight_row,
               fontsize = input$degsp_fontsize,
               angle_col = input$degsp_angle)
    }
  }
})

output$degsp_plot <- renderPlot({
  degsp_plot()
})

output$degsp_plotUI <- renderUI({
  withSpinner(plotOutput("degsp_plot", width = paste0(input$degsp_plot_width, "%"), height = paste0(input$degsp_plot_height, "px")))
})

output$degsp_Pdf <- downloadHandler(
  filename = function()  {paste0("DEGs-expression-pattern",".pdf")},
  content = function(file) {
    p <- degsp_plot()
    ggsave(file, p, width = input$degsp_width, height = input$degsp_height)
  }
)

## --------------------------------------------------
## DEGs Patterns

degsp_cluster_tab <- eventReactive(input$run_degsp, {
  degsp_object()$df[degsp_object()$df$cluster %>% order(), ]
})

output$degsp_cluster_tab <- renderDataTable({
  degsp_cluster_tab()
},rownames = T, editable = TRUE,
options = list(pageLength = 5, autoWidth = F, scrollX=TRUE, scrollY=TRUE)
)

output$degsp_cluster_csv <- downloadHandler(
  filename = function()  {paste0("DEGs-expression-pattern_table",".csv")},
  content = function(file) {
    write.csv(degsp_cluster_tab(), file, row.names = F)
  }
)
