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

output$degp_time <- renderUI({
  pickerInput(
    inputId = "degp_time", label = "variable that changes:",
    choices = colnames(dds()@colData)[!colnames(dds()@colData) %in% c("sizeFactor", "replaceable", "samples")],
    selected = "condition", multiple = F, width = "100%"
  )
})

output$degp_col <- renderUI({
  choices_vector <- colnames(dds()@colData)[!colnames(dds()@colData) %in% c("sizeFactor", "replaceable", "samples", input$degp_time)]
  pickerInput(
    inputId = "degp_col", label = "variable to separate samples:",
    choices = c("NULL", choices_vector), selected = "NULL", multiple = F, width = "100%"
  )
})


degsp_object <- eventReactive(input$run_degsp, {
  withProgress(message = "", min = 0, max = 1, value = 0, {
    if (!isTRUE(input$degsp_switch) | !file.exists("Cache/des_patterns.rds")) {

      sampleTable <- subset_Tab(dds(), input$degp_time)

      print(sampleTable)

      incProgress(0.2, detail = "Extract differential genes ...")
      GeneList <- load.DEGs(input$degsp_group)
      DeGenes <- lapply(GeneList, function(x){
        rownames(x)
      }) %>% unlist %>% unique()
      
      if (input$degp_col == "NULL") {
        input_degp_col <- NULL
      }else {
        input_degp_col <- input$degp_col
      }

      incProgress(0.2, detail = "Subset transformed exprs table ...")
      DeAssay <- SummarizedExperiment::assay(trans_value())[DeGenes, sampleTable$samples %>% as.character]
      
      print(head(DeAssay))
      incProgress(0.4, detail = "Calculating co-expression genes, this will take a while ...")
      if (dim(DeAssay)[1] < input$degsp_minc) {
        des_patterns <- DEGreport::degPatterns(ma = DeAssay, metadata = sampleTable,reduce = input$degsp_reduce, col = input_degp_col,
                                    scale = input$degsp_scale, minc = dim(DeAssay)[1] / 2, time = input$degp_time, plot = F)
      }else {
        des_patterns <- DEGreport::degPatterns(ma = DeAssay, metadata = sampleTable,reduce = input$degsp_reduce,col = input_degp_col,
                                    scale = input$degsp_scale, minc = input$degsp_minc, time = input$degp_time, plot = F)
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
  sendSweetAlert(title = "success", text = "DEG Patterns completed!", type = "success")
})

## -----------------------------------------
## plotting DEGs Pattern

output$degsp_cluster <- renderUI({
  virtualSelectInput(
    inputId = "degsp_cluster",  label = "Select cluster to plot:",
    choices = degsp_object()$normalized$cluster %>% unique %>% as.character,
    selected = degsp_object()$normalized$cluster %>% unique %>% as.character,
    multiple = TRUE, search = TRUE, width = "100%"
  )
})

output$degsp_order <- renderUI({
  virtualSelectInput(
    inputId = "degsp_order",  label = "Plotting order:",
    choices = as.data.frame(degsp_object()$summarise)[, input$degp_time] %>% unique %>% as.character,
    selected = as.data.frame(degsp_object()$summarise)[, input$degp_time] %>% unique %>% as.character,
    multiple = TRUE, search = TRUE, width = "100%"
  )
})

degsp_plot <- eventReactive(input$plot_degsp, {
  if (input$degsp_type == "BoxPlot") {
    data <- degsp_object()$normalized
    data <- data[data[, input$degp_time] %in% input$degsp_order, ]
    data[, input$degp_time] <- factor(data[, input$degp_time], levels = input$degsp_order)
    data <- data[data$cluster %in% as.numeric(input$degsp_cluster), ]
    
    p <- QRAP::degPlotCluster(table = data, time = input$degp_time, color = "colored", angle = 45,
                         points = input$degsp_points, boxes = input$degsp_boxes, lines = input$degsp_lines,
                         facet_col = input$degsp_cols, facet_scales = input$degsp_scales, cluster_order = input$degsp_cluster)
    if (nchar(input$degsp_ggText) != 0) {
      add_funcs <- strsplit(input$degsp_ggText, "\\+")[[1]]
      p <- p + lapply(add_funcs, function(x){
        eval(parse(text = x))
      })
    }
    return(p)
  }else {
    sampleTable <- subset_Tab(dds(), vars = input$degp_time, input$degsp_order)
    data <- as.data.frame(SummarizedExperiment::assay(trans_value()))[, rownames(sampleTable)]
    col_ids <- lapply(input$degsp_order, function(x){
      trans_value()$samples[trans_value()@colData[, input$degp_time] %in% x] %>% as.character
    }) %>% unlist()
    data <- data[, col_ids]
    
    # df <- degsp_object()$df[degsp_object()$df$cluster %in% as.numeric(input$degsp_cluster), ]
    df <- lapply(input$degsp_cluster, function(x){
      degsp_object()$df[degsp_object()$df$cluster == as.numeric(x), ]
    }) %>% dplyr::bind_rows()
    
    matched_genes <- intersect(df$genes, rownames(data))
    df <- df[df$genes %in% matched_genes, ]
    annotable <- data.frame(row.names = df$genes, deg_cluster = df$cluster)
    annotable$deg_cluster <- factor(as.character(annotable$deg_cluster), levels = annotable$deg_cluster %>% sort %>% as.character %>% unique)
    
    data <- data[matched_genes, ]
    
    color = colorRampPalette(strsplit(input$degsp_color, ",")[[1]])(100)
    
    if (isTRUE(input$degsp_annoRow)) {
      annotation_row <- annotable
    }else {
      annotation_row <- NA
    }

    pheatmap::pheatmap(data, color = color, scale = "row",
             cluster_cols = F,show_rownames = F,
             annotation_row = annotation_row,
             cluster_rows = input$degsp_cluster_rows,
             show_colnames = input$degsp_colname,
             fontsize_col = input$degsp_fontsize,
             treeheight_row = 20, angle_col = "315")
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
options = list(pageLength = 10, autoWidth = F, scrollX=TRUE, scrollY="360px")
)

output$degsp_cluster_csv <- downloadHandler(
  filename = function()  {paste0("DEGs-expression-pattern_table",".csv")},
  content = function(file) {
    write.csv(degsp_cluster_tab(), file, row.names = F)
  }
)
