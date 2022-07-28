observe({
  if (input$nsgene) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "sfunc")
  }
})

int_funcList <- eventReactive(input$get_int_function, {
  withProgress(message = "", value = 0,{
    incProgress(0.2, detail = "Loading ora enrich results ...")
    # if (!input$start_clp_ora | input$intfs_funcs != input$GO_ont) {
    #   ora_functions <- NULL
    # }else {
    #   ora_functions <- as.data.frame(clp_ora_object())$Description
    # }

    if (!input$start_clp_ora) {
      ora_functions <- NULL
    }else if (input$clp_ora_source == 'GO' & input$intfs_funcs == input$GO_ont) {
      ora_functions <- as.data.frame(clp_ora_object())$Description
    }else if (input$clp_ora_source == 'KEGG' & input$intfs_funcs == 'KEGG') {
      ora_functions <- as.data.frame(clp_ora_object())$Description
    }else if (input$clp_ora_source == 'Reactome' & input$intfs_funcs == "REAC"){
      ora_functions <- as.data.frame(clp_ora_object())$Description
    }else {
      ora_functions <- NULL
    }

    incProgress(0.3, detail = "Loading gsea enrich results ...")
    # if (!input$start_clp_gsea | input$intfs_funcs != input$gsea_GO_ont) {
    #   gsea_functions <- NULL
    # }else {
    #   gsea_functions <- as.data.frame(clp_gsea_object())$Description
    # }
    if (!input$start_clp_gsea) {
      gsea_functions <- NULL
    }else if (input$clp_gsea_source == 'GO' & input$intfs_funcs == input$gsea_GO_ont) {
      gsea_functions <- as.data.frame(clp_gsea_object())$Description
    }else if (input$clp_gsea_source == 'KEGG' & input$intfs_funcs == 'KEGG') {
      gsea_functions <- as.data.frame(clp_gsea_object())$Description
    }else if (input$clp_gsea_source == 'Reactome' & input$intfs_funcs == "REAC"){
      gsea_functions <- as.data.frame(clp_gsea_object())$Description
    }else {
      gsea_functions <- NULL
    }

    incProgress(0.3, detail = "Loading gprofiler enrich results ...")
    if (!input$runGprofiler) {
      gprofiler_functions <- NULL
    }else {
      ids <- gprofiler_object()$result$source %>% grep(pattern = input$intfs_funcs)
      gprofiler_functions <- gprofiler_object()$result$term_name[ids]
    }

    FunctionList <- list(ora_functions = ora_functions, gsea_functions = gsea_functions, gprofiler_functions = gprofiler_functions)
  })
  return(FunctionList)
})

intf_venn <- eventReactive(input$get_int_function,{
  venn(int_funcList(), zcolor = 'style', ilcs = input$intf_venn_lsize, sncs = input$intf_venn_nsize, box = F)
})

output$intf_venn <- renderPlot({
  intf_venn()
})

output$intf_plotUI <-  renderUI({
  withSpinner(plotOutput("intf_venn", width = paste0(input$intf_venn_plot_width, "%"), height = paste0(input$intf_venn_plot_height, "px")))
})

output$intf_venn_Pdf <- downloadHandler(
  filename = function()  {"Intersected_function_vennPlot.pdf"},
  content = function(file) {
    p <- intf_venn()
    ggsave(file, p, width = input$intf_venn_width, height = input$intf_venn_height)
  }
)

intf_tab <- eventReactive(input$get_int_function,{
  # ora_int_gsea <- intersect(int_funcList()$ora_functions, int_funcList()$gsea_functions)
  # if (length(ora_int_gsea) == 0) {
  #   int_1 <- NULL
  # }else {
  #   int_1 <- data.frame(Sets = rep("ORA_intersect_GSEA", length(ora_int_gsea)), Functions = ora_int_gsea)
  # }
  # ora_in_gprofiler <- intersect(int_funcList()$ora_functions, int_funcList()$gprofiler_functions)
  # if (length(ora_in_gprofiler) == 0) {
  #   int_2 <- NULL
  # }else {
  #   int_2 <- data.frame(Sets = rep("ORA_intersect_gProfiler", length(ora_in_gprofiler)), Functions = ora_in_gprofiler)
  #
  # }
  # gsea_int_gprofiler <- intersect(int_funcList()$gsea_functions, int_funcList()$gprofiler_functions)
  # if (length(gsea_int_gprofiler) == 0) {
  #   int_3 <- NULL
  # }else {
  #   int_3 <- data.frame(Sets = rep("GSEA_intersect_gProfiler", length(gsea_int_gprofiler)), Functions = gsea_int_gprofiler)
  # }
  int_of_three_groups <- intersect(int_funcList()$ora_functions, int_funcList()$gsea_functions) %>%
    intersect(int_funcList()$gprofiler_functions)
  if (length(int_of_three_groups) == 0) {
    int_4 <- NULL
  }else {
    ids <- gprofiler_object()$result$source %>% grep(pattern = input$intfs_funcs)
    gprofiler_df <- gprofiler_object()$result[ids, c("term_id", "term_name", "p_value", "intersection")]
    gprofiler_df <- gprofiler_df[gprofiler_df$term_name %in% int_of_three_groups, ]
    colnames(gprofiler_df) <- c("ID", "Description", "gprofiler_pvalue", "gprofiler_genes")

    ora_df <- as.data.frame(clp_ora_object())[as.data.frame(clp_ora_object())$Description %in% int_of_three_groups, c("Description", "p.adjust", "geneID")]
    colnames(ora_df) <- c("Description","ora_p.adjust", "ora_genes")

    gsea_df <- as.data.frame(clp_gsea_object())[as.data.frame(clp_gsea_object())$Description %in% int_of_three_groups, c("Description","p.adjust", "enrichmentScore", "core_enrichment")]
    colnames(gsea_df) <- c("Description","gsea_p.adjust", "gsea_enrichmentScore", "gsea_genes")

    int_4 <- join_all(list(gprofiler_df, ora_df, gsea_df), by = "Description", type = "inner")
    colnames(int_4) <- c("ID", "Description", "gprofiler_pvalue", "ora_p.adjust", "gsea_p.adjust",
                         "gsea_enrichmentScore", "gprofiler_genes", "ora_genes", "gsea_genes")
  }
  return(int_4)
})

output$intfs_dfs <- renderDataTable({
  intf_tab()
},rownames = T, options = list(pageLength = 10, autoWidth = F, scrollX=TRUE))

output$intfs_dfs_Csv <- downloadHandler(
  filename = function()  {"Intersected_functions_table.csv"},
  content = function(file) {
    write.csv(intf_tab(), file, row.names = T)
  }
)
