observe({
  if (input$nORA | input$pPathview) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "GSEA_cluster")
  }
})

observeEvent(input$start_clp_gsea,{
  if (is.null(OrgDb())) {
    sendSweetAlert(title = "warning", text = "The organism you selected was not surpported now, please use gProfiler2!", type = "warning")
  }else {
    if (!requireNamespace(OrgDb(), quietly=TRUE)) {
      sendSweetAlert(title = "warning", text = paste0("Can not find package ", OrgDb(), ", please install first!"), type = "warning")
    }
  }
})

output$clp_gsea_gsets <- renderUI({
  shinyjs::enable("start_clp_gsea")
  selectInput(
    inputId = "clp_gsea_regs", label = "REGs:",
    choices = dir("REGs") %>% stringr::str_remove_all(".csv"),
    selected = stringr::str_remove_all(dir("REGs"), ".csv")[1],
    width = "100%", multiple = F
  )
})

observeEvent(input$get_DEGs,{
  updateSelectInput(
    session = session, inputId = "clp_gsea_regs",
    choices = stringr::str_remove_all(dir("REGs"), ".csv")[1]
  )
})

clp_gsea_geneList <- eventReactive(input$start_clp_gsea, {
  withProgress(message = "", value = 0,{
    incProgress(0.5, detail = paste("Extract DE Genes..."))
    ResList <- load.REGs(input$clp_gsea_regs)[1]

    GeneList <- lapply(ResList, function(x){
      lfc_geneList <- x$log2FoldChange
      names(lfc_geneList) <- rownames(x)
      lfc_geneList
    })
  })
  return(GeneList)
})

clp_gsea_object <- eventReactive(input$start_clp_gsea, {
  withProgress(message = "", value = 0,{
    if (keyType() != "SYMBOL") { readable = T }else { readable = F }

    require(clusterProfiler)
    if (input$clp_gsea_source=='GO') {
      GeneList <- sort(clp_gsea_geneList()[[1]], decreasing = T)
      incProgress(0.4, detail = paste("Runing gseGO..."))
      require(OrgDb(), character.only = T)
      # cmd <- paste0("gseGO(geneList = GeneList, OrgDb = ", OrgDb(), ", ont = '", input$gsea_GO_ont, "', keyType = '", keyType(),
      #               "', pAdjustMethod = '", input$clp_gsea_pAdjustMethod, "', by = '", input$gsea_method, "', minGSSize = ", input$clp_gsea_minGSSize,
      #               ", maxGSSize = 1000, pvalueCutoff = ", input$clp_gsea_pval, ")")
      # objects <- eval(parse(text = cmd))
      objects <- gseGO(geneList = GeneList, OrgDb = eval(parse(text = OrgDb())), ont = input$GO_ont, eps = 0,
                       keyType = keyType(), pAdjustMethod = input$clp_gsea_pAdjustMethod, seed = as.logical(input$clp_gsea_seed),
                       by = input$gsea_method, minGSSize = input$clp_gsea_minGSSize, maxGSSize = 1000, pvalueCutoff = input$clp_gsea_pval)
    }else if (input$clp_gsea_source=='KEGG' | input$clp_gsea_source=='Reactome') {
      if (keyType() != "ENTREZID") {
        genes_name <- names(clp_gsea_geneList()[[1]])
        bitr_df <- bitr(genes_name, fromType = keyType(), toType = "ENTREZID", OrgDb = OrgDb())
        bitr_df <- bitr_df[!duplicated(bitr_df[, keyType()]), ]
        rownames(bitr_df) <- bitr_df[, keyType()]
        GeneList <- clp_gsea_geneList()[[1]][genes_name %in% bitr_df[, keyType()]]
        names(GeneList) <- bitr_df[genes_name[genes_name %in% rownames(bitr_df)], "ENTREZID"]
        GeneList <- GeneList %>% sort(decreasing = T)
      }else {
        GeneList <- clp_gsea_geneList()[[1]] %>% sort(decreasing = T)
      }
      
      if (input$clp_gsea_source=='KEGG') {
        incProgress(0.4, detail = paste("Runing gseKEGG..."))
        objects <- gseKEGG(geneList = GeneList, organism = species()$kegg_code[species()$display_name == input$gprofiler_species], eps = 0,
                           by = input$gsea_method, pAdjustMethod = input$clp_gsea_pAdjustMethod, seed = as.logical(input$clp_gsea_seed),
                           minGSSize = input$clp_gsea_minGSSize, maxGSSize = input$clp_gsea_maxGSSize, pvalueCutoff = input$clp_gsea_pval)
      }else if (input$clp_gsea_source=='Reactome') {
        incProgress(0.4, detail = paste("Runing gsePathway..."))
        objects <- gsePathway(geneList = GeneList, organism = input$gsea_reactome_organism, by = input$gsea_method, eps = 0,
                              pAdjustMethod = input$clp_gsea_pAdjustMethod, seed = as.logical(input$clp_gsea_seed),
                              minGSSize = input$clp_gsea_minGSSize, maxGSSize = 1000, pvalueCutoff = input$clp_gsea_pval)
      }
    }
  })
  return(objects)
})

##-----------------------------------------------------------------
observeEvent(input$start_clp_gsea,{
  js$collapse("clp_gsea_tab")
  clp_gsea_object()
  if (dim(as.data.frame(clp_gsea_object()))[1] != 0) {
    shinyjs::enable("PlotGSEA")
    sendSweetAlert(title = "clusterProfiler (GSEA) complete!", type = "success")
  }else {
    shinyjs::disable("PlotGSEA")
    sendSweetAlert(title = "warning", text = "No terms were enriched!", type = "warning")
  }
})

##------------------------------------------------------------------
## GSEA Plot

output$gseaID <- renderUI({
  if (input$start_clp_gsea) {
    if (dim(as.data.frame(clp_gsea_object()))[1] != 0) {
      id <- as.data.frame(clp_gsea_object())$Description
      if (length(id) != 0) {
        pickerInput( "gseaID", "Select GESA Terms:", choices = id, selected = id[1:18],
                     options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 5), width = "100%", multiple = T)
      }
    }else{
      p("no terms enriched ...", style = "color:red")
    }
  }
})

output$expr_gseaID <- renderUI({
  if (input$start_clp_gsea) {
    # if (dim(as.data.frame(clp_gsea_object()))[1] != 0 & input$gseaPlot_type == 'exprs_heatmap') {
    if (dim(as.data.frame(clp_gsea_object()))[1] != 0) {
      id <- as.data.frame(clp_gsea_object())$Description
      if (length(id) != 0) {
        selectInput( "expr_gseaID", "Select GSEA ID", choices = id, width = "100%")
      }else {
        p("no terms enriched ...", style = "color:red")
      }
    }
  }
})

output$gsea_exprs_group <- renderUI({
  virtualSelectInput(
    inputId = "gsea_exprs_group",  label = "Select group to plot:", 
    choices = dds()$condition %>% unique %>% as.character,
    selected = dds()$condition %>% unique %>% as.character,  multiple = T, search = F, width = "100%"
  )
})

gseaPlot <- eventReactive(input$PlotGSEA,{
  require(ggplot2)
  if (input$gseaPlot_type == 'gseaplot2') {
    gsea_id <- as.data.frame(clp_gsea_object())[as.data.frame(clp_gsea_object())$Description == input$expr_gseaID, "ID"]
    p <- enrichplot::gseaplot2(x = clp_gsea_object(), geneSetID = gsea_id, title = gsea_id, color = "green",
              pvalue_table = as.logical(input$gsea_pTable), ES_geom = input$gsea_ES, base_size = input$gsea_fontsize)
    
    if (nchar(input$gsea_gseaplot2_ggText != 0)) {
      add_funcs <- strsplit(input$gsea_gseaplot2_ggText, "\\+")[[1]]
      p <- p + lapply(add_funcs, function(x){
        eval(parse(text = x))
      })
    }
    return(p)
  }else if (input$gseaPlot_type == 'ridgeplot') {
    p <- ridgeplot2(x = clp_gsea_object(), showCategory = input$gsea_nterms, terms = input$gseaID, fill = input$gsea_colorBy)
    
    if (nchar(input$gsea_ridgeplot_ggText != 0)) {
      add_funcs <- strsplit(input$gsea_ridgeplot_ggText, "\\+")[[1]]
      p <- p + lapply(add_funcs, function(x){
        eval(parse(text = x))
      })
    }
    return(p)
    # +
    #   theme(axis.text.x = element_text(size = input$gsea_fontsize),
    #         axis.text.y = element_text(size = input$gsea_fontsize), text = element_text(size = input$gsea_fontsize))
  }else if (input$gseaPlot_type == 'dotplot') {
    p <- dotplotResults(clp_gsea_object(), x = "NES", showCategory = input$gsea_nterms, terms = input$gseaID, color = input$gsea_colorBy)
    
    if (nchar(input$gsea_dotplot_ggText != 0)) {
      add_funcs <- strsplit(input$gsea_dotplot_ggText, "\\+")[[1]]
      p <- p + lapply(add_funcs, function(x){
        eval(parse(text = x))
      })
    }
    return(p)
    # +
    #   theme(axis.text.x = element_text(size = input$gsea_fontsize),
    #         axis.text.y = element_text(size = input$gsea_fontsize), text = element_text(size = input$gsea_fontsize))
  }else {
    geneID <- as.data.frame(clp_gsea_object())[as.data.frame(clp_gsea_object())$Description %in% input$expr_gseaID, "core_enrichment"]
    genes <- stringr::str_split(geneID, pattern = "/")[[1]]

    if (input$clp_gsea_source != 'GO' & keyType() != "ENTREZID") {
      if (keyType() == "ENSEMBL") {
        genes.df <- bitr(genes, fromType = "ENTREZID", toType = "ENSEMBL", OrgDb = OrgDb())
      } else if (keyType() == "SYMBOL") {
        genes.df <- bitr(genes, fromType = "ENTREZID", toType = "SYMBOL", OrgDb = OrgDb())
      }
      genes <- genes.df[, keyType()]
    }

    sampleTable <- as.data.frame(dds()@colData)[dds()$condition %in% input$gsea_exprs_group, ]
    rownames(sampleTable) <- sampleTable$samples

    # data <- assay(trans_value())
    if (input$gsea_data_use == "rel_value") {
      data <- log2(norm_value() + 1) %>% as.data.frame()
    }else if(input$gsea_data_use == "trans_value"){
      data <- SummarizedExperiment::assay(trans_value()) %>% as.data.frame()
    }else if(input$gsea_data_use == "norm_value"){
      data <- norm_value() %>% as.data.frame()
    }

    if (length(genes)==1) {
      Sub_data <- data[genes, sampleTable$samples] %>% t
      rownames(Sub_data) <-  genes
    }else {
      Sub_data <- data[genes, sampleTable$samples]
    }

    if (input$clp_gsea_source == "GO") {
      main1  <- "GSEA (Gene Ontology): "
    }else if (input$clp_gsea_source == "Reactome") {
      main1  <- "GSEA (Reactome Pathway): "
    }else if (input$clp_gsea_source == "KEGG") {
      main1  <- "GSEA (KEGG Pathway): "
    }

    annotation_col = data.frame(condition = factor(sampleTable$condition))
    rownames(annotation_col) = sampleTable$samples
    color = colorRampPalette(c("navy", "white", "red"))(50)
    pheatmap::pheatmap(Sub_data, col=color, cluster_col=F, cluster_row=T, scale = 'row', angle_col = input$gsea_heat_angle,
             show_rownames = input$gsea_heat_rowname, show_colnames = input$gsea_heat_colname, 
             breaks=seq(input$gsea_cluster_break[1], input$gsea_cluster_break[2],
                                           (input$gsea_cluster_break[2] - input$gsea_cluster_break[1])/50),
             annotation_col = annotation_col, fontsize = input$gsea_heatmap_fontsize, main = paste0(main1, input$expr_gseaID))
  }
})

output$gseaPlot <- renderPlot({
  gseaPlot()
})

output$gseaPlotUI <-  renderUI({
  withSpinner(plotOutput("gseaPlot", width = paste0(input$clp_gsea_plot_width, "%"), height = paste0(input$clp_gsea_plot_height, "px")))
})

output$gsea_Pdf <- downloadHandler(
  filename = function()  {paste0("GSEA_Plot",".pdf")},
  content = function(file) {
    p <- gseaPlot()
    ggsave(file, p, width = input$gsea_width, height = input$gsea_height)
  }
)

output$gsea_Tab <- renderDataTable({
  as.data.frame(clp_gsea_object())
},rownames = T, options = list(pageLength = 10, autoWidth = F, scrollX=TRUE))

output$gsea_Tab_Csv <- downloadHandler(
  filename = function()  {paste0("GSEA_enrichment_results_Table",".csv")},
  content = function(file) {
    write.csv(as.data.frame(clp_gsea_object()), file, row.names = T)
  }
)
