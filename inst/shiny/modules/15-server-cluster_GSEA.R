observe({
  if (input$nORA | input$pPathview) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "GSEA_cluster")
  }
})

observeEvent(input$start_clp_gsea,{
  if (is.null(OrgDb())) {
    shinyalert(title = "warning", text = "The organism you selected was not surpported now, please use gProfiler2!", type = "warning")
  }else {
    if (!requireNamespace(OrgDb(), quietly=TRUE)) {
      shinyalert(title = "warning", text = paste0("Can not find package ", OrgDb(), ", please install first!"), type = "warning")
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

    if (input$clp_gsea_source=='GO') {
      GeneList <- sort(clp_gsea_geneList()[[1]], decreasing = T)
      incProgress(0.4, detail = paste("Runing gseGO..."))
      require(OrgDb(), character.only = T)
      cmd <- paste0("gseGO(geneList = GeneList, OrgDb = ", OrgDb(), ", ont = '", input$gsea_GO_ont, "', keyType = '", keyType(),
                    "', pAdjustMethod = '", input$clp_gsea_pAdjustMethod, "', by = '", input$gsea_method, "', minGSSize = ", input$clp_gsea_minGSSize,
                    ", maxGSSize = 1000, pvalueCutoff = ", input$clp_gsea_pval, ")")
      objects <- eval(parse(text = cmd))
      # objects <- gseGO(geneList = GeneList, OrgDb = OrgDb(), ont = input$GO_ont, keyType = keyType(), pAdjustMethod = input$clp_gsea_pAdjustMethod,
      #                by = input$gsea_method, minGSSize = input$clp_gsea_minGSSize, maxGSSize = 1000, pvalueCutoff = input$clp_gsea_pval)
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
        objects <- gseKEGG(geneList = GeneList, organism = species()$kegg_code[species()$display_name == input$gprofiler_species],
                           by = input$gsea_method, pAdjustMethod = input$clp_gsea_pAdjustMethod, nPerm = 1000,
                           minGSSize = input$clp_gsea_minGSSize, maxGSSize = 1000, pvalueCutoff = input$clp_gsea_pval)
      }else if (input$clp_gsea_source=='Reactome') {
        incProgress(0.4, detail = paste("Runing gsePathway..."))
        objects <- gsePathway(geneList = GeneList, organism = input$gsea_reactome_organism, by = input$gsea_method, pAdjustMethod = input$clp_gsea_pAdjustMethod,
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
    shinyalert(title = "Run of GSEA finished!", type = "success")
  }else {
    shinyjs::disable("PlotGSEA")
    shinyalert(title = "warning", text = "No Tems Was Enriched !!!", type = "warning")
  }
})

##------------------------------------------------------------------
## GSEA Plot

output$gseaID <- renderUI({
  if (input$start_clp_gsea) {
    if (dim(as.data.frame(clp_gsea_object()))[1] != 0) {
      id <- as.data.frame(clp_gsea_object())$ID
      names(id) <- as.data.frame(clp_gsea_object())$Description
      if (length(id) != 0) {
        pickerInput( "gseaID", "Select GESA Terms:", choices = id, selected = id[1:3],
                     options = list(`actions-box` = TRUE), width = "100%", multiple = T)
      }
    }else{
      p("no terms enriched ...", style = "color:red")
    }
  }
})

output$expr_gseaID <- renderUI({
  if (input$start_clp_gsea) {
    if (dim(as.data.frame(clp_gsea_object()))[1] != 0 & input$gseaPlot_type == 'exprs_heatmap') {
      id <- as.data.frame(clp_gsea_object())$Description
      if (length(id) != 0) {
        selectInput( "expr_gseaID", "Select GSEA ID", choices = id, width = "100%" )
      }
    }
  }
})

output$gsea_exprs_group <- renderUI({
  selectInput( inputId = "gsea_exprs_group", label = "Select group to plot:", dds()$condition %>% unique %>% as.character,
               multiple = T, selected = dds()$condition %>% unique %>% as.character, width = "100%" )
})

# observe({
#   gsea_object()
# })

gseaPlot <- eventReactive(input$PlotGSEA,{
  if (input$gseaPlot_type == 'gseaplot2') {
    gseaplot2(x = clp_gsea_object(), geneSetID = input$gseaID, pvalue_table = input$gsea_pTable, ES_geom = input$gsea_ES, base_size = input$gsea_fontsize)
  }else if (input$gseaPlot_type == 'ridgeplot') {
    ridgeplot(clp_gsea_object(), showCategory = input$gsea_nterms) +
      theme(axis.text.x = element_text(size = input$gsea_fontsize),
            axis.text.y = element_text(size = input$gsea_fontsize), text = element_text(size = input$gsea_fontsize))
  }else {
    geneID <- as.data.frame(clp_gsea_object())[as.data.frame(clp_gsea_object())$Description %in% input$expr_gseaID, "core_enrichment"]
    genes <- str_split(geneID, pattern = "/")[[1]]

    if (input$clp_gsea_source != 'GO' & keyType() != "ENTREZID") {
      if (keyType() == "ENSEMBL") {
        genes.df <- bitr(genes, fromType = "ENTREZID", toType = "ENSEMBL", OrgDb = OrgDb())
      } else if (keyType() == "SYMBOL") {
        genes.df <- bitr(genes, fromType = "ENTREZID", toType = "SYMBOL", OrgDb = OrgDb())
      }
      genes <- genes.df[, keyType()]
    }

    sampleTable <- as.data.frame(colData(dds()))[dds()$condition %in% input$gsea_exprs_group, ]
    rownames(sampleTable) <- sampleTable$samples

    # data <- assay(trans_value())
    if (input$gsea_data_use == "rel_value") {
      data <- log2(norm_value() + 1) %>% as.data.frame()
    }else if(input$gsea_data_use == "trans_value"){
      data <- assay(trans_value()) %>% as.data.frame()
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
    color = colorRampPalette(strsplit(input$gsea_heatmap_color, ",")[[1]])(50)
    pheatmap(Sub_data, col=color, cluster_col=F, cluster_row=input$gsea_cluster_row, scale = 'row', show_rownames = T,
             show_colnames = F, breaks=seq(input$gsea_cluster_break[1], input$gsea_cluster_break[2],
                                           (input$gsea_cluster_break[2] - input$gsea_cluster_break[1])/50),
             annotation_col = annotation_col, fontsize = input$gsea_fontsize, main = paste0(main1, input$expr_gseaID))
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
