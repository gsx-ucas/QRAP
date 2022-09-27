observe({
  if (input$nGprofiler | input$pGSEA) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "ORA_cluster")
  }
})

observeEvent(input$start_clp_ora,{
  if (is.null(OrgDb())) {
    sendSweetAlert(title = "warning", text = "The organism you selected was not surpported now, please use gProfiler!", type = "warning")
  }else {
    if (!requireNamespace(OrgDb(), quietly=TRUE)) {
      sendSweetAlert(title = "warning", text = paste0("Can not find package ", OrgDb(), ", please install first!"), type = "warning")
    }
  }
})

output$clp_ora_gsets <- renderUI({
  if (input$clp_ora_genes=="DEGs") {
    shinyjs::enable("start_clp_ora")
    virtualSelectInput(
      inputId = "clp_ora_degs",  label = "DEGs:",
      choices = dir("DEGs") %>% stringr::str_remove_all(".csv"),
      selected = stringr::str_remove_all(dir("DEGs"), ".csv")[1],
      multiple = T, search = F, width = "100%"
    )
  }else if (input$clp_ora_genes=="DEG Patterns") {
    if (input$run_degsp == 0) {
      shinyjs::disable("start_clp_ora")
      selectInput(inputId = "clp_ora_patterns", label = "Select Patterns ID:", width = "100%", 
                  choices = "*Please Run DEGs Patterns First !!!", selected = "*Please Run DEGs Patterns First !!!")
      # p("*Please Run DEGs Patterns First!", style = "color: red; padding-top: 30px; padding-bttom: 30px; font-weight: 700px; width: 100%")
    }else {
      shinyjs::enable("start_clp_ora")
      selectInput(inputId = "clp_ora_patterns", label = "Select Patterns ID:", width = "100%", 
                  choices = degsp_object()$normalized$cluster %>% unique %>% as.character)
      virtualSelectInput(
        inputId = "clp_ora_patterns",  label = "Select Patterns ID:",
        choices = degsp_object()$normalized$cluster %>% unique %>% as.character,
        selected = (degsp_object()$normalized$cluster %>% unique %>% as.character)[1],
        multiple = T, search = F, width = "100%"
      )
    }
  }else if (input$clp_ora_genes=="WGCNA Modules") {
    if (input$moldue_detect == 0) {
      shinyjs::disable("start_clp_ora")
      selectInput(inputId = "clp_ora_modules", label = "Select WGCNA Modules ID:", width = "100%", 
                  choices = "*Please Run WGCNA First !!!", selected = "*Please Run WGCNA First !!!")
      # p("*Please Run WGCNA First!", style = "color: red; padding-top: 30px; padding-bttom: 30px; font-weight: 700px; width: 100%")
    }else {
      shinyjs::enable("start_clp_ora")
      MEs0 = moduleEigengenes(datExpr(), moduleColors())$eigengenes
      MEs = orderMEs(MEs0)
      virtualSelectInput(
        inputId = "clp_ora_modules",  label = "Select WGCNA Modules ID:",
        choices = substring(names(MEs), first = 3),
        selected = substring(names(MEs), first = 3)[1],
        multiple = T, search = F, width = "100%"
      )
    }
  }
})

observeEvent(input$get_DEGs,{
  updateVirtualSelect(
    session = session, inputId = "clp_ora_degs",
    choices = stringr::str_remove_all(dir("DEGs"), ".csv")[1]
  )
})

clp_ora_geneList <- eventReactive(input$start_clp_ora, {
  withProgress(message = "", value = 0,{
    if (input$clp_ora_genes=="DEGs") {
      incProgress(0.5, detail = "Loading differential genes ...")
      DeGenes <- load.DEGs(input$clp_ora_degs)
      GeneList <- lapply(DeGenes, function(x){ genes <- x$log2FoldChange; names(genes) <- rownames(x);return(genes) })
    }else if (input$clp_ora_genes=="WGCNA Modules") {
      incProgress(0.5, detail = "Loading WGCNA module genes ...")
      GeneList <- lapply(input$clp_ora_modules, function(x){ names(moduleColors())[moduleColors() == x] })
      names(GeneList) <- input$clp_ora_modules
    }else if (input$clp_ora_genes=="DEG Patterns") {
      incProgress(0.5, detail = "Loading expression pattern genes ...")
      GeneList <- lapply(input$clp_ora_patterns, function(x){ degsp_object()$df[degsp_object()$df$cluster == x, "genes"] })
      names(GeneList) <- input$clp_ora_patterns
    }
  })
  return(GeneList)
})

clp_ora_object <- eventReactive(input$start_clp_ora, {
  withProgress(message = "", value = 0,{
    if (keyType() != "SYMBOL") { readable = T }else { readable = F }

    if (input$clp_ora_genes=="DEGs") {
      GeneList <- lapply(clp_ora_geneList(), function(x){ names(x) })
    }else {
      GeneList <- clp_ora_geneList()
    }

    require(clusterProfiler)
    if (input$clp_ora_source=='GO') {
      incProgress(0.4, detail = paste("Runing enrichGO ...."))
      if (length(GeneList) > 1) {
        objects <- compareCluster(geneClusters = GeneList, fun = "enrichGO", OrgDb = OrgDb(), ont = input$GO_ont, keyType = keyType(),
                                  pAdjustMethod = input$clp_ora_pAdjustMethod, qvalueCutoff = input$clp_ora_qval,
                                  minGSSize = input$clp_ora_minGSSize, maxGSSize = 1000, pvalueCutoff = input$clp_ora_pval, readable = readable)
      }else {
        objects <- enrichGO(gene = GeneList[[1]], OrgDb = OrgDb(), ont = input$GO_ont, keyType = keyType(),
                            pAdjustMethod = input$clp_ora_pAdjustMethod, qvalueCutoff = input$clp_ora_qval,
                            minGSSize = input$clp_ora_minGSSize, maxGSSize = 1000, pvalueCutoff = input$clp_ora_pval, readable = readable)
      }
    }else if (input$clp_ora_source=='KEGG' | input$clp_ora_source=='Reactome') {
      if (keyType() != "ENTREZID") {
        GeneList <- lapply(GeneList, function(x){
          bitr(x, fromType = keyType(), toType = "ENTREZID", OrgDb = OrgDb())$ENTREZID
        })
      }

      if (input$clp_ora_source=='KEGG'){
        incProgress(0.4, detail = paste("Runing enrichGO ...."))
        if (length(GeneList) > 1) {
          objects <- compareCluster(geneClusters = GeneList, fun = "enrichKEGG", organism = species()$kegg_code[species()$display_name == input$gprofiler_species],
                                    pAdjustMethod = input$clp_ora_pAdjustMethod, qvalueCutoff = input$clp_ora_qval, minGSSize = input$clp_ora_minGSSize, maxGSSize = 1000, pvalueCutoff = input$clp_ora_pval)
        }else {
          objects <- enrichKEGG(gene = GeneList[[1]], organism = species()$kegg_code[species()$display_name == input$gprofiler_species],
                                pAdjustMethod = input$clp_ora_pAdjustMethod, qvalueCutoff = input$clp_ora_qval, minGSSize = input$clp_ora_minGSSize, maxGSSize = 1000, pvalueCutoff = input$clp_ora_pval)
        }
      }else {
        incProgress(0.4, detail = paste("Runing enrichGO ...."))
        if (length(GeneList) > 1) {
          objects <- compareCluster(geneClusters = GeneList, fun = "enrichPathway", organism = input$ora_reactome_organism, pAdjustMethod = input$clp_ora_pAdjustMethod,
                                    qvalueCutoff = input$clp_ora_qval, minGSSize = input$clp_ora_minGSSize, maxGSSize = 1000, pvalueCutoff = input$clp_ora_pval)
        }else {
          objects <- enrichPathway(gene = GeneList[[1]], organism = input$ora_reactome_organism, pAdjustMethod = input$clp_ora_pAdjustMethod,
                                   qvalueCutoff = input$clp_ora_qval, minGSSize = input$clp_ora_minGSSize, maxGSSize = 1000, pvalueCutoff = input$clp_ora_pval)
        }
      }
    }
  })
  return(objects)
})

##-----------------------------------------------------------------
observeEvent(input$start_clp_ora,{
  js$collapse("clp_ora_tab")
  clp_ora_object()
  if (dim(as.data.frame(clp_ora_object()))[1] != 0) {
    shinyjs::enable("plotORA")
    sendSweetAlert(title = "clusterProfiler (ORA) completed!", type = "success")
  }else {
    shinyjs::disable("plotORA")
    sendSweetAlert(title = "warning", text = "No terms were enriched!", type = "warning")
  }
})

##------------------------------------------------------------------
## ORA Plot
output$oraPlot_type <- renderUI({
  if (length(clp_ora_geneList()) > 1) {
    fluidRow(
      column(
        12,
        prettyRadioButtons(inputId = "oraPlot_type", label = "Methods to visualize:", animation = "jelly", inline = TRUE,
                           choices = c("dotplot", "barplot", "ggtable", "exprs_heatmap"), icon = icon("check"), status = "info"),
        tags$em("cnetplot and emapplot currently do not support multi-group comparison enrich analysis !", style = "color:brown")
      )
    )
  }else {
    prettyRadioButtons(inputId = "oraPlot_type", label = "Methods to visualize:", animation = "jelly", inline = TRUE,
                       choices = c("dotplot", "barplot", "cnetplot", "emapplot", "exprs_heatmap"), icon = icon("check"), status = "info")
  }
})

output$ora_termID <- renderUI({
  if (dim(as.data.frame(clp_ora_object()))[1] != 0) {
    pickerInput("ora_termID", "Select terms from results:",
                choices = as.data.frame(clp_ora_object())$Description,
                selected = as.data.frame(clp_ora_object())$Description[1:10],
                options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 5), width = "100%", multiple = T)
  }else {
    p("no terms enriched ...", style = "color:red")
  }
})

# # plot heatmap for spicific terms
output$ora_termID2 <- renderUI({
  if (dim(as.data.frame(clp_ora_object()))[1] != 0) {
    selectInput("ora_termID2", "Select terms from results:",
                choices = as.data.frame(clp_ora_object())$Description,
                selected = as.data.frame(clp_ora_object())$Description[1],
                width = "100%", multiple = F)
  }
})

output$ora_exprs_group <- renderUI({
  pickerInput(
    inputId = "ora_exprs_group",
    label = "Select group to plot:",
    dds()$condition %>% unique %>% as.character,
    multiple = T,
    selected = dds()$condition %>% unique %>% as.character,
    options = list(
      `actions-box` = TRUE),
    width = "100%"
  )
})

oraPlots <- eventReactive(input$plotORA, {
  if (input$useTop == 'use topN terms') {
    terms = NULL
  } else {
    terms = input$ora_termID
  }
  require(ggplot2)
  if (input$oraPlot_type == 'dotplot') {
    p <- dotplotResults(object = clp_ora_object(), showCategory = input$n_terms, color = input$ora_colorBy,
                   terms = terms, size = input$ora_orderBy)
    if (nchar(input$cl_ora_ggText != 0)) {
      add_funcs <- strsplit(input$cl_ora_ggText, "\\+")[[1]]
      p <- p + lapply(add_funcs, function(x){
        eval(parse(text = x))
      })
    }
    return(p)
  }else if (input$oraPlot_type == 'barplot') {
    p <- barplotResults(object = clp_ora_object(), x = input$ora_orderBy, showCategory = input$n_terms, color = input$ora_colorBy,
                   terms = terms, font.size = input$ora_fontsize)
    if (nchar(input$cl_ora_ggText != 0)) {
      add_funcs <- strsplit(input$cl_ora_ggText, "\\+")[[1]]
      p <- p + lapply(add_funcs, function(x){
        eval(parse(text = x))
      })
    }
    return(p)
  }else if (input$oraPlot_type == 'cnetplot') {
    if (inherits(clp_ora_object(), "enrichResult")) {
      if (input$clp_ora_genes=="DEGs") {
        if (keyType() != "ENTREZID" & input$clp_ora_source != "GO") {
          genes_name <- names(clp_ora_geneList()[[1]])
          bitr_df <- bitr(genes_name, fromType = keyType(), toType = "ENTREZID", OrgDb = OrgDb())
          bitr_df <- bitr_df[!duplicated(bitr_df[, keyType()]), ]
          rownames(bitr_df) <- bitr_df[, keyType()]
          GeneList <- clp_ora_geneList()[[1]][genes_name %in% bitr_df[, keyType()]]
          names(GeneList) <- bitr_df[genes_name[genes_name %in% rownames(bitr_df)], "ENTREZID"]
          log2FoldChange <- GeneList %>% sort(decreasing = T)
        }else {
          log2FoldChange <- clp_ora_geneList()[[1]] %>% sort(decreasing = T)
        }
      }else {
        log2FoldChange = NULL
      }
      enrichplot::cnetplot(clp_ora_object(), showCategory = input$n_terms, node_label = input$ora_node_label, 
                           circular = as.logical(input$ora_circular), foldChange = log2FoldChange)
    }
  }else if (input$oraPlot_type == 'emapplot') {
    if (inherits(clp_ora_object(), "enrichResult")) {
      if (input$clp_ora_source == "GO") {
        gd <- GOSemSim::godata(OrgDb(), ont = input$GO_ont)
        compare_emap <- enrichplot::pairwise_termsim(clp_ora_object(), semData = gd,  method="Wang")
      }else {
        compare_emap <- enrichplot::pairwise_termsim(clp_ora_object())
      }
      enrichplot::emapplot(compare_emap, showCategory = input$n_terms, color = input$ora_colorBy, cex_label_category = input$cex_label_category / 10)
    }
  }else if (input$oraPlot_type == 'exprs_heatmap') {
    geneID <- as.data.frame(clp_ora_object())[as.data.frame(clp_ora_object())$Description %in% input$ora_termID2, "geneID"]
    genes <- stringr::str_split(geneID, pattern = "/")[[1]]

    if (input$clp_ora_source != 'GO' & keyType() != "ENTREZID") {
      if (keyType() == "ENSEMBL") {
        genes.df <- bitr(genes, fromType = "ENTREZID", toType = "ENSEMBL", OrgDb = OrgDb())
      } else if (keyType() == "SYMBOL") {
        genes.df <- bitr(genes, fromType = "ENTREZID", toType = "SYMBOL", OrgDb = OrgDb())
      }
      genes <- genes.df[, keyType()]
    }

    sampleTable <- as.data.frame(dds()@colData)[dds()$condition %in% input$ora_exprs_group, ]
    rownames(sampleTable) <- sampleTable$samples

    # data <- assay(trans_value())
    if (input$ora_data_use == "rel_value") {
      data <- log2(norm_value() + 1) %>% as.data.frame()
    }else if(input$ora_data_use == "trans_value"){
      data <- SummarizedExperiment::assay(trans_value()) %>% as.data.frame()
    }else if(input$ora_data_use == "norm_value"){
      data <- norm_value() %>% as.data.frame()
    }

    if (length(genes)==1) {
      Sub_data <- data[rownames(data) %in% genes, sampleTable$samples] %>% t
      rownames(Sub_data) <-  genes
    }else {
      Sub_data <- data[rownames(data) %in% genes, sampleTable$samples]
    }

    if (input$clp_ora_source == "GO") {
      main1  <- "ORA (Gene Ontology): "
    }else if (input$clp_ora_source == "Reactome") {
      main1  <- "ORA (Reactome Pathway): "
    }else if (input$clp_ora_source == "KEGG") {
      main1  <- "ORA (KEGG Pathway): "
    }

    annotation_col = data.frame(condition = factor(sampleTable$condition))
    rownames(annotation_col) = sampleTable$samples
    color = colorRampPalette(c("navy", "white", "red"))(50)
    pheatmap::pheatmap(Sub_data, col = color, cluster_col = F, cluster_row = T, scale = 'row', 
             show_rownames = input$ora_heat_rowname,  show_colnames = input$ora_heat_colname, 
             breaks=seq(input$ora_cluster_break[1], input$ora_cluster_break[2],
                                           (input$ora_cluster_break[2] - input$ora_cluster_break[1])/50),
             annotation_col = annotation_col, fontsize_row = input$ora_heatmap_fontsize,
             angle_col = input$ora_heat_angle, main = paste0(main1, input$ora_termID2))
  }
})

output$oraPlots <- renderPlot({
  oraPlots()
})

output$oraPlotsUI <-  renderUI({
  withSpinner(plotOutput("oraPlots", width = paste0(input$ora_plot_width, "%"), height = paste0(input$ora_plot_height, "px")))
})

output$ora_Pdf <- downloadHandler(
  filename = function()  {paste0("ORA_", input$oraPlot_type, ".pdf")},
  content = function(file) {
    p <- oraPlots()
    ggsave(file, p, width = input$ora_width, height = input$ora_height)
  }
)

output$ora_Tab <- renderDataTable({
  as.data.frame(clp_ora_object())
},rownames = T, options = list(pageLength = 10, autoWidth = F, scrollX=TRUE))

output$ora_Tab_Csv <- downloadHandler(
  filename = function()  {paste0("ORA_enrichment_results_Table",".csv")},
  content = function(file) {
    write.csv(as.data.frame(clp_ora_object()), file, row.names = T)
  }
)
