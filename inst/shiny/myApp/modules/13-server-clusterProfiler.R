observe({
  if (input$nWGCNA_3 | input$pGprofiler) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "clusterProfiler")
  }
})


observeEvent(input$start_clpr,{
  if (is.null(OrgDb())) {
    shinyalert(title = "warning", text = "The organism you selected was not surpported now, please use gProfiler!", type = "warning")
  }else {
    if (!requireNamespace(OrgDb(), quietly=TRUE)) {
      shinyalert(title = "warning", text = paste0("Can not find package ", OrgDb(), ", please install first!"), type = "warning")
    }
  }
})


output$clpr_gsets <- renderUI({
  if (input$clpr_types == 'ORA') {
    if (input$clpr_genes=="DEGs") {
      shinyjs::enable("start_clpr")
      selectInput(
        inputId = "clpr_degs", label = "DEGs:",
        choices = dir("DEGs") %>% stringr::str_remove_all(".csv"),
        selected = stringr::str_remove_all(dir("DEGs"), ".csv")[1],
        width = "100%", multiple = T
      )
    }else if (input$clpr_genes=="DEG Patterns") {
      if (input$run_degsp == 0) {
        shinyjs::disable("start_clpr")
        selectInput(inputId = "clpr_patterns", label = "Select Patterns ID:", width = "100%", multiple = T,
                    choices = "*Please Run DEGs Patterns First !!!", selected = "*Please Run DEGs Patterns First !!!")
        # p("*Please Run DEGs Patterns First!", style = "color: red; padding-top: 30px; padding-bttom: 30px; font-weight: 700px; width: 100%")
      }else {
        shinyjs::enable("start_clpr")
        selectInput(inputId = "clpr_patterns", label = "Select Patterns ID:", width = "100%", multiple = T,
                    choices = degsp_object()$normalized$cluster %>% unique %>% as.character)
      }
    }else if (input$clpr_genes=="WGCNA Modules") {
      if (input$moldue_detect == 0) {
        shinyjs::disable("start_clpr")
        selectInput(inputId = "clpr_modules", label = "Select WGCNA Modules ID:", width = "100%", multiple = T,
                    choices = "*Please Run WGCNA First !!!", selected = "*Please Run WGCNA First !!!")
        # p("*Please Run WGCNA First!", style = "color: red; padding-top: 30px; padding-bttom: 30px; font-weight: 700px; width: 100%")
      }else {
        shinyjs::enable("start_clpr")
        MEs0 = moduleEigengenes(datExpr(), moduleColors())$eigengenes
        MEs = orderMEs(MEs0)
        selectInput(inputId = "clpr_modules", label = "Select WGCNA Modules ID:",
                    choices = substring(names(MEs), first = 3), width = "100%", multiple = T)
      }
    }
  }else if (input$clpr_types == 'GSEA') {
    shinyjs::enable("start_clpr")
    selectInput(
      inputId = "clpr_regs", label = "REGs:",
      choices = dir("REGs") %>% stringr::str_remove_all(".csv"),
      selected = stringr::str_remove_all(dir("REGs"), ".csv")[1],
      width = "100%", multiple = F
    )
  }
})

observeEvent(input$get_DEGs,{
  if (input$clpr_genes=="DEGs") {
    updateSelectInput(
      session = session, inputId = "clpr_degs",
      choices = stringr::str_remove_all(dir("DEGs"), ".csv")[1]
    )
  }else if (input$clpr_types == 'GSEA') {
    updateSelectInput(
      session = session, inputId = "clpr_regs",
      choices = stringr::str_remove_all(dir("REGs"), ".csv")[1]
    )
  }
})

output$kegg_organism <- renderUI({
  kegg_species <- readRDS(system.file("shiny", "myApp/www/Species/kegg_species.rds", package = "QRseq"))
  choices <- kegg_species$kegg_code
  names(choices) <- kegg_species$scientific_name
  selectInput(inputId = "kegg_organism", label = "KEGG organism:", choices = choices, width = "100%", multiple = F)
})

clpr_geneList <- eventReactive(input$start_clpr, {
  withProgress(message = "", value = 0,{
    if (input$clpr_types == 'ORA') {
      if (input$clpr_genes=="DEGs") {
        incProgress(0.5, detail = "Loading differential genes ...")
        DeGenes <- load.DEGs(input$clpr_degs)
        GeneList <- lapply(DeGenes, function(x){ genes <- x$log2FoldChange; names(genes) <- rownames(x);return(genes) })
      }else if (input$clpr_genes=="WGCNA Modules") {
        incProgress(0.5, detail = "Loading WGCNA module genes ...")
        GeneList <- lapply(input$clpr_modules, function(x){ names(moduleColors())[moduleColors() == x] })
        names(GeneList) <- input$clpr_modules
      }else if (input$clpr_genes=="DEG Patterns") {
        incProgress(0.5, detail = "Loading expression pattern genes ...")
        GeneList <- lapply(input$clpr_patterns, function(x){ degsp_object()$df[degsp_object()$df$cluster == x, "genes"] })
        names(GeneList) <- input$clpr_patterns
      }
    }else if (input$clpr_types == 'GSEA') {
      incProgress(0.5, detail = paste("Extract DE Genes..."))
      ResList <- load.REGs(input$clpr_regs)[1]

      GeneList <- lapply(ResList, function(x){
        lfc_geneList <- x$log2FoldChange
        names(lfc_geneList) <- rownames(x)
        lfc_geneList
      })
    }
  })
  return(GeneList)
})

clpr_object <- eventReactive(input$start_clpr, {
  withProgress(message = "", value = 0,{
    if (keyType() != "SYMBOL") { readable = T }else { readable = F }

    if (input$clpr_types == 'GSEA') {
      if (input$clpr_source=='GO') {
        GeneList <- sort(clpr_geneList()[[1]], decreasing = T)
        incProgress(0.4, detail = paste("Runing gseGO..."))
        objects <- gseGO(geneList = GeneList, OrgDb = OrgDb(), ont = input$GO_ont, keyType = keyType(), pAdjustMethod = input$clpr_pAdjustMethod,
                       by = input$gsea_method, minGSSize = input$clpr_minGSSize, maxGSSize = 1000, pvalueCutoff = input$clpr_pval)
      }else if (input$clpr_source=='KEGG' | input$clpr_source=='Reactome') {
        if (keyType() != "ENTREZID") {
          genes_name <- names(clpr_geneList()[[1]])
          bitr_df <- bitr(genes_name, fromType = keyType(), toType = "ENTREZID", OrgDb = OrgDb())
          bitr_df <- bitr_df[!duplicated(bitr_df[, keyType()]), ]
          rownames(bitr_df) <- bitr_df[, keyType()]
          GeneList <- clpr_geneList()[[1]][genes_name %in% bitr_df[, keyType()]]
          names(GeneList) <- bitr_df[genes_name[genes_name %in% rownames(bitr_df)], "ENTREZID"]
          GeneList <- GeneList %>% sort(decreasing = T)
        }else {
          GeneList <- clpr_geneList()[[1]] %>% sort(decreasing = T)
        }

        if (input$clpr_source=='KEGG') {
          incProgress(0.4, detail = paste("Runing gseKEGG..."))
          objects <- gseKEGG(geneList = GeneList, organism = input$kegg_organism, by = input$gsea_method, pAdjustMethod = input$clpr_pAdjustMethod,
                          minGSSize = input$clpr_minGSSize, maxGSSize = 1000, pvalueCutoff = input$clpr_pval)
        }else if (input$clpr_source=='Reactome') {
          incProgress(0.4, detail = paste("Runing gsePathway..."))
          objects <- gsePathway(geneList = GeneList, organism = input$Reactome_organism, by = input$gsea_method, pAdjustMethod = input$clpr_pAdjustMethod,
                             minGSSize = input$clpr_minGSSize, maxGSSize = 1000, pvalueCutoff = input$clpr_pval)
        }
      }
      # print(head(as.data.frame(objects)))
    }else if (input$clpr_types == 'ORA') {
      if (input$clpr_genes=="DEGs") {
        GeneList <- lapply(clpr_geneList(), function(x){ names(x) })
      }else {
        GeneList <- clpr_geneList()
      }

      if (input$clpr_source=='GO') {
        incProgress(0.4, detail = paste("Runing enrichGO ...."))
        if (length(GeneList) > 1) {
          objects <- compareCluster(geneClusters = GeneList, fun = "enrichGO", OrgDb = OrgDb(), ont = input$GO_ont, keyType = keyType(),
                                    pAdjustMethod = input$clpr_pAdjustMethod, minGSSize = input$clpr_minGSSize, maxGSSize = 1000, pvalueCutoff = input$clpr_pval, readable = readable)
        }else {
          objects <- enrichGO(gene = GeneList[[1]], OrgDb = OrgDb(), ont = input$GO_ont, keyType = keyType(),
                              pAdjustMethod = input$clpr_pAdjustMethod, minGSSize = input$clpr_minGSSize, maxGSSize = 1000, pvalueCutoff = input$clpr_pval, readable = readable)
        }
      }else if (input$clpr_source=='KEGG' | input$clpr_source=='Reactome') {
        if (keyType() != "ENTREZID") {
          GeneList <- lapply(GeneList, function(x){
            bitr(x, fromType = keyType(), toType = "ENTREZID", OrgDb = OrgDb())$ENTREZID
          })
        }

        if (input$clpr_source=='KEGG'){
          incProgress(0.4, detail = paste("Runing enrichGO ...."))
          if (length(GeneList) > 1) {
            objects <- compareCluster(geneClusters = GeneList, fun = "enrichKEGG", organism = input$kegg_organism, pAdjustMethod = input$clpr_pAdjustMethod,
                                      minGSSize = input$clpr_minGSSize, maxGSSize = 1000, pvalueCutoff = input$clpr_pval)
          }else {
            objects <- enrichKEGG(gene = GeneList[[1]], organism = input$kegg_organism, pAdjustMethod = input$clpr_pAdjustMethod,
                            minGSSize = input$clpr_minGSSize, maxGSSize = 1000, pvalueCutoff = input$clpr_pval)
          }
        }else {
          incProgress(0.4, detail = paste("Runing enrichGO ...."))
          if (length(GeneList) > 1) {
            objects <- compareCluster(geneClusters = GeneList, fun = "enrichPathway", organism = input$Reactome_organism, pAdjustMethod = input$clpr_pAdjustMethod,
                                      minGSSize = input$clpr_minGSSize, maxGSSize = 1000, pvalueCutoff = input$clpr_pval)
          }else {
            objects <- enrichPathway(gene = GeneList[[1]], organism = input$Reactome_organism, pAdjustMethod = input$clpr_pAdjustMethod,
                                      minGSSize = input$clpr_minGSSize, maxGSSize = 1000, pvalueCutoff = input$clpr_pval)
          }
        }
      }
    }
  })
  return(objects)
})

##-----------------------------------------------------------------
observeEvent(input$start_clpr,{
  js$collapse("clpr_tab")
  clpr_object()
  if (dim(as.data.frame(clpr_object()))[1] != 0) {
    if ("NES" %in% colnames(as.data.frame(clpr_object()))) {
      shinyjs::enable("PlotGSEA")
      shinyalert(title = "Run of GSEA finished!", type = "success")
    }else {
      shinyjs::enable("plotORA")
      shinyalert(title = "Run of ORA finished!", type = "success")
    }
  }else {
    shinyjs::disable("PlotGSEA")
    shinyjs::disable("plotORA")
    shinyalert(title = "warning", text = "No Tems Was Enriched !!!", type = "warning")
  }
})

##------------------------------------------------------------------
## GSEA Plot

output$gseaID <- renderUI({
  if (input$clpr_types == 'GSEA') {
    if ("NES" %in% colnames(as.data.frame(clpr_object()))) {
      id <- as.data.frame(clpr_object())$ID
      names(id) <- as.data.frame(clpr_object())$Description
      if (length(id) != 0) {
        selectInput( "gseaID", "Select GESA Terms:", choices = id, multiple = T, width = "100%" )
      }
    }
  }
})

output$expr_gseaID <- renderUI({
  if (input$clpr_types == 'GSEA' & input$gseaPlot_type == 'exprs_heatmap') {
    id <- as.data.frame(clpr_object())$Description
    if (length(id) != 0) {
      selectInput( "expr_gseaID", "Select GSEA ID", choices = id, width = "100%" )
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
    gseaplot2(x = clpr_object(), geneSetID = input$gseaID, pvalue_table = input$gsea_pTable, ES_geom = input$gsea_ES, base_size = input$gsea_fontsize)
  }else if (input$gseaPlot_type == 'ridgeplot') {
    ridgeplot(clpr_object(), showCategory = input$gsea_nterms) +
      theme(axis.text.x = element_text(size = input$gsea_fontsize),
            axis.text.y = element_text(size = input$gsea_fontsize), text = element_text(size = input$gsea_fontsize))
  }else {
    geneID <- as.data.frame(clpr_object())[as.data.frame(clpr_object())$Description %in% input$expr_gseaID, "core_enrichment"]
    genes <- str_split(geneID, pattern = "/")[[1]]

    if (input$clpr_source != 'GO' & keyType() != "ENTREZID") {
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

    if (input$clpr_source == "GO") {
      main1  <- "GSEA (Gene Ontology): "
    }else if (input$clpr_source == "Reactome") {
      main1  <- "GSEA (Reactome Pathway): "
    }else if (input$clpr_source == "KEGG") {
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
  withSpinner(plotOutput("gseaPlot", width = paste0(input$clpr_plot_width, "%"), height = paste0(input$clpr_plot_height, "px")))
})

output$gsea_Pdf <- downloadHandler(
  filename = function()  {paste0("GSEA_Plot",".pdf")},
  content = function(file) {
    p <- gseaPlot()
    ggsave(file, p, width = input$gsea_width, height = input$gsea_height)
  }
)

##------------------------------------------------------------------
## ORA Plot
output$oraPlot_type <- renderUI({
  if (length(clpr_geneList()) > 1) {
    fluidRow(
      column(
        12,
        radioButtons(
          "oraPlot_type", "Methods to visualize:", c("dotplot", "barplot", "ggtable", "exprs_heatmap"), inline = T, width = "100%"
        ),
        tags$em("cnetplot and emapplot currently do not support multi-group comparison enrich analysis !", style = "color:brown")
      )
    )
  }else {
    radioButtons(
      "oraPlot_type", "Methods to visualize:", c("dotplot", "barplot", "ggtable", "cnetplot", "emapplot", "exprs_heatmap"),
      inline = T, width = "100%"
    )
  }
})

output$ora_termID <- renderUI({
  if (dim(as.data.frame(clpr_object()))[1] != 0) {
    selectInput("ora_termID", "Select terms from results:", choices = as.data.frame(clpr_object())$Description, width = "100%", multiple = T)
  }else {
    p("no terms enriched ...", style = "color:red")
  }
})

# # plot heatmap for spicific terms
output$ora_termID2 <- renderUI({
  if (dim(as.data.frame(clpr_object()))[1] != 0) {
    selectInput("ora_termID2", "Select terms from results:",
                choices = as.data.frame(clpr_object())$Description,
                selected = as.data.frame(clpr_object())$Description[1],
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
  if (input$oraPlot_type == 'dotplot') {
    dotplotResults(object = clpr_object(), showCategory = input$n_terms, color = input$ora_colorBy,
                    terms = terms, font.size = input$ora_fontsize, size = input$ora_orderBy)
  }else if (input$oraPlot_type == 'barplot') {
    barplotResults(object = clpr_object(), x = input$ora_orderBy, showCategory = input$n_terms, color = input$ora_colorBy,
                    terms = terms, font.size = input$ora_fontsize)
  }else if (input$oraPlot_type == 'ggtable') {
    ggtableResults(object = clpr_object(), by = input$ora_orderBy, showCategory = input$n_terms, color = input$ora_colorBy,
                    terms = terms, font.size = input$ora_fontsize)
  }else if (input$oraPlot_type == 'cnetplot') {
    if (inherits(clpr_object(), "enrichResult")) {
      if (input$clpr_genes=="DEGs") {
        if (keyType() != "ENTREZID" & input$clpr_source != "GO") {
          genes_name <- names(clpr_geneList()[[1]])
          bitr_df <- bitr(genes_name, fromType = keyType(), toType = "ENTREZID", OrgDb = OrgDb())
          bitr_df <- bitr_df[!duplicated(bitr_df[, keyType()]), ]
          rownames(bitr_df) <- bitr_df[, keyType()]
          GeneList <- clpr_geneList()[[1]][genes_name %in% bitr_df[, keyType()]]
          names(GeneList) <- bitr_df[genes_name[genes_name %in% rownames(bitr_df)], "ENTREZID"]
          log2FoldChange <- GeneList %>% sort(decreasing = T)
        }else {
          log2FoldChange <- clpr_geneList()[[1]] %>% sort(decreasing = T)
        }
      }else {
        log2FoldChange = NULL
      }
      enrichplot::cnetplot(clpr_object(), showCategory = input$n_terms, circular = as.logical(input$ora_circular), foldChange = log2FoldChange)
    }
  }else if (input$oraPlot_type == 'emapplot') {
    if (inherits(clpr_object(), "enrichResult")) {
      if (input$clpr_source == "GO") {
        gd <- GOSemSim::godata(OrgDb(), ont = input$GO_ont)
        compare_emap <- enrichplot::pairwise_termsim(clpr_object(), semData = gd,  method="Wang")
      }else {
        compare_emap <- enrichplot::pairwise_termsim(clpr_object())
      }
      enrichplot::emapplot(compare_emap, showCategory = input$n_terms, color = input$ora_colorBy)
    }
  }else if (input$oraPlot_type == 'exprs_heatmap') {
    geneID <- as.data.frame(clpr_object())[as.data.frame(clpr_object())$Description %in% input$ora_termID2, "geneID"]
    genes <- str_split(geneID, pattern = "/")[[1]]

    if (input$clpr_source != 'GO' & keyType() != "ENTREZID") {
      if (keyType() == "ENSEMBL") {
        genes.df <- bitr(genes, fromType = "ENTREZID", toType = "ENSEMBL", OrgDb = OrgDb())
      } else if (keyType() == "SYMBOL") {
        genes.df <- bitr(genes, fromType = "ENTREZID", toType = "SYMBOL", OrgDb = OrgDb())
      }
      genes <- genes.df[, keyType()]
    }

    sampleTable <- as.data.frame(colData(dds()))[dds()$condition %in% input$ora_exprs_group, ]
    rownames(sampleTable) <- sampleTable$samples

    # data <- assay(trans_value())
    if (input$ora_data_use == "rel_value") {
      data <- log2(norm_value() + 1) %>% as.data.frame()
    }else if(input$ora_data_use == "trans_value"){
      data <- assay(trans_value()) %>% as.data.frame()
    }else if(input$ora_data_use == "norm_value"){
      data <- norm_value() %>% as.data.frame()
    }

    if (length(genes)==1) {
      Sub_data <- data[rownames(data) %in% genes, sampleTable$samples] %>% t
      rownames(Sub_data) <-  genes
    }else {
      Sub_data <- data[rownames(data) %in% genes, sampleTable$samples]
    }

    if (input$clpr_source == "GO") {
      main1  <- "ORA (Gene Ontology): "
    }else if (input$clpr_source == "Reactome") {
      main1  <- "ORA (Reactome Pathway): "
    }else if (input$clpr_source == "KEGG") {
      main1  <- "ORA (KEGG Pathway): "
    }

    annotation_col = data.frame(condition = factor(sampleTable$condition))
    rownames(annotation_col) = sampleTable$samples
    color = colorRampPalette(c("navy", "white", "red"))(50)
    # Sub_data <- Sub_data - rowMeans(Sub_data)
    pheatmap(Sub_data, col=color,
             cluster_col=F, cluster_row=input$ora_cluster_row,
             scale = 'row', show_rownames = T,
             show_colnames = F, breaks=seq(input$ora_cluster_break[1], input$ora_cluster_break[2],
                                           (input$ora_cluster_break[2] - input$ora_cluster_break[1])/50),
             annotation_col = annotation_col, fontsize = input$ora_heatmap_fontsize,
             main = paste0(main1, input$ora_termID2))
  }
})

output$oraPlots <- renderPlot({
  oraPlots()
})

output$oraPlotsUI <-  renderUI({
  withSpinner(plotOutput("oraPlots", width = paste0(input$clpr_plot_width, "%"), height = paste0(input$clpr_plot_height, "px")))
})

output$ora_Pdf <- downloadHandler(
  filename = function()  {paste0("ORA_", input$oraPlot_type, ".pdf")},
  content = function(file) {
    p <- oraPlots()
    ggsave(file, p, width = input$ora_width, height = input$ora_height)
  }
)



# ora_heatmap <- eventReactive(input$plotORA_heatmap,{
#   if (dim(as.data.frame(clpr_object()))[1] != 0) {
#     if (keyType() == "ENTREZID") {
#       if (input$ora_types != "KEGG") {
#         geneID <- as.data.frame(clpr_object())[as.data.frame(clpr_object())$Description %in% input$ora_termID2, "geneID"]
#         genes <- str_split(geneID, pattern = "/")[[1]]
#         genes.df <- bitr(genes, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = OrgDb())
#         genes <- genes.df[, "ENTREZID"]
#       }else {
#         geneID <- as.data.frame(clpr_object())[as.data.frame(clpr_object())$Description %in% input$ora_termID2, "geneID"]
#         genes <- str_split(geneID, pattern = "/")[[1]]
#       }
#     }else if (keyType() == "ENSEMBL") {
#       if (input$ora_types != "KEGG") {
#         geneID <- as.data.frame(clpr_object())[as.data.frame(clpr_object())$Description %in% input$ora_termID2, "geneID"]
#         genes <- str_split(geneID, pattern = "/")[[1]]
#         genes.df <- bitr(genes, fromType = "SYMBOL", toType = "ENSEMBL", OrgDb = OrgDb())
#         genes <- genes.df[, "ENSEMBL"]
#       }else if (input$ora_types == "KEGG") {
#         geneID <- as.data.frame(clpr_object())[as.data.frame(clpr_object())$Description %in% input$ora_termID2, "geneID"]
#         genes <- str_split(geneID, pattern = "/")[[1]]
#         genes.df <- bitr(genes, fromType = "ENTREZID", toType = "ENSEMBL", OrgDb = OrgDb())
#         genes <- genes.df[, "ENSEMBL"]
#       }
#     }else if (keyType() == "SYMBOL") {
#       if (input$ora_types == "KEGG") {
#         geneID <- as.data.frame(clpr_object())[as.data.frame(clpr_object())$Description %in% input$ora_termID2, "geneID"]
#         genes <- str_split(geneID, pattern = "/")[[1]]
#         genes.df <- bitr(genes, fromType = "ENTREZID", toType = "SYMBOL", OrgDb = OrgDb())
#         genes <- genes.df[, "SYMBOL"]
#       }else {
#         geneID <- as.data.frame(clpr_object())[as.data.frame(clpr_object())$Description %in% input$ora_termID2, "geneID"]
#         genes <- str_split(geneID, pattern = "/")[[1]]
#       }
#     }
#
#     sampleTable <- as.data.frame(colData(dds()))[dds()$condition %in% input$ora_exprs_group, ]
#     rownames(sampleTable) <- sampleTable$samples
#
#     data <- assay(trans_value())
#
#     if (length(genes)==1) {
#       Sub_data <- data[rownames(data) %in% genes, sampleTable$samples] %>% t
#       rownames(Sub_data) <-  genes
#     }else {
#       Sub_data <- data[rownames(data) %in% genes, sampleTable$samples]
#     }
#
#     if (input$ora_types == "GO") {
#       main1  <- "Gene Ontology: "
#     }else if (input$ora_types == "DO") {
#       main1  <- "Disease Ontology: "
#     }else if (input$ora_types == "KEGG") {
#       main1  <- "KEGG Pathway: "
#     }else {
#       main1  <- "Reactome Pathway: "
#     }
#
#     annotation_col = data.frame(condition = factor(sampleTable$condition))
#     rownames(annotation_col) = sampleTable$samples
#     color = colorRampPalette(c("navy", "white", "red"))(50)
#     # Sub_data <- Sub_data - rowMeans(Sub_data)
#     pheatmap(Sub_data, col=color,
#              cluster_col=F, cluster_row=input$ora_cluster_row,
#              scale = 'row', show_rownames = T,
#              show_colnames = F, breaks=seq(input$ora_cluster_break[1], input$ora_cluster_break[2],
#                                            (input$ora_cluster_break[2] - input$ora_cluster_break[1])/50),
#              annotation_col = annotation_col,
#              main = paste0(main1, input$ora_termID2))
#
#   }else {
#     return(NULL)
#   }
# })

# output$ora_heatmap <- renderPlot({
#   ora_heatmap()
# })

# output$ora_heatmap_Pdf <- downloadHandler(
#   filename = function()  {paste0("Genes_expression_heatmap_of_specific_terms",".pdf")},
#   content = function(file) {
#     p <- ora_heatmap()
#     ggsave(file, p, width = input$ora_heatmap_width, height = input$ora_heatmap_height)
#   }
# )

# # enrichment results tab
# ora_Tab <- eventReactive(input$start_clpr,{
#   as.data.frame(clpr_object())
# })

output$ora_Tab <- renderDataTable({
  as.data.frame(clpr_object())
},rownames = T, options = list(pageLength = 10, autoWidth = F, scrollX=TRUE))

output$ora_Tab_Csv <- downloadHandler(
  filename = function()  {paste0("ORA_enrichment_results_Table",".csv")},
  content = function(file) {
    write.csv(ora_Tab(), file, row.names = T)
  }
)



