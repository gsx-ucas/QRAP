observe({
  # if (input$nPattern | input$pModule | input$pORA) {
  if (input$nEpv | input$pWGCNA_2) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "wgcna-1")
  }
})

## ---------------------------------
## WGCNA expression data

output$wgcna_degs <- renderUI({
  pickerInput(
    inputId = "wgcna_degs", label = "Select DEGs:", 
    choices = dir("DEGs") %>% stringr::str_remove_all(".csv"),
    selected = dir("DEGs") %>% stringr::str_remove_all(".csv"),
    width = "100%", multiple = T, options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 5)
  )
})

observeEvent(input$get_DEGs,{
  updatePickerInput( session = session, inputId = "wgcna_degs", choices = dir("DEGs") %>% stringr::str_remove_all(".csv") )
})

output$wgcna_condition <- renderUI({
  pickerInput("wgcna_condition", "Select Conditions:", 
              choices = dds()$condition %>% unique %>% as.character,
              selected = dds()$condition %>% unique %>% as.character,
              width = "100%", multiple = T, options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 5))
})

datExpr <- eventReactive(input$get_wgcna_exprs,{
  withProgress(message = "", value = 0, min = 0, max = 1, {
    sampleTable <- as.data.frame(colData(dds()))[dds()$condition %in% input$wgcna_condition, ]
    
    if (input$filter_wgcna_genes == "differential genes") {
      incProgress(0.2, detail = "Getting DEGs ...")
      Des_list <- load.DEGs(input$wgcna_degs)
      DeGenes <- lapply(Des_list, function(x){
        rownames(x)
      }) %>% unlist %>% unique
      incProgress(0.2, detail = "getting expression data ...")
      exprs <- log2(norm_value() + 1)[DeGenes , sampleTable$samples] %>% as.data.frame()
    }else {
      incProgress(0.2, detail = "filtering low expression genes ...")
      exprs <- log2(norm_value() + 1)[ , dds()$samples] %>% as.data.frame()
      ffun=filterfun(pOverA(p = input$sample_prop, A = input$mini_reads))
      filt=genefilter(exprs,ffun)
      exprs = exprs[filt,]
      
      incProgress(0.2, detail = "testing good genes ...")
      gsg <- goodSamplesGenes(as.data.frame(t(exprs)), verbose = 3)
      if (!gsg$allOK) {
        # Optionally, print the gene and sample names that were removed:
        if (sum(!gsg$goodGenes)>0)
          printFlush(paste("Removing", length(rownames(exprs)[!gsg$goodGenes]), "genes", sep = " "));
        # Remove the offending genes and samples from the data:
        exprs = exprs[gsg$goodGenes, ]
      }
    }
    
    incProgress(0.2, detail = "estimateSizeFactors ...")
    # wgcna_dds <- DESeqDataSetFromMatrix(countData = exprs, colData = sampleTable, design = ~ condition)
    # wgcna_dds <- estimateSizeFactors(wgcna_dds)
    # incProgress(0.2, detail = "estimateDispersions ...")
    # wgcna_dds <- estimateDispersions(wgcna_dds)
    # incProgress(0.2, detail = "varianceStabilizingTransformation ...")
    # wgcna_vst <- varianceStabilizingTransformation(wgcna_dds, blind=FALSE)
    # wgcna_vst <- varianceStabilizingTransformation(exprs %>% as.matrix, blind=TRUE)
    # datExpr <- assay(wgcna_vst) %>% t %>% as.data.frame
    exprs <- exprs %>% t %>% as.data.frame
  })
  return(exprs)
})

output$wgcna_exprs <- renderDataTable({
  datExpr()[, 1:20]
},rownames = T, editable = TRUE,
options = list(pageLength = 5, autoWidth = F, scrollX=TRUE, scrollY=TRUE)
)

# observeEvent(input$start_wgcna_meta, {
#   js$collapse("wgcna_expr_card")
# })

## ---------------------------------
## WGCNA meta data

# # upload or generate a clinical trait data
traitDataTab <- eventReactive(input$get_wgcna_exprs,{
  if (input$wgcna_meta_source == 'upload from local') {
    sampleTable <- as.data.frame(colData(dds()))[dds()$condition %in% input$wgcna_condition, ]
    
    inFile <- input$traitfile
    traitData <- vroom::vroom(inFile$datapath, col_names = input$trait_header) %>% as.data.frame
    rownames(traitData) <- traitData[, 1]
    traitData <- traitData[sampleTable$samples, -1]
  }else {
    sampleTable <- as.data.frame(colData(dds()))[dds()$condition %in% input$wgcna_condition, ]
    rownames(sampleTable) <- sampleTable$samples
    sampleTable <- sampleTable[rownames(datExpr()), ]
    
    sampleTable$condition <- sampleTable$condition %>% as.character
    
    for (x in sampleTable$condition %>% unique %>% as.character) {
      sampleTable[sampleTable$condition == x, x] <- 1
      sampleTable[sampleTable$condition != x, x] <- 0
    }
    traitData <- sampleTable[, sampleTable$condition %>% unique%>% as.character]
  }
  return(traitData)
})

output$wgcna_meta <- renderDataTable({
  traitDataTab()
},rownames = T, editable = TRUE,
options = list(pageLength = 5, autoWidth = F, scrollX=TRUE, scrollY=TRUE)
)