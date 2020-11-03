observe({
  # if (input$nPattern | input$pModule | input$pORA) {
  if (input$nWGCNA_1 | input$pWGCNA_3) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "wgcna-2")
  }
})

## --------------------------------------------------------------
## module detection 
net <- eventReactive(input$moldue_detect,{
  withProgress(message = "", value = 0, {
    incProgress(0.2, detail = "Calculating SoftThreshold ...")
    powers = c(c(1:10), seq(from = 12, to=20, by=2))
    sft = pickSoftThreshold(datExpr(), powerVector = powers)
    incProgress(0.6, detail = "Detecting module genes ...")
    cor <- WGCNA::cor
    
    if (input$wgcna_cache != T | !file.exists("./Cache/WGCNA_net.rds")) {
      net <- try(
        blockwiseModules(datExpr(), power = sft$powerEstimate, maxBlockSize = input$maxBlockSize,
                         minModuleSize = input$minModuleSize, blockSizePenaltyPower = input$blockSizePenaltyPower,
                         maxPOutliers = input$maxPOutliers, quickCor = input$quickCor, detectCutHeight = input$detectCutHeight,
                         reassignThreshold = input$reassignThreshold, minCoreKME = input$minCoreKME,
                         minKMEtoStay = input$minKMEtoStay, mergeCutHeight = input$mergeCutHeight,
                         impute = input$impute %>% as.logical, corType = input$corType,
                         TOMDenom = input$TOMDenom, pearsonFallback = input$pearsonFallback,
                         networkType = input$blockwise_networkType, TOMType = input$TOMType,
                         deepSplit = input$deepSplit %>% as.integer, saveTOMs = FALSE, verbose = 0)
      )
      saveRDS(net, "./Cache/WGCNA_net.rds")
    }else {
      net <- readRDS("./Cache/WGCNA_net.rds")
    }
    
  })
  return(net)
})

observeEvent(input$moldue_detect, {
  net()
  if ('try-error' %in% class(net())) {
    shinyalert(title = "error", text = paste0(net()[1], " Consider remove batch effects or your data are not suitable for WGCNA analysis !"),
               type = "error", confirmButtonText = "Close")
  }else {
    shinyalert(title = "success", text = "WGCNA Moudule Detection Finished !", type = "success")
  }
})

output$block_id <- renderUI({
  selectInput("block_id", "Which blocks to plot:", choices = seq(1, length(net()$blockGenes)), width = "100%")
})

plotDendro <- reactive({
  withProgress(message = "", value = 0,{
    if (is.null(input$block_id))
      return(NULL)
    # Convert labels to colors for plotting
    mergedColors = labels2colors(net()$colors)
    incProgress(0.6, detail = "Plotting module genes ...")
    # Plot the dendrogram and the module colors underneath
    p <- plotDendroAndColors(net()$dendrograms[[input$block_id %>% as.numeric]], mergedColors[net()$blockGenes[[input$block_id %>% as.numeric]]],
                             "Module colors", dendroLabels = FALSE, hang = 0.03, addGuide = TRUE, guideHang = 0.05)
  })
  return(p)
})

output$plotDendro <- renderPlot({
  plotDendro()
})

output$wgcna_dendroUI <- renderUI({
  withSpinner(plotOutput("plotDendro", width = paste0(input$wgcna_dendro_width, "%"), height = paste0(input$wgcna_dendro_height, "px")))
})

output$plotDendro_Pdf <- downloadHandler(
  filename = function()  {paste0("WGCNA_Cluster_Dendrogram_Plot",".pdf")},
  content = function(file) {
    pdf(file, width = input$plotDendro_width, height = input$plotDendro_height)
    mergedColors = labels2colors(net()$colors)
    # Plot the dendrogram and the module colors underneath
    plotDendroAndColors(net()$dendrograms[[input$block_id %>% as.numeric]], mergedColors[net()$blockGenes[[input$block_id %>% as.numeric]]],
                        "Module colors", dendroLabels = FALSE, hang = 0.03, addGuide = TRUE, guideHang = 0.05)
    dev.off()
  }
)

##----------------------------------------------------
## Gene Table 

moduleColors <- reactive({
  moduleColors = labels2colors(net()$colors)
  names(moduleColors) <- names(net()$colors)
  return(moduleColors)
})

moduleGene_table <- reactive({
  data.frame(moduleGene = names(moduleColors()), moduleColors = moduleColors())
})

output$moduleGene_table <- renderDataTable({
  moduleGene_table()
},rownames = T, options = list(pageLength = 10, autoWidth = F, scrollX=TRUE))

output$moduleGene_table_csv <- downloadHandler(
  filename = function()  {paste0("moduleGene_table",".csv")},
  content = function(file) {
    write.csv(moduleGene_table(), file, row.names = F)
  }
)