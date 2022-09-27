observe({
  # if (input$nPattern | input$pModule | input$pORA) {
  if (input$nWGCNA_1 | input$pWGCNA_3) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "wgcna-2")
  }
})

## --------------------------------------------------------------
## module detection
SoftThreshold <- eventReactive(input$cal_power,{
  withProgress(message = "", value = 0, {
    require(ggplot2)
    incProgress(0.2, detail = "Calculating SoftThreshold ...")
    powers = c(c(1:10), seq(from = 12, to=30, by=2))
    cor <- WGCNA::cor
    bicor <- WGCNA::bicor
    if (input$power_corFnc == "cor") {
      sft = WGCNA::pickSoftThreshold(datExpr(), powerVector = powers, corFnc = cor, 
                                     RsquaredCut = input$power_RsquaredCut, nBreaks = input$power_nBreaks, 
                                     networkType = input$power_networkType, moreNetworkConcepts = as.logical(input$moreNetworkConcepts))
    }else {
      sft = WGCNA::pickSoftThreshold(datExpr(), powerVector = powers, corFnc = bicor, 
                                     RsquaredCut = input$power_RsquaredCut, nBreaks = input$power_nBreaks, 
                                     networkType = input$power_networkType, moreNetworkConcepts = as.logical(input$moreNetworkConcepts))
    }
    
    incProgress(0.6, detail = "generating plots ...")
    df <- sft$fitIndices
    p1 <- ggplot(df, aes(x = Power, y = -sign(slope)*SFT.R.sq))+
      geom_text(aes(label = Power), col = "red")+
      labs(x = "Soft Threshold (power)", y = "Scale Free Topology Model Fit,signed R^2")+
      ggtitle("Scale independence")+
      geom_hline(yintercept = 0.85, col = "red", lty = 3)+
      theme_test()+
      theme(plot.title = element_text(hjust = 0.5))

    p2 <- ggplot(df, aes(x = Power, y = mean.k.))+
      geom_text(aes(label = Power), col = "red")+
      labs(x = "Soft Threshold (power)", y = "Mean Connectivity")+
      ggtitle("Mean Connectivity")+
      geom_hline(yintercept = 100, col = "red", lty = 3)+
      theme_test()+
      theme(plot.title = element_text(hjust = 0.5))
    p <- cowplot::plot_grid(p1, p2, ncol = 2)
  })
  return(p)
})

output$SoftThreshold <- renderPlot({
  SoftThreshold()
})

output$wgcna_SoftThresholdUI <- renderUI({
  withSpinner(plotOutput("SoftThreshold", width = paste0(input$wgcna_power_width, "%"), height = paste0(input$wgcna_power_height, "px")))
})

output$SoftThreshold_Pdf <- downloadHandler(
  filename = function()  {paste0("WGCNA_SoftThreshold",".pdf")},
  content = function(file) {
    pdf(file, width = input$SoftThreshold_width, height = input$SoftThreshold_height)
    SoftThreshold()
    dev.off()
  }
)
