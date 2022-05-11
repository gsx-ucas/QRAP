observe({
  # if (input$nCondition | input$pCorr) {
  if (input$nCondition | input$pHiera) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "pca")
  }
})

output$pca_samples <- renderUI({
  pickerInput(
    inputId = "pca_samples", label = "Select Samples:", choices = dds()$samples %>% as.character,
    selected = dds()$samples %>% as.character,
    multiple = T, width = "100%", options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 5)
  )
})

output$pca_colorby <- renderUI({
  if(length(input$pca_samples) == 0)
    return(NULL)
  selectInput(
    inputId = "pca_colorby",
    label = "Group for color legend:",
    choices = colnames(as.data.frame(colData(dds()))[input$pca_samples, ])[!as.data.frame(colData(dds()))[input$pca_samples, ] %in% c("sizeFactor", "replaceable")],
    # choices = subset(colData(dds())[input$pca_samples, ], select = -c(sizeFactor, replaceable)) %>% colnames,
    selected = "condition",
    width = "100%"
  )
})

PCA <- eventReactive(input$plot_pca, {
  pcadata <- plotPCA(trans_value()[, input$pca_samples], intgroup=c("samples", input$pca_colorby),returnData=TRUE)
  percentVar <- round(100 * attr(pcadata, "percentVar"))

  p <- ggplot(pcadata, aes_string('PC1', 'PC2', color = input$pca_colorby)) +
    geom_point(size = input$pca_size) +
    xlab(paste0("PC1:", percentVar[1], "% variance")) +
    ylab(paste0("PC2:", percentVar[2], "% variance")) +
    theme_classic() +
    theme(text = element_text(size = input$pca_fontsize))
  if (input$pca_text_repel == "TRUE") {
    p <- p + geom_text_repel(data = pcadata,
                             aes(pcadata$PC1, pcadata$PC2, label=pcadata$samples),
                             point.padding = 0.25,
                             cex = input$pca_text_cex)
  }
  if (nchar(input$pca_ggText != 0)) {
    add_funcs <- strsplit(input$pca_ggText, "\\+")[[1]]
    p <- p + lapply(add_funcs, function(x){
      eval(parse(text = x))
    })
  }
  return(p)
})

output$PCA <- renderPlot({ PCA() })

output$PCA_plotUI <- renderUI({
  withSpinner(plotOutput("PCA", width = paste0(input$pca_plot_width, "%"), height = paste0(input$pca_plot_height, "px")))
})

output$pca_Pdf <- downloadHandler(
  filename = function()  {paste0("PCA_Plot",".pdf")},
  content = function(file) {
    p <- PCA()
    ggsave(file, p, width = input$pca_width, height = input$pca_height)
  }
)
