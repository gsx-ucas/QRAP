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
  sub_dat <- as.data.frame(dds()@colData)[input$pca_samples, ]
  selectInput(
    inputId = "pca_colorby", label = "Group for color legend:", width = "100%",
    choices = colnames(sub_dat)[!colnames(sub_dat) %in% c("samples", "sizeFactor", "replaceable")]
  )
})

# pca_sample_order <- reactiveValues(values = NULL)
#
# pca_samples <- reactive({
#   if (length(pca_sample_order$values) > length(input$pca_samples)) {
#     pca_sample_order$values <- pca_sample_order$values[pca_sample_order$values %in% input$pca_samples]
#   }else {
#     pca_sample_order$values <- c(pca_sample_order$values, input$pca_samples[!input$pca_samples %in% pca_sample_order$values])
#   }
#   pca_sample_order$values
# })
#
# observe( pca_samples() )

output$pca_shapeby <- renderUI({
  if(length(input$pca_samples) == 0)
    return(NULL)
  sub_dat <- as.data.frame(dds()@colData)[input$pca_samples, ]
  selectInput(
    inputId = "pca_shapeby",
    label = "Group for shape legend:",
    choices = c("NULL", colnames(sub_dat)[!colnames(sub_dat) %in% c("samples", "sizeFactor", "replaceable", input$pca_colorby)]),
    width = "100%"
  )
})

PCA <- eventReactive(input$plot_pca, {
  require(ggplot2)
  if (input$pca_shapeby != "NULL") {
    pcadata <- DESeq2::plotPCA(trans_value()[, input$pca_samples], intgroup=c("samples", input$pca_colorby, input$pca_shapeby),returnData=TRUE)
    p <- ggplot(pcadata, aes_string('PC1', 'PC2', color = input$pca_colorby, shape = input$pca_shapeby))
  }else {
    pcadata <- DESeq2::plotPCA(trans_value()[, input$pca_samples], intgroup=c("samples", input$pca_colorby),returnData=TRUE)
    p <- ggplot(pcadata, aes_string('PC1', 'PC2', color = input$pca_colorby))
  }
  percentVar <- round(100 * attr(pcadata, "percentVar"))

  p <- p + geom_point(size = input$pca_size) +
    xlab(paste0("PC1:", percentVar[1], "% variance")) +
    ylab(paste0("PC2:", percentVar[2], "% variance"))
  if (input$pca_text_repel == "TRUE") {
    require(ggrepel)
    p <- p + geom_text_repel(data = pcadata,
                             aes(pcadata$PC1, pcadata$PC2, label=pcadata$samples),
                             point.padding = 0.25,
                             cex = input$pca_text_cex)
  }
  p <- p + eval(parse(text = paste0(input$pca_theme, "()")))
  if (nchar(input$pca_ggText) != 0) {
    add_funcs <- strsplit(input$pca_ggText, "\\+")[[1]]
    p <- p + lapply(add_funcs, function(x){
      eval(parse(text = x))
    })
  }
  return(p)
})

output$PCA <- renderPlot({ PCA() })

output$PCA_plotUI <- renderUI({
  shinycssloaders::withSpinner(plotOutput("PCA", width = paste0(input$pca_plot_width, "%"), height = paste0(input$pca_plot_height, "px")))
})

output$pca_Pdf <- downloadHandler(
  filename = function()  {paste0("PCA_Plot",".pdf")},
  content = function(file) {
    p <- PCA()
    ggplot2::ggsave(file, p, width = input$pca_width, height = input$pca_height)
  }
)
