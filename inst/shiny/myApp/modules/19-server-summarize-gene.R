observe({
  if (input$ngenie3 | input$psfunc) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "sgene")
  }
})

output$intgs_degs <- renderUI({
  selectInput(
    inputId = "intgs_degs", label = "DEGs:",
    choices = dir("DEGs") %>% stringr::str_remove_all(".csv"),
    selected = stringr::str_remove_all(dir("DEGs"), ".csv")[1], width = "100%")
})

output$intgs_degps <- renderUI({
  if (is.null(degsp_object())){
    return(NULL)
  }else {
    selectInput(inputId = "intgs_degps", label = "Select Patterns ID:", width = "100%",
                choices = degsp_object()$normalized$cluster %>% unique %>% as.character,
                selected = (degsp_object()$normalized$cluster %>% unique %>% as.character)[1])
  }
})

output$intgs_wgcnaps <- renderUI({
  if (is.null(datExpr()) | is.null(moduleColors())){
    return(NULL)
  }else {
    MEs0 = moduleEigengenes(datExpr(), moduleColors())$eigengenes
    MEs = orderMEs(MEs0)
    selectInput(inputId = "intgs_wgcnaps", label = "Select WGCNA Modules ID:",
                choices = c("None", substring(names(MEs), first = 3)),
                selected = substring(names(MEs), first = 3)[1], width = "100%")
  }
})

int_geneList <- eventReactive(input$get_int_genes, {
  withProgress(message = "", value = 0,{
    incProgress(0.2, detail = "Loading differential genes ...")
    if (is.null(input$intgs_degs)) {
      DeGenes <- NULL
    }else {
      DeGenes <- load.DEGs(input$intgs_degs)[[1]] %>% rownames()
    }
    incProgress(0.3, detail = "Loading WGCNA module genes ...")
    if (is.null(input$intgs_wgcnaps)) {
      wgcnaGenes <- NULL
    }else {
      wgcnaGenes <- names(moduleColors())[moduleColors() == input$intgs_wgcnaps]
    }
    incProgress(0.3, detail = "Loading expression pattern genes ...")
    if (is.null(input$intgs_degps)) {
      degpGenes <- NULL
    }else {
      degpGenes <- degsp_object()$df[degsp_object()$df$cluster == input$intgs_degps, "genes"]
    }
    GeneList <- list(De_Genes = DeGenes, wgcna_Genes = wgcnaGenes, degp_Genes = degpGenes)
  })
  return(GeneList)
})

int_venn <- eventReactive(input$get_int_genes,{
  venn(int_geneList(), zcolor = 'style', ilcs = input$intg_venn_lsize, sncs = input$intg_venn_nsize, box = F)
})

output$int_venn <- renderPlot({
  int_venn()
})

output$intg_plotUI <-  renderUI({
  withSpinner(plotOutput("int_venn", width = paste0(input$intg_venn_plot_width, "%"), height = paste0(input$intg_venn_plot_height, "px")))
})

output$intg_venn_Pdf <- downloadHandler(
  filename = function()  {"Intersected_genes_vennPlot.pdf"},
  content = function(file) {
    p <- int_venn()
    ggsave(file, p, width = input$intg_venn_width, height = input$intg_venn_height)
  }
)

intg_tab <- eventReactive(input$get_int_genes,{
  # De_Genes_int_wgcna_Genes <- intersect(int_geneList()$De_Genes, int_geneList()$wgcna_Genes)
  # if (length(De_Genes_int_wgcna_Genes) == 0) {
  #   int_1 <- "NULL"
  # }else {
  #   int_1 <- De_Genes_int_wgcna_Genes %>% paste(collapse = "/")
  # }
  # De_Genes_int_degp_Genes <- intersect(int_geneList()$De_Genes, int_geneList()$degp_Genes)
  # if (length(De_Genes_int_degp_Genes) == 0) {
  #   int_2 <- "NULL"
  # }else {
  #   int_2 <- De_Genes_int_degp_Genes %>% paste(collapse = "/")
  # }
  # wgcna_Genes_int_degp_Genes <- intersect(int_geneList()$wgcna_Genes, int_geneList()$degp_Genes)
  # if (length(wgcna_Genes_int_degp_Genes) == 0) {
  #   int_3 <- "NULL"
  # }else {
  #   int_3 <- wgcna_Genes_int_degp_Genes %>% paste(collapse = "/")
  # }
  De_int_degp_in_wgcna <- intersect(int_geneList()$De_Genes, int_geneList()$wgcna_Genes) %>%
      intersect(int_geneList()$degp_Genes)
  if (length(De_int_degp_in_wgcna) == 0) {
    int_4 <- NULL
  }else {
    if (!is.null(int_geneList()$De_Genes)) {
      int_4 <- load.DEGs(input$intgs_degs)[[1]][load.DEGs(input$intgs_degs)[[1]] %>% rownames %in% De_int_degp_in_wgcna, ]
    }else {
      int_4 <- data.frame(row.names = De_int_degp_in_wgcna, genes = De_int_degp_in_wgcna)
    }
  }
  # data.frame(groups = c("DEGs_intersect_DEG.Patterns", "DEGs_intersect_WGCNA.genes", "DEG.Patterns_intersect_WGCNA.genes", "Intersected_genes_of_the_three"),
  #            genes = c(int_1, int_2, int_3, int_4))
  return(int_4)
})

output$intgs_dfs <- renderDataTable({
  intg_tab()
},rownames = T, options = list(pageLength = 10, autoWidth = F, scrollX=TRUE))

output$intgs_dfs_Csv <- downloadHandler(
  filename = function()  {"Intersected_genes_table.csv"},
  content = function(file) {
    write.csv(intg_tab(), file, row.names = T)
  }
)
