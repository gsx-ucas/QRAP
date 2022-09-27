observe({
  if (input$ngenie3 | input$psfunc) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "sgene")
  }
})

output$intgs_degs <- renderUI({
  if ("DEGs" %in% input$intgs_genesets) {
    if (dir("DEGs") %>% stringr::str_remove_all(".csv") %>% length == 0 ) {
      selectInput(inputId = "intgs_degs", label = "DEGs:", width = "100%",
                  choices = c("*No DEG results were detected !" = "NULL"))
    }else {
      selectInput(
        inputId = "intgs_degs", label = "DEGs:",
        choices = dir("DEGs") %>% stringr::str_remove_all(".csv"),
        selected = stringr::str_remove_all(dir("DEGs"), ".csv")[1], width = "100%")
    }
  }
})

output$intgs_degps <- renderUI({
  if ("DEG Pattern" %in% input$intgs_genesets) {
    if (input$run_degsp == 0){
      selectInput(inputId = "intgs_degps", label = "DEG Patterns ID:", width = "100%",
                  choices = c("*No DEG Pattern results were detected !" = "NULL"))
    }else {
      selectInput(inputId = "intgs_degps", label = "DEG Patterns ID:", width = "100%",
                  choices = degsp_object()$normalized$cluster %>% unique %>% as.character,
                  selected = (degsp_object()$normalized$cluster %>% unique %>% as.character)[1])
    }
  }
})

output$intgs_wgcnaps <- renderUI({
  if ("WGCNA module" %in% input$intgs_genesets) {
    if (input$moldue_detect == 0){
      selectInput(inputId = "intgs_wgcnaps", label = "WGCNA Module ID:", width = "100%",
                  choices = c("*No WGCNA module results were detected !" = "NULL"))
    }else {
      MEs0 = moduleEigengenes(datExpr(), moduleColors())$eigengenes
      MEs = orderMEs(MEs0)
      selectInput(inputId = "intgs_wgcnaps", label = "WGCNA Module ID:",
                  choices = substring(names(MEs), first = 3),
                  selected = substring(names(MEs), first = 3)[1], width = "100%")
    }
  }
})

observe({
  req(input$intgs_degs, input$intgs_wgcnaps, input$intgs_degps)
  if (input$intgs_degs == "NULL" & input$intgs_wgcnaps == "NULL" & input$intgs_degps == "NULL") {
    shinyjs::disable("get_int_genes")
  }else if (length(input$intgs_genesets) == 0) {
    shinyjs::disable("get_int_genes")
  }else {
    shinyjs::enable("get_int_genes")
  }
})

int_geneList <- eventReactive(input$get_int_genes, {
  withProgress(message = "", value = 0,{
    incProgress(0.2, detail = "Loading differential genes ...")
    
    GeneList <- list()
    
    if (input$intgs_degs != "NULL") {
      GeneList[["De_Genes"]] <- load.DEGs(input$intgs_degs)[[1]] %>% rownames()
    }
    
    incProgress(0.3, detail = "Loading WGCNA module genes ...")
    if (input$intgs_wgcnaps != "NULL") {
      GeneList[["wgcna_Genes"]] <- names(moduleColors())[moduleColors() == input$intgs_wgcnaps]
    }
    
    incProgress(0.3, detail = "Loading expression pattern genes ...")
    if (input$intgs_degps != "NULL") {
      GeneList[["degp_Genes"]] <- degsp_object()$df[degsp_object()$df$cluster == input$intgs_degps, "genes"]
    }
  })
  return(GeneList)
})

int_venn <- eventReactive(input$get_int_genes,{
  venn::venn(int_geneList(), zcolor = 'style', ilcs = input$intg_venn_lsize, sncs = input$intg_venn_nsize, box = F)
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

output$intgs_df_id <- renderUI({
  if ("De_Genes" %in% names(int_geneList()) & "wgcna_Genes" %in% names(int_geneList())) {
    intgs_id <- c("DEGs_WGCNA")
  }else {
    intgs_id <- NULL
  }
  
  if ("De_Genes" %in% names(int_geneList()) & "degp_Genes" %in% names(int_geneList())) {
    intgs_id <- c(intgs_id, "DEGs_DEGPattern")
  }
  if ("wgcna_Genes" %in% names(int_geneList()) & "degp_Genes" %in% names(int_geneList())) {
    intgs_id <- c(intgs_id, "WGCNA_DEGPattern")
  }
  if ("wgcna_Genes" %in% names(int_geneList()) & "degp_Genes" %in% names(int_geneList()) & "De_Genes" %in% names(int_geneList())) {
    intgs_id <- c(intgs_id, "DEGs_DEGPattern_WGCNA")
  }
  selectInput("intgs_df_id", "Group of intersect Gene:", choices = intgs_id)
})

intg_tab <- reactive({
  req(input$intgs_df_id)
  if (input$intgs_df_id == "DEGs_WGCNA") {
    genes <- intersect(int_geneList()$De_Genes, int_geneList()$wgcna_Genes)
    int_df <- load.DEGs(input$intgs_degs)[[1]][load.DEGs(input$intgs_degs)[[1]] %>% rownames %in% genes, ]
  }else if (input$intgs_df_id == "DEGs_DEGPattern") {
    genes <- intersect(int_geneList()$De_Genes, int_geneList()$degp_Genes)
    int_df <- load.DEGs(input$intgs_degs)[[1]][load.DEGs(input$intgs_degs)[[1]] %>% rownames %in% genes, ]
  }else if (input$intgs_df_id == "WGCNA_DEGPattern") {
    genes <- intersect(int_geneList()$wgcna_Genes, int_geneList()$degp_Genes)
    wgcna_df <- moduleGene_table()[moduleGene_table()$moduleGene %in% genes, ]
    colnames(wgcna_df) <- c("genes", "WGCNA_moduleColor")
    degp_df <- degsp_object()$df[degsp_object()$df$genes %in% genes, ]
    colnames(degp_df) <- c("genes", "DEG_Pattern_clusterID")
    expr_data <- as.data.frame(assay(trans_value()))[as.data.frame(assay(trans_value())) %>% rownames() %in% genes, ]
    expr_data$genes <- rownames(expr_data)
    int_df <- plyr::join_all(list(wgcna_df, degp_df, expr_data), by = "genes", type = "inner")
  }else if (input$intgs_df_id == "DEGs_DEGPattern_WGCNA") {
    genes <- intersect(int_geneList()$De_Genes, int_geneList()$degp_Genes) %>% intersect(int_geneList()$wgcna_Genes)
    int_df <- load.DEGs(input$intgs_degs)[[1]][load.DEGs(input$intgs_degs)[[1]] %>% rownames %in% genes, ]
  }
  return(int_df)
})

output$intgs_dfs <- renderDataTable({
  intg_tab()
},rownames = T, options = list(pageLength = 10, autoWidth = F, scrollX=TRUE, scrollY = "300px"))

output$intgs_dfs_Csv <- downloadHandler(
  filename = function()  {"Intersected_genes_table.csv"},
  content = function(file) {
    write.csv(intg_tab(), file, row.names = T)
  }
)
