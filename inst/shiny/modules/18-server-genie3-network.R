observe({
  if (input$nPPI | input$psgene) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "genie3")
  }
})

output$genie3_group <- renderUI({
  if (input$genie3_genes=="Differential Genes") {
    selectizeInput(inputId = "genie3_group", "Regulators (DEGs):",
                choices = dir("DEGs") %>% stringr::str_remove_all(".csv"),
                selected = (dir("DEGs") %>% stringr::str_remove_all(".csv"))[1],
                width = "100%", multiple = F)
  }else if (input$genie3_genes=="Pattern Genes") {
    if (input$run_degsp != 0) {
      selectizeInput(inputId = "genie3_patterns", label = "Regulators (Patterns):",
                  choices = degsp_object()$normalized$cluster %>% unique %>% as.character,
                  selected = (degsp_object()$normalized$cluster %>% unique %>% as.character)[1],
                  width = "100%", multiple = F)
    }else {
      p("*Please Run Expression Patterns First!", style = "color: red")
    }
  }else if (input$genie3_genes=="WGCNA Module Genes") {
    if (input$plot_mtrs !=0) {
      MEs0 = moduleEigengenes(datExpr(), moduleColors())$eigengenes
      MEs = orderMEs(MEs0)
      selectizeInput(inputId = "genie3_modules", label = "Regulators (Modules):",
                  choices = substring(names(MEs), first = 3), selected = (substring(names(MEs), first = 3))[1],
                  width = "100%", multiple = F)
    }else {
      p("*Please Run WGCNA First!", style = "color: red")
    }
  }
})

observeEvent(input$get_DEGs,{
  if (input$genie3_genes=="Differential Genes") {
    updateSelectInput(
      session = session, inputId = "genie3_group",
      choices = dir("DEGs") %>% stringr::str_remove_all(".csv")
    )
  }
})


g3_linkList <- eventReactive(input$run_genie, {
  withProgress(message = '', value = 0, {
    if (input$genie3_genes=="Differential Genes") {
      incProgress(0.2, detail = "Extract differential expressed genes ...")
      # Des <- load.DEGs(input$genie3_group)
      # GeneList <- lapply(Des, function(x){
      #   rownames(x)
      # }) %>% unlist %>% unique
      regulators <- rownames(load.DEGs(input$genie3_group)[[1]])
    }else if (input$genie3_genes=="Pattern Genes") {
      incProgress(0.2, detail = "Extract expression pattern genes ...")
      # GeneList <- lapply(input$genie3_patterns, function(x){
      #   degsp_object()$df[degsp_object()$df$cluster == x, "genes"]
      # }) %>% unlist %>% unique
      regulators <- degsp_object()$df[degsp_object()$df$cluster == input$genie3_patterns, "genes"]
    }else {
      incProgress(0.2, detail = "Extract WGCNA module genes ...")
      # GeneList <- lapply(input$genie3_modules, function(x){
      #   names(moduleColors())[moduleColors() == x]
      # }) %>% unlist %>% unique
      regulators <- names(moduleColors())[moduleColors() == input$genie3_modules]
    }

    exprMatr <- counts(dds()) # The expression data does not need to be normalised in any particular way
    # exprMatr <- exprMatr[rownames(exprMatr) %in% GeneList, ]

    incProgress(0.5, detail = "Running GENIE3 ...")
    if (parallel::detectCores() < input$g3_core) {
      pCores <- parallel::detectCores() - 1
    }else {
      pCores <- input$g3_core
    }
    weightMat <- GENIE3::GENIE3(exprMatr, regulators = regulators, nCores = pCores, treeMethod = input$g3_method, K = input$g3K, nTrees = input$g3_nTrees)

    linkList <- GENIE3::getLinkList(weightMat, reportMax = input$g3_reportMax)
  })
  return(linkList)
})

observeEvent(input$run_genie, {
  js$collapse("run_genie_card")
  g3_linkList()
  sendSweetAlert(title = "GENIE3 completed!", type = "success")
})

genie3_plot <- eventReactive(input$plot_genie3, {
  g3_linkList <- g3_linkList()[order(g3_linkList()$weight, decreasing = T), ]
  g3_linkList <- g3_linkList[1:min(nrow(g3_linkList), input$g3_top_links), ]

  nodes <- data.frame(nodeName = unique(c(g3_linkList$targetGene %>% as.character, g3_linkList$regulatoryGene  %>% as.character)))
  gene_table <- c(g3_linkList$targetGene %>% as.character, g3_linkList$regulatoryGene %>% as.character) %>% table %>% sort(decreasing = T)
  nodes$connected_number <- gene_table[nodes$nodeName]
  nodes$connected_degree <- nodes$connected_number / sum(nodes$connected_number)
  
  top_n <- nodes[nodes$connected_degree %>% order(decreasing = T) %>% head(input$g3_top_genes), "nodeName"]
  
  nodes$labels <- nodes$nodeName
  nodes[nodes$nodeName %in% top_n, "label_size"] <- 3
  nodes[!nodes$nodeName %in% top_n, "label_size"]  <- 0
  
  mygraph <- igraph::graph_from_data_frame(d = g3_linkList, vertices = nodes, directed=F)
  
  p <- ggnet3(igraph::simplify(mygraph), label = "labels", label.size = "label_size", 
              size = "connected_degree", color = "#97C2FC", label.color = "black", 
              edge.alpha = 0.5, legend.position = "none") %>% 
       plotly::ggplotly(tooltip = "label") %>% plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
  return(p)
})


output$g3_Plot <- renderPlotly({
  genie3_plot()
})


output$genie3_PlotUI <- renderUI({
  withSpinner(plotlyOutput("g3_Plot", height = paste0(input$genie3_plot_height, "px"), width = paste0(input$genie3_plot_width, "%")))
})

output$g3_Pdf <- downloadHandler(
  filename = function()  {
    paste0("GENIE3_network_plot",".pdf")
  },
  content = function(file) {
    genie3_plot()
    ggsave(file, width = input$genie3_width, height = input$genie3_height)
  }
)

# # OUTPUT DataFrame
output$linkList <- renderDataTable({
  g3_linkList()[1:10000, ]
},rownames = T,
options = list(pageLength = 10, autoWidth = F, scrollX=TRUE)
)

output$linkList_download <- downloadHandler(
  filename = function()  {paste0("Top_", input$g3_top_genes, "_genie2_network_table.csv")},
  content = function(file) {
    write.csv(g3_linkList(), file)
  }
)
