observe({
  if (input$nPPI) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "genie3")
  }
})

output$genie3_group <- renderUI({
  if (input$genie3_genes=="Differential Genes") {
    pickerInput(inputId = "genie3_group", "Groups Of Differential Expressed Genes:", choices = dir("DEGs") %>% stringr::str_remove_all(".csv"),
                width = "100%", multiple = T, options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 5))
  }else if (input$genie3_genes=="Pattern Genes") {
    if (input$run_expp != 0) {
      pickerInput(inputId = "genie3_patterns", label = "Select Patterns ID:", choices = expp_object()$normalized$cluster %>% unique %>% as.character,
                  width = "100%", multiple = T, options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 5) )
    }else {
      p("*Please Run Expression Patterns First!", style = "color: red")
    }
  }else if (input$genie3_genes=="WGCNA Module Genes") {
    if (input$plot_mtrs !=0) {
      MEs0 = moduleEigengenes(datExpr(), moduleColors())$eigengenes
      MEs = orderMEs(MEs0)
      pickerInput(inputId = "genie3_modules", label = "Select WGCNA Modules ID:", choices = substring(names(MEs), first = 3), width = "100%",
                  multiple = T, options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 5) )
    }else {
      p("*Please Run WGCNA First!", style = "color: red")
    }
  }
})

observeEvent(input$get_DEGs,{
  updatePickerInput(
    session = session, inputId = "genie3_group",
    choices = dir("DEGs") %>% stringr::str_remove_all(".csv")
  )
})


# output$genie3_group <- renderUI({
#   if (input$genie3_genes=="Differential Genes") {
#     selectInput(
#       inputId = "genie3_group",
#       label = "Select group:",
#       choices = setdiff(dds()$condition %>% unique %>% as.character, input$genie3_ref),
#       selected = setdiff(dds()$condition %>% unique %>% as.character, input$genie3_ref)[1],
#       width = "100%",
#       multiple = T
#     )
#   }
# })

genie3_object <- eventReactive(input$run_genie, {
  withProgress(message = '', value = 0, {
    if (input$genie3_genes=="Differential Genes") {
      incProgress(0.2, detail = "Extract differential expressed genes ...")
      Des <- load.DEGs(input$genie3_group)
      GeneList <- lapply(Des, function(x){
        rownames(x)
      }) %>% unlist %>% unique
    }else if (input$genie3_genes=="Pattern Genes") {
      incProgress(0.2, detail = "Extract expression pattern genes ...")
      GeneList <- lapply(input$genie3_patterns, function(x){
        expp_object()$df[expp_object()$df$cluster == x, "genes"]
      }) %>% unlist %>% unique
    }else {
      incProgress(0.2, detail = "Extract WGCNA module genes ...")
      GeneList <- lapply(input$genie3_modules, function(x){
        names(moduleColors())[moduleColors() == x]
      }) %>% unlist %>% unique
    }

    exprMatr <- assay(trans_value())
    exprMatr <- exprMatr[rownames(exprMatr) %in% GeneList, ]

    incProgress(0.5, detail = "Running GENIE3 ...")
    weightMat <- GENIE3(exprMatr, nCores=5)
    linkList <- getLinkList(weightMat)
  })
  return(linkList)
})

observeEvent(input$run_genie, {
  js$collapse("run_genie_card")
  genie3_object()
})

output$genie3_Intgenes <- renderUI({
  selectInput(
    inputId = "genie3_Intgenes",
    label = "Insterested genes:",
    choices = genie3_object()$regulatoryGene %>% as.character %>% unique ,
    width = "100%",
    multiple = T
  )
})

genie3_plot <- eventReactive(input$plot_genie3, {
  genie3_object <- genie3_object()[genie3_object()$regulatoryGene %in% input$genie3_Intgenes, ]
  genie3_object <- genie3_object[order(genie3_object$weight, decreasing = T), ]
  genie3_object <- head(genie3_object, input$top_genie3_genes %>% as.numeric)

  if (input$genie3_plotTypes == "edgebundle") {
    mygraph <- graph_from_data_frame(genie3_object)
    nodes <- unique(c(genie3_object$targetGene  %>% as.character, genie3_object$regulatoryGene  %>% as.character))

    angle <- 360 * (c(1:length(nodes)) - 0.5)/length(nodes)
    hjust <- ifelse(angle > 180, 1.05, -0.05)
    angle <- ifelse(angle > 180, 90 - angle + 180, 90 - angle)

    # p <- edgebundle(mygraph, fontsize = input$edgebundle_fontsize)
    p <- ggraph(mygraph, 'linear', circular = TRUE)+
      geom_edge_arc(edge_width=0.5, color = input$edgeARCcolor, show.legend = F)+
      geom_node_point(size = input$edgebundle_nodesize, color = input$edgeNDcolor, alpha = 0.7)+
      geom_node_text(aes(label = nodes), angle = angle, size = input$edgebundle_fontsize, hjust = hjust, alpha = 1)+
      theme_graph()+
      expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))
  }else {
    edges <- data.frame(from = genie3_object$regulatoryGene, to = genie3_object$targetGene)
    nodes <- data.frame(id= unique(union(as.vector(edges$from),as.vector(edges$to))))

    p <- visNetwork(nodes, edges, height = "500px", width = "100%") %>%
      visNodes(size = input$visNetwork_nodesize, font = list(size = input$visNetwork_fontsize))%>%
      visOptions(highlightNearest = list(enabled = T, degree = input$visNetwork_degree, hover = T), nodesIdSelection = TRUE) %>%
      visPhysics(stabilization = FALSE) %>%
      visEdges(arrows = "to", smooth = as.logical(input$visNetwork_smooth)) %>%
      visLayout(randomSeed = 42)
  }
  return(p)
})

# output$Edgebundle_Plot <- renderEdgebundle({
#   genie3_plot()
# })
output$Edgebundle_Plot <- renderPlot({
  genie3_plot()
})

output$visNetwork_Plot <- renderVisNetwork({
  genie3_plot()
})

output$genie3_PlotUI <- renderUI({
  if (input$genie3_plotTypes == "edgebundle") {
    withSpinner(plotOutput("Edgebundle_Plot", height = paste0(input$genie3_plot_height, "px"), width = paste0(input$genie3_plot_width, "%")))
  }else {
    withSpinner(visNetworkOutput("visNetwork_Plot", height = paste0(input$genie3_plot_height, "px"), width = paste0(input$genie3_plot_width, "%")))
  }
})

output$edgebundle_Pdf <- downloadHandler(
  filename = function()  {
    paste0("GENIE3_network_edgebundle_plot",".pdf")
  },
  content = function(file) {
    ggsave(file, genie3_plot(), width = input$genie3_width, height = input$genie3_height)
  }
)

output$genie3_visNetwork <- downloadHandler(
  filename = function()  {
    paste0("GENIE3_network_visNetwork_plot",".html")
  },
  content = function(file) {
    visNetwork::visSave(graph = genie3_plot(), file = file)
  }
)

# # OUTPUT DataFrame
genie3_linkList <- eventReactive(input$plot_genie3, {
  genie3_object <- genie3_object()[genie3_object()$regulatoryGene %in% input$genie3_Intgenes, ]
  genie3_object <- genie3_object[order(genie3_object$weight, decreasing = T), ]
  genie3_object <- head(genie3_object, input$top_genie3_genes %>% as.numeric)
})

output$linkList <- renderDataTable({
  genie3_linkList()
},rownames = T,
options = list(pageLength = 5, autoWidth = F, scrollX=TRUE, scrollY=TRUE)
)

output$linkList_download <- downloadHandler(
  filename = function()  {paste0("Top_", input$top_genie3_genes, "_genie2_network_of_", paste(input$genie3_Intgenes, collapse = "_"), ".csv")},
  content = function(file) {
    write.csv(genie3_linkList(), file)
  }
)
