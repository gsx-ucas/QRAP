observe({
  if (input$nWGCNA_3 | input$pORA) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "gprofiler2")
  }
})

output$gprofiler_gsets <- renderUI({
  if (input$gprofiler_genes=="DEGs") {
    shinyjs::enable("start_clpr")
    selectInput(
      inputId = "gprofiler_degs", label = "DEGs:",
      choices = dir("DEGs") %>% stringr::str_remove_all(".csv"),
      selected = stringr::str_remove_all(dir("DEGs"), ".csv")[1],
      width = "100%", multiple = T
    )
  }else if (input$gprofiler_genes=="DEG Patterns") {
    if (input$run_degsp == 0) {
      shinyjs::disable("start_clpr")
      selectInput(inputId = "gprofiler_patterns", label = "Select Patterns ID:", width = "100%", multiple = T,
                  choices = "*Please Run DEGs Patterns First !!!", selected = "*Please Run DEGs Patterns First !!!")
      # p("*Please Run DEGs Patterns First!", style = "color: red; padding-top: 30px; padding-bttom: 30px; font-weight: 700px; width: 100%")
    }else {
      shinyjs::enable("start_clpr")
      selectInput(inputId = "gprofiler_patterns", label = "Select Patterns ID:", width = "100%", multiple = T,
                  choices = degsp_object()$normalized$cluster %>% unique %>% as.character)
    }
  }else if (input$gprofiler_genes=="WGCNA Modules") {
    if (input$moldue_detect == 0) {
      shinyjs::disable("start_clpr")
      selectInput(inputId = "gprofiler_modules", label = "Select WGCNA Modules ID:", width = "100%", multiple = T,
                  choices = "*Please Run WGCNA First !!!", selected = "*Please Run WGCNA First !!!")
      # p("*Please Run WGCNA First!", style = "color: red; padding-top: 30px; padding-bttom: 30px; font-weight: 700px; width: 100%")
    }else {
      shinyjs::enable("start_clpr")
      MEs0 = moduleEigengenes(datExpr(), moduleColors())$eigengenes
      MEs = orderMEs(MEs0)
      selectInput(inputId = "gprofiler_modules", label = "Select WGCNA Modules ID:",
                  choices = substring(names(MEs), first = 3), width = "100%", multiple = T)
    }
  }
})

observeEvent(input$get_DEGs,{
  updateSelectInput( session = session, inputId = "gprofiler_degs", choices = dir("DEGs") %>% stringr::str_remove_all(".csv") )
})

gprofiler_object <- eventReactive(input$runGprofiler,{
  withProgress(message = "", min = 0, max = 1, value = 0, {
    if (input$gprofiler_genes=="DEGs") {
      incProgress(0.2, detail = "Loading DEGs ...")
      DeGenes <- load.DEGs(input$gprofiler_degs)
      GeneList <- lapply(DeGenes, function(x){ genes <- rownames(x) })
    }else if (input$gprofiler_genes=="DEG Patterns") {
      GeneList <- lapply(input$gprofiler_patterns, function(x){ degsp_object()$df[degsp_object()$df$cluster == x, "genes"] })
      names(GeneList) <- input$gprofiler_patterns
    }else if (input$gprofiler_genes=="WGCNA Modules") {
      GeneList <- lapply(input$gprofiler_modules, function(x){ names(moduleColors())[moduleColors() == x] })
      names(GeneList) <- input$gprofiler_modules
    }

    if (length(GeneList) > 1) {
      incProgress(0.6, detail = "Running gProfiler ...")
      gostres <- gost(query = GeneList,
                      sources = input$gprofiler_sources,
                      organism = species()$id[species()$display_name == input$gprofiler_species],
                      user_threshold = input$gprofiler_pval,
                      correction_method = input$gprofiler_cor_method,
                      evcodes = as.logical(input$gprofiler_evcodes),
                      exclude_iea = as.logical(input$gprofiler_excludeIEA),
                      significant = as.logical(input$gprofiler_significant))
    }else {
      incProgress(0.6, detail = "Running gProfiler ...")
      gostres <- gost(query = GeneList %>% unlist,
                      sources = input$gprofiler_sources,
                      organism = species()$id[species()$display_name == input$gprofiler_species],
                      user_threshold = input$gprofiler_pval,
                      correction_method = input$gprofiler_cor_method,
                      evcodes = as.logical(input$gprofiler_evcodes),
                      exclude_iea = as.logical(input$gprofiler_excludeIEA),
                      significant = as.logical(input$gprofiler_significant))
    }
  })
  return(gostres)
})

##-----------------------------------------------------------------
observeEvent(input$runGprofiler,{
  js$collapse("gprofiler_tab")
  gprofiler_object()
  if (dim(gprofiler_object()$result)[1] != 0) {
    shinyjs::enable("Plot_gprofiler")
    shinyalert(title = "Run of gProfiler finished!", type = "success")
  }else {
    shinyjs::disable("Plot_gprofiler")
    shinyalert(title = "warning", text = "No Tems Was Enriched !!!", type = "warning")
  }
})

##-----------------------------------------------------------
## Visualize Enrichment results
output$sourceTypes <- renderUI({
  if (input$gprofiler_type=='dotplot' | input$gprofiler_type=='exprs_heatmap') {
    result_data <- gprofiler_object()$result
    source <- result_data$source %>% unique()
    selectInput("sourceTypes", "Source to show", choices = source, selected = source[1], multiple = F, width = "100%")
  }
})

output$gprofiler_termID <- renderUI({
  if (input$gprofiler_type=='gostplot' | input$gprofiler_type=='gosttable') {
    result_data <- gprofiler_object()$result
    id <- result_data$term_id
    names(id) <- paste0("(", result_data$source, ")", result_data$term_name)
    if (length(id) != 0) {
      pickerInput("gprofiler_termID", "Terms to highlight:", choices = id, selected = id[1:10], options = list(`live-search` = TRUE, `actions-box` = TRUE, size = 5), multiple = T, width = "100%")
    }
  }else if (input$gprofiler_Top=='custom select terms') {
    result_data <- gprofiler_object()$result[gprofiler_object()$result$source == input$sourceTypes, ]
    id <- result_data$term_name
    if (length(id) != 0) {
      pickerInput("gprofiler_termID", "Terms to Plot:", choices = id, selected = id[1:10], options = list(`live-search` = TRUE, `actions-box` = TRUE, size = 5),multiple = T,width = "100%")
    }
  }
})

# output$sourceTypes2 <- renderUI({
#   result_data <- gprofiler_object()$result
#   source <- result_data$source %>% unique()
#   selectInput("sourceTypes2","Source to show",choices = source,selected = source[1],multiple = F,width = "100%")
# })
#
output$gprofiler_termID2 <- renderUI({
  result_data <- gprofiler_object()$result[gprofiler_object()$result$source == input$sourceTypes, ]
  id <- result_data$term_name
  if (length(id) != 0) {
    selectInput("gprofiler_termID2","Terms to Plot",choices = id, multiple = F,width = "100%")
  }
})

output$gprofiler_exprs_group <- renderUI({
  pickerInput( inputId = "gprofiler_exprs_group", label = "Select group to plot:",
    dds()$condition %>% unique %>% as.character, selected = dds()$condition %>% unique %>% as.character,
    multiple = T,width = "100%", options = list(`actions-box` = TRUE) )
})

gprofilerPlot <- eventReactive(input$Plot_gprofiler,{
  par(mar=c(2,2,2,2))
  if (input$gprofiler_type == "gostplot") {
    p <- gostplot(gprofiler_object(), capped = FALSE, interactive = FALSE)
    pp <- publish_gostplot2(p, highlight_terms = input$gprofiler_termID, filename = NULL,
                            fontsize = input$gprofiler_tbfontsize, show_link = input$gprofiler_showLink  %>% as.logical,
                            show_columns = input$gprofiler_showColumns)
  }else if (input$gprofiler_type == "gosttable") {
    pp <- publish_gosttable2(gprofiler_object(), highlight_terms = input$gprofiler_termID, use_colors = TRUE,filename = NULL,
                            fontsize = input$gprofiler_tbfontsize, show_link = input$gprofiler_showLink %>% as.logical,
                            show_columns = input$gprofiler_showColumns)
  }else if (input$gprofiler_type == "dotplot") {
    if (input$gprofiler_Top=='custom select terms') {
      plot_terms <- input$gprofiler_termID
    } else {
      plot_terms <- NULL
    }
    pp <- publish_gostdot(object = gprofiler_object(), by = input$gprofiler_orderBy, terms = plot_terms,
                             source = input$sourceTypes, showCategory = input$gprofiler_n_terms, font.size = input$gprofiler_fontsize)
  }else {
    if (dim(gprofiler_object()$result)[1] != 0) {
      geneID <- gprofiler_object()$result[gprofiler_object()$result$term_name %in% input$gprofiler_termID2, "intersection"]
      genes <- str_split(geneID, pattern = ",")[[1]]

      sampleTable <- as.data.frame(colData(dds()))[dds()$condition %in% input$gprofiler_exprs_group, ]
      rownames(sampleTable) <- sampleTable$samples

      # data <- assay(trans_value())

      if (input$gprofiler_data_use == "rel_value") {
        data <- log2(norm_value() + 1) %>% as.data.frame()
      }else if(input$gprofiler_data_use == "trans_value"){
        data <- assay(trans_value()) %>% as.data.frame()
      }else if(input$gprofiler_data_use == "norm_value"){
        data <- norm_value() %>% as.data.frame()
      }

      if (length(genes)==1) {
        Sub_data <- data[rownames(data) %in% genes, sampleTable$samples] %>% t
        rownames(Sub_data) <-  genes
      }else {
        Sub_data <- data[rownames(data) %in% genes, sampleTable$samples]
      }

      annotation_col = data.frame(condition = factor(sampleTable$condition))
      rownames(annotation_col) = sampleTable$samples
      color = colorRampPalette(c("navy", "white", "red"))(50)

      pp <- pheatmap(Sub_data, col=color,
                     cluster_col=F, cluster_row=input$gprofiler_cluster_row,
                     scale = 'row', show_rownames = T,
                     show_colnames = F, breaks=seq(input$gprofiler_cluster_break[1], input$gprofiler_cluster_break[2],
                                                   (input$gprofiler_cluster_break[2] - input$gprofiler_cluster_break[1])/50),
                     annotation_col = annotation_col, fontsize = input$gprofiler_heatmap_fontsize,
                     main = paste0(input$sourceTypes, ": ", input$gprofiler_termID2))

    }else {
      return(NULL)
    }
  }
  return(pp)
})

output$gprofilerPlot <- renderPlot({
  gprofilerPlot()
})

output$gprofilerPlotUI <-  renderUI({
  withSpinner(plotOutput("gprofilerPlot", width = paste0(input$gprofiler_plot_width, "%"), height = paste0(input$gprofiler_plot_height, "px")))
})

output$gprofiler_Pdf <- downloadHandler(
  filename = function()  {paste0("gProfiler_Plot",".pdf")},
  content = function(file) {
    p <- gprofilerPlot()
    ggsave(file, p, width = input$gprofiler_width, height = input$gprofiler_height, limitsize = FALSE)
  }
)

##-----------------------------------------------------------
## Enrichment results Table

output$sourceID <- renderUI({
  result_data <- gprofiler_object()$result
  source <- result_data$source %>% unique()
  selectInput(
    "sourceID",
    "Source to show",
    choices = source,
    selected = source[1],
    multiple = T,
    width = "50%"
  )
})

output$gProfiler_Tab <- renderDataTable({
  tab <- gprofiler_object()$result[gprofiler_object()$result$source %in% input$sourceID, ]
  if (input$gprofiler_evcodes == "TRUE") {
    tab <- tab[ , !colnames(tab) %in% "evidence_codes"]
  }
},rownames = T,
options = list(pageLength = 5, autoWidth = F, scrollX=TRUE, scrollY=TRUE)
)

output$gprofiler_csv <- downloadHandler(
  filename = function()  {paste0("gProfiler_Table",".csv")},
  content = function(file) {
    table <- gprofiler_object()$result[gprofiler_object()$result$source %in% input$sourceID, !colnames(gprofiler_object()$result) %in% "parents"]
    if (input$gprofiler_evcodes == "TRUE") {
      table <- table[ , !colnames(table) %in% "evidence_codes"]
    }
    write.csv(table, file, row.names = T)
  }
)
