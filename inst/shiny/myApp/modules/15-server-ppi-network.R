observe({
  if (input$nGprofiler) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "ppi")
  }
})

STRING_species <- reactive({
  species <- readRDS(system.file("shiny", "myApp/www/Species/stringDB_species.rds", package = "QRseq"))
})

output$STRING_species <- renderUI({
  selectizeInput("string_species", "Select species:", choices = c("", STRING_species()$official_name), width = "100%")
})

# string_db <- reactive({
#   if(!dir.exists("./stringDB"))
#     dir.create("./stringDB")
#   string_species <- STRING_species()[STRING_species()$official_name == input$string_species, "species_id"]
#   if (input$score_threshold <= 400) {
#     score <- input$score_threshold
#   }else {
#     score <- 400
#   }
#   string_db <- STRINGdb$new(species=string_species, score_threshold=score, input_directory="./stringDB/")
# })
#
# observeEvent(input$start_STRINGdb,{
#   js$collapse("start_ppi")
#   string_db()
# })

# # # Select first group of samples
# output$ppi_ref <- renderUI({
#   selectInput(
#     inputId = "ppi_ref",
#     label = "Select Reference:",
#     choices = dds()$condition %>% unique %>% as.character,
#     width = "100%"
#   )
# })
#
# output$ppi_group <- renderUI({
#   selectInput(
#     inputId = "ppi_group",
#     label = "Select group:",
#     choices = setdiff(dds()$condition %>% unique %>% as.character, input$ppi_ref),
#     # selected = setdiff(sampeTable()$condition %>% unique %>% as.character, input$heat_ref),
#     width = "100%"
#   )
# })

output$ppi_group <- renderUI({
  pickerInput(
    inputId = "ppi_group", "Groups Of Differential Expressed Genes:", choices = dir("DEGs") %>% stringr::str_remove_all(".csv"),
    width = "100%", options = list(`live-search` = TRUE, size = 5))
})

observeEvent(input$get_DEGs,{
  updatePickerInput(
    session = session, inputId = "ppi_group",
    choices = dir("DEGs") %>% stringr::str_remove_all(".csv")
  )
})

output$ppi_degenes <- renderUI({
  pickerInput(
    inputId = "ppi_degenes", "Select DEGs to plot:", choices = rownames(load.DEGs(input$ppi_group)[[1]]),
    width = "100%", multiple = T, options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 5))
})

string_db <- eventReactive(input$plot_STRINGdb, {
  withProgress(message = "",{
    if(!dir.exists("./stringDB"))
      dir.create("./stringDB")
    string_species <- STRING_species()[STRING_species()$official_name == input$string_species, "species_id"]
    if (input$score_threshold <= 400) {
      score <- input$score_threshold
    }else {
      score <- 400
    }
    incProgress(0.5, detail = "Initializing & downloading data, please wait...")
    string_db <- STRINGdb$new(version = "11", species=string_species, score_threshold=score, input_directory="./stringDB/")
  })
  return(string_db)
})

PPI_hits <- eventReactive(input$plot_STRINGdb, {
  withProgress(message = "",{
    string_db <- string_db()

    incProgress(0.2, detail = "extract differential expression genes...")
    Des <- load.DEGs(input$ppi_group)[[1]]
    diff_exp <- data.frame(pvalue=Des$padj, logFC=Des$log2FoldChange, gene=rownames(Des))

    if (input$ppi_genes == "topN") {
      diff_exp <- diff_exp[order(diff_exp$pvalue, decreasing = F), ]

      incProgress(0.6, detail = "mapping genes to protein, please wait a while...")
      print("start mapping ....")
      diff_mapped <- string_db$map(diff_exp, "gene", removeUnmappedRows = TRUE)
      hits <- diff_mapped$STRING_id[1:input$top_ppi_genes]
    }else {
      diff_exp <- diff_exp[diff_exp$gene %in% input$ppi_degenes, ]

      incProgress(0.6, detail = "mapping genes to protein, please wait a while...")
      print("start mapping ....")
      diff_mapped <- string_db$map(diff_exp, "gene", removeUnmappedRows = TRUE)
      hits <- diff_mapped$STRING_id
    }
  })
  return(hits)
})

# PPI_plot <- eventReactive(input$plot_STRINGdb, {
#   # withProgress(message = "",{
#     string_db <- string_db()
#     hits <- PPI_hits()
#     # incProgress(0.6, detail = "plotting, please wait a while...")
#     string_db$plot_network( hits )
#   # })
#   # return(p)
# })

# observeEvent(input$plot_STRINGdb, {
#   string_db()
#   PPI_hits()
#   # PPI_plot()
#   # if ('try-error' %in% class(PPI_plot())) {
#   #   shinyalert(title = "error", text = PPI_plot()[1], type = "error", confirmButtonText = "Close")
#   # }else {
#   #   shinyalert(title = "success", text = "PPI Network Analysis has completed !", type = "success")
#   # }
# })

output$PPI_Plot <- renderPlot({
  withProgress(message = "",{
    string_db <- string_db()
    hits <- PPI_hits()
    incProgress(0.6, detail = "plotting, please wait a while...")
    string_db$plot_network( hits )
  })
  # PPI_plot()
})

output$PPI_PlotUI <- renderUI({
  withSpinner(plotOutput("PPI_Plot", height = paste0(input$ppi_plot_height, "px"), width = paste0(input$ppi_plot_width, "%")))
})

output$ppi_Pdf <- downloadHandler(
  filename = function()  {paste0("Protein_to_protein_network_plot",".pdf")},
  content = function(file) {
    withProgress(message = "",{
      pdf(file, width = input$ppi_width, height = input$ppi_height)
      string_db <- string_db()
      hits <- PPI_hits()
      incProgress(0.6, detail = "plotting, please wait a while...")
      p <- string_db$plot_network( hits )
      dev.off()
    })
  }
)
