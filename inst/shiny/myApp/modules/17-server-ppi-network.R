observe({
  if (input$nPathview | input$pgenie3) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "ppi")
  }
})

# STRING_species <- reactive({
#   species <- readRDS(system.file("shiny", "myApp/www/Species/stringDB_species.rds", package = "QRseq"))
# })
#
# output$STRING_species <- renderUI({
#   selectizeInput("string_species", "Select species:", choices = c("", STRING_species()$official_name), width = "100%")
# })

output$ppi_group <- renderUI({
  selectInput(
    inputId = "ppi_group", "Groups Of Differential Expressed Genes:", choices = dir("DEGs") %>% stringr::str_remove_all(".csv"),width = "100%")
})

observeEvent(input$get_DEGs,{
  updatePickerInput(
    session = session, inputId = "ppi_group",
    choices = dir("DEGs") %>% stringr::str_remove_all(".csv")
  )
})

# output$ppi_degenes <- renderUI({
#   pickerInput(
#     inputId = "ppi_degenes", "Select DEGs to plot:", choices = rownames(load.DEGs(input$ppi_group)[[1]]),
#     width = "100%", multiple = T, options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 5))
# })

string_db <- eventReactive(input$Init_STRINGdb, {
  withProgress(message = "",{
    string_species <- species()$taxon_id[species()$display_name == input$gprofiler_species] %>% as.integer
    if (input$score_threshold <= 400) {
      score <- input$score_threshold
    }else {
      score <- 400
    }
    incProgress(0.5, detail = "Initializing & downloading data, please wait...")
    string_dir <- system.file("stringDB", package = "QRseq")
    string_db <- STRINGdb$new(version = "11", species=string_species, score_threshold=score, input_directory = string_dir)
  })
  return(string_db)
})

diff_exp <- eventReactive(input$Init_STRINGdb, {
  withProgress(message = "",{
    incProgress(0.2, detail = "extract differential expression genes...")
    Des <- load.DEGs(input$ppi_group)[[1]]
    diff_exp <- data.frame(pvalue=Des$padj, logFC=Des$log2FoldChange, gene=rownames(Des))
  })
  return(diff_exp)
})

PPI_hits <- eventReactive(input$Init_STRINGdb, {
  withProgress(message = "",{
    string_db <- string_db()
    string_species <- species()$taxon_id[species()$display_name == input$gprofiler_species]

    incProgress(0.6, detail = "mapping genes to protein, please wait a while...")

    diff_mapped <- try(string_db$map(diff_exp(), "gene", removeUnmappedRows = TRUE))

    if ('try-error' %in% class(diff_mapped)) {
      string_dir <- system.file("stringDB", package = "QRseq")
      file.remove(dir(string_dir, pattern = paste0(string_species, ".protein.aliases"), full.names = T))
      file.remove(dir(string_dir, pattern = paste0(string_species, ".protein.info"), full.names = T))
      shinyalert(title = "error", text = paste0(diff_mapped[1], ", Please check your network or try again!"), type = "error", confirmButtonText = "Close")
      hits <- NULL
    }else {
      hits <- diff_mapped$STRING_id

      if (input$ppi_cluster == "TRUE") {
        incProgress(0.2, detail = "clustering proteins, please wait a while...")
        hits <- try(string_db$get_clusters(hits))

        if ('try-error' %in% class(hits)) {
          string_dir <- system.file("stringDB", package = "QRseq")
          file.remove(dir(string_dir, pattern = paste0(string_species, ".protein.links"), full.names = T))

          shinyalert(title = "error", text = paste0(diff_mapped[1], ", Please check your network or try again!"), type = "error", confirmButtonText = "Close")
          hits <- NULL
        }
      }
    }
  })
  return(hits)
})

observeEvent(input$Init_STRINGdb, {
  js$collapse("run_ppi_card")
})

output$string_cluster <- renderUI({
  if (input$ppi_cluster == "TRUE" & !is.null(PPI_hits())) {
    selectInput("ppi_cluster_id", "Protein cluster ID:", choices = 1:length(PPI_hits()), selected = 1, width = "100%")
  }
})

observeEvent(input$plot_STRINGdb, {
  withProgress(message = "",{
    if(is.null(PPI_hits()))
      return(NULL)
    string_db <- string_db()
    if (is.list(PPI_hits())) {
      hits <- PPI_hits()[[as.numeric(input$ppi_cluster_id)]]
      ppiview_name <- paste0(input$ppi_group, "_", input$ppi_cluster_id, "_ppi_network.pdf")
    }else {
      hits <- PPI_hits()
      ppiview_name <- paste0(input$ppi_group, "_ppi_network.pdf")
    }

    if (!dir.exists("www/PPI_network")) {
      dir.create("www/PPI_network", recursive = T)
    }

    incProgress(0.6, detail = "plotting, please wait a while...")
    pdf(paste0("www/PPI_network/", ppiview_name), width = input$ppi_pdfsize[1], height = input$ppi_pdfsize[2])
    string_db$plot_network( hits )
    dev.off()

    output$PPI_iframe <- renderUI({
      if(is.null(PPI_hits()))
        return(NULL)
      if (is.list(PPI_hits())) {
        if (file.exists(paste0("www/PPI_network/", input$ppi_group, "_", input$ppi_cluster_id, "_ppi_network.pdf"))) {
          tags$object(type="application/pdf",
                      width = "100%",
                      height = "1000px",
                      alt = "Oops, please click the `plot_STRINGdb` button first and then the picture will be show",
                      data = paste0("PPI_network/", input$ppi_group, "_", input$ppi_cluster_id, "_ppi_network.pdf"))
        }
      }else {
        if (file.exists(paste0("www/PPI_network/", input$ppi_group,"_ppi_network.pdf"))) {
          tags$object(type="application/pdf",
                      width = "100%",
                      height = "1000px",
                      alt = "Oops, please click the `plot_STRINGdb` button first and then the picture will be show",
                      data = paste0("PPI_network/", input$ppi_group,"_ppi_network.pdf"))
        }
      }
    })

    # output$PPI_iframe <- renderUI({
    #   if(is.null(PPI_hits()))
    #     return(NULL)
    #   if (is.list(PPI_hits())) {
    #     tags$object(type="application/pdf",
    #                 width = "100%",
    #                 height = "1000px",
    #                 alt = "Oops, please click the `plot_STRINGdb` button first and then the picture will be show",
    #                 data = paste0("PPI_network/", input$ppi_group, "_", input$ppi_cluster_id, "_ppi_network.pdf"))
    #   }else {
    #     tags$object(type="application/pdf",
    #                 width = "100%",
    #                 height = "1000px",
    #                 alt = "Oops, please click the `plot_STRINGdb` button first and then the picture will be show",
    #                 data = paste0("PPI_network/", input$ppi_group,"_ppi_network.pdf"))
    #   }
    # })
  })
})

# output$PPI_iframe <- renderUI({
#   if(is.null(PPI_hits()))
#     return(NULL)
#   if (is.list(PPI_hits())) {
#     if (file.exists(paste0("PPI_network/", input$ppi_group, "_", input$ppi_cluster_id, "_ppi_network.pdf"))) {
#       tags$object(type="application/pdf",
#                   width = "100%",
#                   height = "1000px",
#                   alt = "Oops, please click the `plot_STRINGdb` button first and then the picture will be show",
#                   data = paste0("PPI_network/", input$ppi_group, "_", input$ppi_cluster_id, "_ppi_network.pdf"))
#     }
#   }else {
#     if (file.exists(paste0("PPI_network/", input$ppi_group,"_ppi_network.pdf"))) {
#       tags$object(type="application/pdf",
#                   width = "100%",
#                   height = "1000px",
#                   alt = "Oops, please click the `plot_STRINGdb` button first and then the picture will be show",
#                   data = paste0("PPI_network/", input$ppi_group,"_ppi_network.pdf"))
#     }
#   }
# })

# output$ppi_Pdf <- downloadHandler(
#   filename = function()  {paste0("Protein_to_protein_network",".pdf")},
#   content = function(file) {
#     file.copy("www/PPI_network.pdf", file)
#   }
# )


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

# output$PPI_Plot <- renderPlot({
#   withProgress(message = "",{
#     string_db <- string_db()
#     hits <- PPI_hits()
#     incProgress(0.6, detail = "plotting, please wait a while...")
#     string_db$plot_network( hits )
#   })
#   # PPI_plot()
# })
#
# output$PPI_PlotUI <- renderUI({
#   withSpinner(plotOutput("PPI_Plot", height = paste0(input$ppi_plot_height, "px"), width = paste0(input$ppi_plot_width, "%")))
# })

# output$ppi_Pdf <- downloadHandler(
#   filename = function()  {paste0("Protein_to_protein_network_plot",".pdf")},
#   content = function(file) {
#     withProgress(message = "",{
#       pdf(file, width = input$ppi_width, height = input$ppi_height)
#       string_db <- string_db()
#       hits <- PPI_hits()
#       incProgress(0.6, detail = "plotting, please wait a while...")
#       p <- string_db$plot_network( hits )
#       dev.off()
#     })
#   }
# )
