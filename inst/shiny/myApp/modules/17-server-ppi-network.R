observe({
  if (input$nPathview | input$pgenie3) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "ppi")
  }
})

output$ppi_group <- renderUI({
  if (input$ppi_genes == "DEGs") {
    if (length(dir("DEGs")) == 0) {
      shinyjs::disable("Init_STRINGdb")
      selectInput(inputId = "ppi_group", label = "Groups Of Differential Expressed Genes:", width = "100%",
                  choices = "*Please Run DEGs First !!!", selected = "*Please Run DEGs Patterns First !!!")
    }else {
      shinyjs::enable("Init_STRINGdb")
      selectInput(
        inputId = "ppi_group", "Groups Of Differential Expressed Genes:", width = "100%",
        choices = dir("DEGs") %>% stringr::str_remove_all(".csv"), selected = stringr::str_remove_all(dir("DEGs"), ".csv")[1])
    }
  }else if (input$ppi_genes == "DEG Patterns") {
    if (input$run_degsp == 0) {
      shinyjs::disable("Init_STRINGdb")
      selectInput(inputId = "ppi_patterns", label = "Select Patterns ID:", width = "100%",
                  choices = "*Please Run DEGs Patterns First !!!", selected = "*Please Run DEGs Patterns First !!!")
      # p("*Please Run DEGs Patterns First!", style = "color: red; padding-top: 30px; padding-bttom: 30px; font-weight: 700px; width: 100%")
    }else {
      shinyjs::enable("Init_STRINGdb")
      selectInput(inputId = "ppi_patterns", label = "Select Patterns ID:", width = "100%",
                  choices = degsp_object()$normalized$cluster %>% unique %>% as.character)
    }
  }else if (input$ppi_genes=="WGCNA Modules") {
    if (input$moldue_detect == 0) {
      shinyjs::disable("Init_STRINGdb")
      selectInput(inputId = "ppi_modules", label = "Select WGCNA Modules ID:", width = "100%",
                  choices = "*Please Run WGCNA First !!!", selected = "*Please Run WGCNA First !!!")
      # p("*Please Run WGCNA First!", style = "color: red; padding-top: 30px; padding-bttom: 30px; font-weight: 700px; width: 100%")
    }else {
      shinyjs::enable("Init_STRINGdb")
      MEs0 = moduleEigengenes(datExpr(), moduleColors())$eigengenes
      MEs = orderMEs(MEs0)
      selectInput(inputId = "ppi_modules", label = "Select WGCNA Modules ID:",
                  choices = substring(names(MEs), first = 3), width = "100%")
    }
  }
})

observeEvent(input$get_DEGs,{
  updateSelectInput(
    session = session, inputId = "ppi_group",
    choices = dir("DEGs") %>% stringr::str_remove_all(".csv")
  )
})

output$ppi_subGene <- renderUI({
  if (input$ppi_genes=="DEGs") {
    if (is.null(input$ppi_group))
      return(NULL)
    identifiers <- load.DEGs(input$ppi_group)[[1]] %>% rownames()
  }else if (input$ppi_genes=="WGCNA Modules") {
    if (is.null(input$ppi_modules))
      return(NULL)
    identifiers <- names(moduleColors())[moduleColors() == input$ppi_modules]
  }else if (input$ppi_genes=="DEG Patterns") {
    if (is.null(input$ppi_patterns))
      return(NULL)
    identifiers <- degsp_object()$df[degsp_object()$df$cluster == as.integer(input$ppi_patterns), "genes"]
  }

  if (length(identifiers) > 400) {
    fluidPage(
      style = "padding-top:0px; padding-left:0px; padding-right:0px; padding-bottom:10px; margin-top:0px; margin-left:0px; margin-right:0px",
      strong(paste("Selected", length(identifiers), "genes, but only first 400
                 highest connective genes will be used to perform PPI analysis."), style = "text-align:justify; color:orange;")
    )
  }
})

output$required_score <- renderUI({
  if (input$ppi_genes=="DEGs") {
    if (is.null(input$ppi_group))
      return(NULL)
    identifiers <- load.DEGs(input$ppi_group)[[1]] %>% rownames()
  }else if (input$ppi_genes=="WGCNA Modules") {
    if (is.null(input$ppi_modules))
      return(NULL)
    identifiers <- names(moduleColors())[moduleColors() == input$ppi_modules]
  }else if (input$ppi_genes=="DEG Patterns") {
    if (is.null(input$ppi_patterns))
      return(NULL)
    identifiers <- degsp_object()$df[degsp_object()$df$cluster == as.integer(input$ppi_patterns), "genes"]
  }
  numericInput("required_score", "Threshold of significance to include an interaction:", value = 400, width = "100%")
})

string_db <- eventReactive(input$Init_STRINGdb, {
  withProgress(message = "", {
    string_species <- species()$taxon_id[species()$display_name == input$gprofiler_species] %>% as.integer
    # if (input$required_score <= 400) {
    #   score <- input$required_score
    # }else {
    #   score <- 400
    # }

    incProgress(0.1, detail = "Initializing, please wait...")
    if (input$ppi_genes=="DEGs") {
      incProgress(0.1, detail = "Loading differential genes ...")
      df <- load.DEGs(input$ppi_group)[[1]]
      identifiers <- df[order(df$padj, decreasing = F), ] %>% rownames()
      if (length(identifiers) > 400) {
        identifiers <- identifiers[1:400]
      }
    }else if (input$ppi_genes=="WGCNA Modules") {
      incProgress(0.1, detail = "Loading WGCNA module genes ...")
      identifiers <- names(moduleColors())[moduleColors() == input$ppi_modules]
      if (length(identifiers) > 400) {
        IMConn = softConnectivity(datExpr()[, identifiers])
        identifiers <- identifiers[rank(-IMConn) <= 400]
      }
    }else if (input$ppi_genes=="DEG Patterns") {
      incProgress(0.1, detail = "Loading expression pattern genes ...")
      identifiers <- degsp_object()$df[degsp_object()$df$cluster == as.integer(input$ppi_patterns), "genes"]
      if (length(identifiers) > 400) {
        IMConn = softConnectivity(t(assay(trans_value()))[ ,rownames(trans_value()) %in% identifiers])
        identifiers <- identifiers[rank(-IMConn) <= 400]
      }
    }

    incProgress(0.1, detail = "preparing identifiers for analysis ...")

    ppi_identifiers <- paste(identifiers, collapse = "%0d")

    incProgress(0.1, detail = "getting image results ...")

    image_url <- paste0("https://string-db.org/api/svg/network?identifiers=", ppi_identifiers,
                        "&species=", string_species, "&add_color_nodes=0&add_white_nodes=0&required_score=", input$required_score,
                        "&network_type=", input$network_type, "&hide_disconnected_nodes=", input$hide_disconnected_nodes,
                        "&show_query_node_labels=", input$show_query_node_labels, "&block_structure_pics_in_bubbles=", input$block_structure_pics_in_bubbles)

    string_url <- try(paste0("https://string-db.org/api/tsv-no-header/get_link?identifiers=",
                             ppi_identifiers, "&species=", string_species) %>% url() %>% read.table(colClasses = "character"))

    if ('try-error' %in% class(string_url)) {
      shinyalert(title = "error", text = "Failed to connect to stringdb web, Please try again !", type = "error", confirmButtonText = "Close")
    }

    string_list <- list(image_url = image_url, string_url = string_url)
  })
  return(string_list)
})

output$PPI_Image <- renderUI({
  wellPanel(
    fluidRow(
      column(
        12,
        tags$image(type="text/html",
                   width = paste0(input$ppi_plot_width, "%"),
                   height = paste0(input$ppi_plot_height, "px"),
                   alt = "Oops, something wrong!",
                   src = string_db()$image_url),
      )
    )
  )
})

output$PPI_weblink <- renderUI({
  fluidRow(
    p(tags$strong("Note:"), "Just a images of network may be not enough informative, if you want to get more detail about this protein network,
        here we provide you an prepared weblink to the stringdb website, just click",
      actionLink('ppi_weblink', label = tags$a('this link',
                                               href = string_db()$string_url, target = "_blank"), icon = icon("link"), width = "100%"),
      "to find more information ...", style = "text-align:justify; padding:5px"),
    br(),
    p(tags$strong("Download:"), "Dragging the image to where you want to store to download .svg format picture,
        or you can download a high-resolution .png format picture using the download button bellow.", style = "text-align:justify; padding:5px"),
    downloadButton('ppi_png','Download .png', class = "btn btn-warning", width = "100%")
  )
})

output$ppi_png <- downloadHandler(
  filename = function()  {paste0("Protein_to_protein_network",".png")},
  content = function(file) {
    download_url <- stringr::str_replace(string_db()$image_url, "api/svg", "api/highres_image")
    download.file(download_url, "ppi_network.png", quiet = T)
    file.copy("ppi_network.png", file)
    file.remove("ppi_network.png")
  }
)
