observe({
  if (input$nGSEA | input$pPPI) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "pathview")
  }
})

output$pathview_group <- renderUI({
  selectInput(
    inputId = "pathview_degs", label = "DEGs:",
    choices = dir("DEGs") %>% stringr::str_remove_all(".csv"),
    selected = stringr::str_remove_all(dir("DEGs"), ".csv")[1],
    width = "100%", multiple = F
  )
})

observeEvent(input$get_DEGs,{
  updateSelectInput(
    session = session, inputId = "pathview_degs",
    choices = stringr::str_remove_all(dir("DEGs"), ".csv")[1]
  )
})

pathview_geneList <- eventReactive(input$show_pathview, {
  DeGenes <- load.DEGs(input$pathview_degs)
  degs <- DeGenes[[1]] %>% tibble::rownames_to_column(var = "input")
  if (species()$ENTREZGENE_ACC[species()$display_name == input$gprofiler_species] == 'yes') {
    convert_df <- gprofiler2::gconvert(degs$input, organism = species()$id[species()$display_name == input$gprofiler_species], target = "ENTREZGENE_ACC")
  }else if (species()$ENTREZGENE[species()$display_name == input$gprofiler_species] == 'yes') {
    convert_df <- gprofiler2::gconvert(degs$input, organism = species()$id[species()$display_name == input$gprofiler_species], target = "ENTREZGENE")
  }
  joined_df <- left_join(degs, convert_df, by = "input") %>% filter(target != "nan")
  genes <- joined_df$log2FoldChange
  names(genes) <- joined_df$target
  return(genes)
})


output$pathview_id <- renderUI({
  if (input$pathview_inherit == "gProfiler2") {
    if (!input$runGprofiler) {
      shinyjs::disable("show_pathview")
      shinyalert(title = "warning", text = "Please run enrich KEGG using gProfiler2 first !!!", type = "warning")
      return(NULL)
    }else {
      shinyjs::enable("show_pathview")
      id <- (gprofiler_object()$result %>% filter(source == "KEGG") %>% select(term_id))[,1] %>% str_remove_all(pattern = "KEGG:")
      names(id) <- (gprofiler_object()$result %>% filter(source == "KEGG") %>% select(term_name))[, 1]
      selectInput("pathview_id", "Pathway to show:", choices = id, selected = id[1], multiple = F, width = "100%")
    }
  }else if (input$pathview_inherit == "clusterProfiler"){
    if (!input$start_clp_ora | input$clp_ora_source != 'KEGG') {
      shinyjs::disable("show_pathview")
      shinyalert(title = "warning", text = "Please run enrich KEGG using clusterProfiler first !!!", type = "warning")
      return(NULL)
    }else {
      shinyjs::enable("show_pathview")
      id <- as.data.frame(clp_ora_object())$ID %>% str_remove_all(pattern = species()$kegg_code[species()$display_name == input$gprofiler_species])
      names(id) <- as.data.frame(clp_ora_object())$Description
      selectInput("pathview_id", "Pathway to show:", choices = id, selected = id[1], multiple = F, width = "100%")
    }
  }
})

observeEvent(input$show_pathview, {
  withProgress(message = "", value = 0,{
    if (!dir.exists("www/Kegg_dir/")) {
      dir.create("www/Kegg_dir", recursive = T)
    }
    kegg_id <- species()$kegg_code[species()$display_name == input$gprofiler_species]
    incProgress(0.5, detail = paste("Generating pathway png figure ...."))
    pathview(gene.data = pathview_geneList(), pathway.id = input$pathview_id,
             species = kegg_id, out.suffix = "pathview", kegg.native = TRUE, res = 500, new.signature = FALSE,
             pdf.size = c(input$pathview_pdfsize[1], input$pathview_pdfsize[2]), cex = input$pathview_cex,
             key.pos = input$key_pos, limit = list(gene = input$colorbar_limit[1]),
             bins = list(gene = input$colorbar_limit[2]))
    file.copy(paste0(kegg_id, input$pathview_id, ".pathview.png"),
              paste0("www/Kegg_dir/", kegg_id, input$pathview_id, ".pathview.png"), overwrite = TRUE)

    incProgress(0.5, detail = paste("Generating pathway pdf figure ...."))
    pathview(gene.data = pathview_geneList(), pathway.id = input$pathview_id,
             species = kegg_id, out.suffix = "pathview", kegg.native = FALSE, new.signature = FALSE,
             pdf.size = c(input$pathview_pdfsize[1], input$pathview_pdfsize[2]), cex = input$pathview_cex,
             key.pos = input$key_pos, limit = list(gene = input$colorbar_limit[1]),
             bins = list(gene = input$colorbar_limit[2]))
    file.copy(paste0(kegg_id, input$pathview_id, ".pathview.pdf"),
              paste0("www/Kegg_dir/", kegg_id, input$pathview_id, ".pathview.pdf"), overwrite = TRUE)
    lapply(dir("./", pattern = paste0(kegg_id, input$pathview_id, "*")), function(x){file.remove(x)})

    output$pathview_iframe <- renderUI({
      if (file.exists(paste0("www/Kegg_dir/", kegg_id, input$pathview_id, ".pathview.png"))) {
        tags$image(style = paste0("width:", input$pathview_plot_width, "%;",
                                  "height:", input$pathview_plot_height, "px;scrolling=no"),
                   alt = "Oops, please click the `show pathway graph` button first and then the picture will be show",
                   src = paste0("Kegg_dir/", kegg_id, input$pathview_id, ".pathview.png"))
      }
    })
  })
})


output$pathview_Pdf <- downloadHandler(
  filename = function()  {paste0(species()$kegg_code[species()$display_name == input$gprofiler_species], input$pathview_id,".pdf")},
  content = function(file) {
    file.copy(paste0("www/Kegg_dir/",species()$kegg_code[species()$display_name == input$gprofiler_species], input$pathview_id, ".pathview.pdf"), file)
  }
)

output$pathview_Png <- downloadHandler(
  filename = function()  {paste0(species()$kegg_code[species()$display_name == input$gprofiler_species], input$pathview_id,".png")},
  content = function(file) {
    file.copy(paste0("www/Kegg_dir/",species()$kegg_code[species()$display_name == input$gprofiler_species], input$pathview_id, ".pathview.png"), file)
  }
)
