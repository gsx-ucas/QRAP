observe({
  if (input$nsgene) {
    updateTabsetPanel(session = session, inputId = 'mainMenu', selected = "sfunc")
  }
})

output$intfs_funcs <- renderUI({
  if (input$gprofiler_genes == "DEGs" & input$clp_ora_genes == "DEGs") {
    checkboxGroupInput("intfs_funcs", "Gene Sets to explore:", inline = T, width = "100%",
                       choices = c("gProfiler", "clusterProfiler-ORA", "clusterProfiler-GSEA"),
                       selected = c("gProfiler", "clusterProfiler-ORA", "clusterProfiler-GSEA"))
  }else {
    checkboxGroupInput("intfs_funcs", "Gene Sets to explore:", inline = T, width = "100%",
                       choices = c("gProfiler", "clusterProfiler-ORA"),
                       selected = c("gProfiler", "clusterProfiler-ORA"))
  }
})

int_funcList <- eventReactive(input$get_int_function, {
  withProgress(message = "", value = 0, {
    incProgress(0.2, detail = "Loading ora enrich results ...")

    if (input$gprofiler_genes == "DEGs" & input$clp_ora_genes == "DEGs") {
      if ("clusterProfiler-GSEA" %in% input$intfs_funcs) {
        if (input$clp_ora_degs %>% length > 1 | input$gprofiler_degs %>% length > 1) {
          sendSweetAlert(title = "warning", text = "Only support single sample input.", type = "warning", btn_labels = "Close")
          return(NULL)
        }else {
          if (input$clp_ora_degs != input$gprofiler_degs) {
            sendSweetAlert(title = "warning", text = "Only support same geneset input.", type = "warning", btn_labels = "Close")
            return(NULL)
          }else {
            if (stringr::str_detect(input$intfs_sources, pattern = "GO")) {
              intfs_sources <- input$intfs_sources
              clp_ora_source <- paste0(input$clp_ora_source, ":", input$GO_ont)
              clp_gsea_source <- paste0(input$clp_gsea_source, ":", input$gsea_GO_ont)
            }else if (input$intfs_sources == "REAC") {
              intfs_sources <- "Reactome"
              clp_ora_source <- input$clp_ora_source
              clp_gsea_source <- input$clp_gsea_source
            }else {
              intfs_sources <- input$intfs_sources
              clp_ora_source <- input$clp_ora_source
              clp_gsea_source <- input$clp_gsea_source
            }
            
            if (intfs_sources == clp_ora_source & intfs_sources == clp_gsea_source & intfs_sources %in% input$gprofiler_sources) {
              if (!input$start_clp_ora | !"clusterProfiler-ORA" %in% input$intfs_funcs) {
                ora_functions <- NULL
              }else{
                ora_functions <- as.data.frame(clp_ora_object())$Description
              }

              if (!input$start_clp_gsea | !"clusterProfiler-GSEA" %in% input$intfs_funcs) {
                gsea_functions <- NULL
              }else {
                gsea_functions <- as.data.frame(clp_gsea_object())$Description
              }

              if (!input$runGprofiler | !"gProfiler" %in% input$intfs_funcs) {
                gprofiler_functions <- NULL
              }else {
                ids <- gprofiler_object()$result$source %>% grep(pattern = input$intfs_sources)
                gprofiler_functions <- gprofiler_object()$result$term_name[ids]
              }

              FunctionList <- list(ora_functions = ora_functions, gsea_functions = gsea_functions, gprofiler_functions = gprofiler_functions)
              return(FunctionList)
            }else {
              sendSweetAlert(title = "warning", text = "Only support the same sources.", type = "warning", btn_labels = "Close")
              return(NULL)
            }
          }
        }
      }else {
        if (!all(input$clp_ora_degs %in% input$gprofiler_degs)) {
          sendSweetAlert(title = "warning", text = "Only support the same geneset input.", type = "warning", btn_labels = "Close")
          return(NULL)
        }else {
          if (stringr::str_detect(input$intfs_sources, pattern = "GO")) {
            intfs_sources <- input$intfs_sources
            clp_ora_source <- paste0(input$clp_ora_source, ":", input$GO_ont)
          }else if (input$intfs_sources == "REAC") {
            intfs_sources <- "Reactome"
            clp_ora_source <- input$clp_ora_source
          }else {
            intfs_sources <- input$intfs_sources
            clp_ora_source <- input$clp_ora_source
          }
          
          if (intfs_sources == clp_ora_source & intfs_sources %in% input$gprofiler_sources) {
            if (!input$start_clp_ora | !"clusterProfiler-ORA" %in% input$intfs_funcs) {
              ora_functions <- NULL
            }else{
              if ("Cluster" %in% colnames(as.data.frame(clp_ora_object()))) {
                ora_functions <- paste0("Cluster:", as.data.frame(clp_ora_object())$Cluster, "~", as.data.frame(clp_ora_object())$Description)
              }else {
                ora_functions <- as.data.frame(clp_ora_object())$Description
              }
            }
            
            if (!input$runGprofiler | !"gProfiler" %in% input$intfs_funcs) {
              gprofiler_functions <- NULL
            }else {
              ids <- gprofiler_object()$result$source %>% grep(pattern = input$intfs_sources)
              if (length(unique(gprofiler_object()$result$query)) > 1) {
                gprofiler_functions <- paste0("Cluster:", gprofiler_object()$result$query[ids], "~", gprofiler_object()$result$term_name[ids])
              }else {
                gprofiler_functions <- gprofiler_object()$result$term_name[ids]
              }
            }
            
            FunctionList <- list(ora_functions = ora_functions, gprofiler_functions = gprofiler_functions)
            return(FunctionList)
          }else {
            sendSweetAlert(title = "warning", text = "Only support the same sources.", type = "warning", btn_labels = "Close")
            return(NULL)
          }
        }
      }
    }else {
      if (input$gprofiler_genes != input$clp_ora_genes) {
        sendSweetAlert(title = "warning", text = "Only support the same geneset.", type = "warning", btn_labels = "Close")
        return(NULL)
      }else {
        if (input$gprofiler_genes == "WGCNA Modules" & input$clp_ora_genes == "WGCNA Modules") {
          clp_ora_gsets <- input$clp_ora_modules
          gprofiler_gsets <- input$gprofiler_modules
        }else if (input$gprofiler_genes == "DEG Patterns" & input$clp_ora_genes == "DEG Patterns") {
          clp_ora_gsets <- input$clp_ora_patterns
          gprofiler_gsets <- input$gprofiler_patterns
        }
        
        if (!all(clp_ora_gsets %in% gprofiler_gsets)) {
          sendSweetAlert(title = "warning", text = "Only support the same geneset.", type = "warning", btn_labels = "Close")
          return(NULL)
        }else{
          if (stringr::str_detect(input$intfs_sources, pattern = "GO")) {
            intfs_sources <- input$intfs_sources
            clp_ora_source <- paste0(input$clp_ora_source, ":", input$GO_ont)
          }else if (input$intfs_sources == "REAC") {
            intfs_sources <- "Reactome"
            clp_ora_source <- input$clp_ora_source
          }else {
            intfs_sources <- input$intfs_sources
            clp_ora_source <- input$clp_ora_source
          }
          
          if (intfs_sources == clp_ora_source & intfs_sources %in% input$gprofiler_sources) {
            if (!input$start_clp_ora | !"clusterProfiler-ORA" %in% input$intfs_funcs) {
              ora_functions <- NULL
            }else{
              if ("Cluster" %in% colnames(as.data.frame(clp_ora_object()))) {
                ora_functions <- paste0("Cluster:", as.data.frame(clp_ora_object())$Cluster, "~", as.data.frame(clp_ora_object())$Description)
              }else {
                ora_functions <- as.data.frame(clp_ora_object())$Description
              }
            }
            
            if (!input$runGprofiler | !"gProfiler" %in% input$intfs_funcs) {
              gprofiler_functions <- NULL
            }else {
              ids <- gprofiler_object()$result$source %>% grep(pattern = input$intfs_sources)
              if (length(unique(gprofiler_object()$result$query)) > 1) {
                gprofiler_functions <- paste0("Cluster:", gprofiler_object()$result$query[ids], "~", gprofiler_object()$result$term_name[ids])
              }else {
                gprofiler_functions <- gprofiler_object()$result$term_name[ids]
              }
            }
            
            FunctionList <- list(ora_functions = ora_functions, gprofiler_functions = gprofiler_functions)
            return(FunctionList)
          }else {
            sendSweetAlert(title = "warning", text = "Only support the same sources.", type = "warning", btn_labels = "Close")
            return(NULL)
          }
        }
      }
    }
  })
})

intf_venn <- eventReactive(input$get_int_function,{
  req(int_funcList())
  venn::venn(int_funcList(), zcolor = 'style', ilcs = input$intf_venn_lsize, sncs = input$intf_venn_nsize, box = F)
})

output$intf_venn <- renderPlot({
  intf_venn()
})

output$intf_venn_plotUI <-  renderUI({
  withSpinner(plotOutput("intf_venn", width = paste0(input$intf_venn_plot_width, "%"), height = paste0(input$intf_venn_plot_height, "px")))
})

output$intf_venn_Pdf <- downloadHandler(
  filename = function()  {"Intersected_function_vennPlot.pdf"},
  content = function(file) {
    p <- intf_venn()
    ggsave(file, p, width = input$intf_venn_width, height = input$intf_venn_height)
  }
)

intf_tab <- eventReactive(input$get_int_function,{
  if (all(c("gProfiler", "clusterProfiler-ORA", "clusterProfiler-GSEA") %in% input$intfs_funcs)) {
    terms <- intersect(int_funcList()$ora_functions, int_funcList()$gsea_functions) %>%
      intersect(int_funcList()$gprofiler_functions)
    clusters <- NULL
  }else if (!"gProfiler" %in% input$intfs_funcs) {
    terms <- intersect(int_funcList()$ora_functions, int_funcList()$gsea_functions)
  }else if (!"clusterProfiler-ORA" %in% input$intfs_funcs) {
    terms <- intersect(int_funcList()$gprofiler_functions, int_funcList()$gsea_functions)
  }else if (!"clusterProfiler-GSEA" %in% input$intfs_funcs) {
    terms <- intersect(int_funcList()$ora_functions, int_funcList()$gprofiler_functions)
  }
  
  enrich_df <- gprofiler_object()$result[gprofiler_object()$result$source == input$intfs_sources, ]
  if (length(unique(enrich_df$query)) > 1) {
    enrich_df$idx <- paste0("Cluster:", enrich_df$query, "~", enrich_df$term_name)
    if (all(stringr::str_detect(terms, pattern = "^Cluster"))) {
      enrich_df <- enrich_df[enrich_df$idx %in% terms, c("query", "term_id", "term_name", "p_value", "intersection_size", "precision", "intersection")]
    }else {
      enrich_df <- enrich_df[enrich_df$term_name %in% terms, c("query", "term_id", "term_name", "p_value", "intersection_size", "precision", "intersection")]
    }
  }else {
    enrich_df <- enrich_df[enrich_df$term_name %in% terms, c("query", "term_id", "term_name", "p_value", "intersection_size", "precision", "intersection")]
  }
})

output$intfs_dfs <- renderDataTable({
  intf_tab()
},rownames = T, options = list(pageLength = 5, autoWidth = T, scrollX=TRUE,  scrollY="280px",
                               columnDefs = list(list(width = '300px', targets = 1:6))))

output$intfs_dfs_Csv <- downloadHandler(
  filename = function()  {"Intersected_functions_table.csv"},
  content = function(file) {
    write.csv(intf_tab(), file, row.names = T)
  }
)

intf_plot <- reactive({
  tab <- intf_tab()
  if (length(unique(tab$query)) > 1) {
    top_terms <- tab[tab$p_value %>% order(decreasing = F), "term_name"] %>% unique %>% head(input$intf_top_terms)
    tab <- tab[tab$term_name %in% top_terms, ]
    tab <- tab[tab$precision %>% order(decreasing = T), ]
    tab$term_name <- factor(tab$term_name, levels = tab$term_name %>% unique %>% rev)
    ggplot(data = tab)+
      geom_point(stat = "identity", aes(x = query, y = term_name, col = p_value, size = precision))+
      labs(x = NULL, y = NULL)+
      scale_color_gradient(low = "red", high = "blue")+
      theme_bw()+
      theme(axis.title = element_text(size = 18), 
            axis.text = element_text(size = input$intf_plot_text_size, color = "black"),
            axis.text.x = element_text(angle = 45, hjust = 1))
  }else {
    tab <- tab[tab$p_value %>% order(decreasing = F), ] %>% head(input$intf_top_terms)
    tab$term_name <- factor(tab$term_name, levels = tab$term_name %>% unique %>% rev)
    ggplot(data = tab)+
      geom_bar(stat = "identity", aes(x = term_name, y = -log10(p_value)), fill = "#87CEFA")+
      ggplot2::coord_flip()+
      labs(x = NULL, y = "-Log10(P-value)")+
      theme_bw()+
      theme(axis.title = element_text(size = 18), axis.text = element_text(size = input$intf_plot_text_size, color = "black"))
  }
})

output$intf_plot <- renderPlot({
  intf_plot()
})

output$intf_plotUI <-  renderUI({
  withSpinner(plotOutput("intf_plot", width = paste0(input$intf_plot_width, "%"), height = paste0(input$intf_plot_height, "px")))
})

output$intf_plot_Pdf <- downloadHandler(
  filename = function()  {"Intersected_function_Plot.pdf"},
  content = function(file) {
    p <- intf_plot()
    ggsave(file, p, width = input$intf_plot_Pdf_width, height = input$intf_plot_Pdf_height)
  }
)
