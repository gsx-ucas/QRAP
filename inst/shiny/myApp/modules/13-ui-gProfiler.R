fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "Runing gProfiler:", id = "gprofiler_tab", width = 12, collapsible = TRUE,
    fluidRow(
      column(3, selectInput("gprofiler_genes", "What genes to used:",  c("DEGs", "DEG Patterns", "WGCNA Modules"),width = "100%")),
      column(3, uiOutput('gprofiler_gsets')),
      column(3, numericInput("gprofiler_pval", "Pvalue threshold:", value = 0.05,  min = 0, max = 1, width = "100%")),
      column(3, selectInput("gprofiler_cor_method", "Correction method:",
                            c("g_SCS", "bonferroni", "fdr", "false_discovery_rate", "gSCS","analytical"), width = "100%")),
      column(3, selectInput("gprofiler_evcodes", "Show evidence:", c("TRUE", "FALSE"), width = "100%")),
      column(3, selectInput("gprofiler_sources", "Data sources to use:",
                            choices = c("GO:BP", "GO:MF", "GO:CC", "KEGG", "REAC", "TF", "MIRNA", "CORUM", "HP", "HPA", "WP"),
                            selected = c("GO:BP", "GO:MF", "GO:CC", "KEGG", "REAC"), multiple = T, width = "100%")),
      column(3, selectInput("gprofiler_significant", "Only return significant results:", choices = c("TRUE", "FALSE"), width = "100%")),
      column(3, selectInput("gprofiler_excludeIEA", "Exclude GO electronic annotations:", choices = c("FALSE","TRUE"), width = "100%")),
      column( 12, align = "center", actionButton("runGprofiler", "Run gProfiler", class = "run-button", width = "30%") )
    )
  ),
  column(
    12,
    tabsetPanel(
      tabPanel(
        "Enrichment Results Visualization",
        fluidPage(
          style = "padding-top: 5px; padding-left: 0px; padding-right: 0px;margin-left: 0px; margin-right: 0px;",
          box(
            title = "Enrichment Results Visualiztion", width = 4, collapsible = T,
            radioButtons("gprofiler_type", "Plot types:", c("gostplot", "gosttable", "dotplot", "exprs_heatmap"), inline = T, width = "100%"),
            uiOutput("sourceTypes"),
            conditionalPanel(
              "input.gprofiler_type != 'exprs_heatmap'",
              uiOutput("gprofiler_termID")
            ),
            conditionalPanel(
              "input.gprofiler_type=='dotplot'",
              selectInput("gprofiler_orderBy", "Order results by:", c("precision", "recall"), width = "100%"),
              selectInput("gprofiler_Top", "Auto or custom choices terms:", c("use topN terms", "custom select terms"), width = "100%"),
              conditionalPanel(
                "input.gprofiler_Top=='use topN terms'",
                numericInput("gprofiler_n_terms", "TopN terms:", value = 15, width = "100%")
              ),
              # conditionalPanel(
              #   "input.gprofiler_Top=='custom select terms'",
              #   uiOutput("gprofiler_termID")
              # ),
              numericInput("gprofiler_fontsize", "Fontsize of Plots:", value = 15, width = "100%")
            ),
            conditionalPanel(
              "input.gprofiler_type=='gostplot' | input.gprofiler_type=='gosttable'",
              numericInput("gprofiler_tbfontsize", "Fontsize:", value = 10,  min = 0, width = "100%"),
              selectInput("gprofiler_showLink", "Show Link in bottom:", choices = c("FALSE", "TRUE"), width = "100%"),
              selectInput("gprofiler_showColumns", "Columns will be showed:", width = "100%", multiple = T,
                          choices = c("source", "term_name", "term_size", "intersection_size"),
                          selected = c("source", "term_name", "term_size", "intersection_size"))
            ),
            conditionalPanel(
              "input.gprofiler_type == 'exprs_heatmap'",
              # uiOutput("sourceTypes2"),
              uiOutput("gprofiler_termID2"),
              uiOutput("gprofiler_exprs_group"),
              selectInput("gprofiler_data_use", "Values Used To Visualize:", width = "100%",
                          c("rlog or vst transformed value" = "trans_value", "log2(normalized_counts + 1)" = "rel_value",
                            "DESeq2 normalized counts" = "norm_value")),
              numericRangeInput("gprofiler_cluster_break", "Color bar value:", value = c(-2,2), width = "100%"),
              numericInput("gprofiler_heatmap_fontsize", "Fontsize:", value = 15, width = "100%"),
              awesomeCheckbox("gprofiler_cluster_row", "Cluster genes ?", value = TRUE, status = "info", width = "100%")
            ),
            disabled(actionButton("Plot_gprofiler", "Plot gProfiler", class = "plot-button", width = "100%"))
          ),
          column(
            6,
            wellPanel(
              dropdownButton(
                numericInput('gprofiler_width', 'Figure Width:', min = 1, max = 20, value = 12, width = "100%"),
                numericInput('gprofiler_height', 'Figure Height:', min = 1, max = 20, value = 6, width = "100%"),
                downloadButton('gprofiler_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
                circle = FALSE, status = "danger", size = "sm",
                icon = icon("save"), width = "200px",
                tooltip = tooltipOptions(title = "Click to download figures !")
              ),
              uiOutput("gprofilerPlotUI")
              # withSpinner(plotOutput("gprofilerPlot", height = "550px", width = "100%"))
            )
          ),
          column(
            2,
            wellPanel(
              sliderInput("gprofiler_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
              sliderInput("gprofiler_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 400, step = 20, width = "100%")
            )
          )
        )
      ),
      tabPanel(
        "Enrichment Results Table",
        fluidPage(
          style = "padding-top: 5px; padding-left: 0px; padding-right: 0px;margin-left: 0px; margin-right: 0px;",
          uiOutput("sourceID"),
          withSpinner(dataTableOutput("gProfiler_Tab")),
          downloadButton('gprofiler_csv','Download .csv', class = "btn", width = "20%")
        )
      )
    )
  ),
  column(
    12,
    hr(),
    fluidRow(column(2), column(3, actionLink("pGprofiler", "<< Previous", style = "font-size: 20px")),
             column(4, p("You are in gProfiler page ...", style = "color: grey; font-size: 20px")),
             column(3, actionLink("nGprofiler", "Next >>", style = "font-size: 20px")))
  )
)
