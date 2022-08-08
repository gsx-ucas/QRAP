fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "Runing Over-Represented Analysis (ORA) by clusterProfiler:", id = "clp_ora_tab", width = 12, collapsible = TRUE, solidHeader = TRUE,
    fluidRow(
      column(3,selectInput("clp_ora_source", "Function Source:",
                           c("Gene Ontology"="GO", "KEGG Pathway"="KEGG", "Reactome Pathway"="Reactome"),width = "100%")),
      column(3,selectInput("clp_ora_genes", "What genes to used:", width = "100%", c("DEGs", "DEG Patterns", "WGCNA Modules"))),
      column(3, uiOutput('clp_ora_gsets')),
      conditionalPanel(
        "input.clp_ora_source=='GO'",
        column(3, selectInput("GO_ont", "Sub Ontologies:", c("BP", "CC", "MF"),width = "100%"))
      ),
      conditionalPanel(
        "input.clp_ora_source=='Reactome'",
        column(3, selectInput("ora_reactome_organism", "Select organism:", c("human", "mouse", "rat", "celegans", "fly", "yeast", "zebrafish"),width = "100%"))
      ),
      column(3, selectInput("clp_ora_pAdjustMethod", "pAdjustMethod:", width = "100%", c("BH", "holm", "hochberg", "hommel", "bonferroni", "BY", "fdr", "none"))),
      column(3, numericInput("clp_ora_pval", "Pvalue threshold:", value = 0.05,  min = 0, max = 1, width = "100%")),
      column(3, numericInput("clp_ora_qval", "Qvalue threshold:", value = 0.2,  min = 0, max = 1, width = "100%")),
      column(3,numericInput("clp_ora_minGSSize", "minGSSize:", value = 10, width = "100%")),
      # column(3,numericInput("clp_ora_maxGSSize", "maxGSSize:", value = 500, width = "100%")),
      column(12, align = 'center', actionButton("start_clp_ora", "Start ClusterProfiler",
                                                style = "height:40px;font-size:18px", class = "run-button", width = "300px"))
    )
  ),
  column(
    12,
    tabsetPanel(
      tabPanel(
        "Enrichment Results Visualization",
        # fluidPage(
        style = "padding-top: 5px; padding-left: 0px; padding-right: 0px;margin-left: 0px; margin-right: 0px;",
        box(
          title = "Visualization of Results", width = 4, solidHeader = TRUE,
          uiOutput("oraPlot_type"),
          conditionalPanel(
            "input.oraPlot_type != 'exprs_heatmap'",
            selectInput("ora_orderBy", "Order results by:", c("GeneRatio", "Count"), width = "100%")
          ),
          conditionalPanel(
            "input.oraPlot_type == 'dotplot' | input.oraPlot_type == 'barplot' | input.oraPlot_type == 'ggtable'",
            selectInput("useTop", "Auto or custom choices terms:", c("use topN terms", "custom select terms"), width = "100%")
          ),
          conditionalPanel(
            "(input.useTop == 'use topN terms' | input.oraPlot_type == 'cnetplot' | input.oraPlot_type == 'emapplot') & input.oraPlot_type != 'exprs_heatmap'",
            numericInput("n_terms", "TopN terms:", value = 15, width = "100%")
          ),
          conditionalPanel(
            "input.useTop == 'custom select terms' & (input.oraPlot_type == 'dotplot' | input.oraPlot_type == 'barplot' | input.oraPlot_type == 'ggtable')",
            uiOutput("ora_termID")
          ),
          conditionalPanel(
            "input.oraPlot_type != 'cnetplot' & input.oraPlot_type != 'exprs_heatmap'",
            selectInput("ora_colorBy", "Color plots by:", c("p.adjust", "pvalue", "qvalue"), width = "100%")
          ),
          conditionalPanel(
            "input.oraPlot_type == 'cnetplot'",
            selectInput("ora_circular", "whether using circular layout:", c("FALSE", "TRUE"), width = "100%")
          ),
          conditionalPanel(
            "input.oraPlot_type == 'dotplot' | input.oraPlot_type == 'barplot' | input.oraPlot_type == 'ggtable'",
            numericInput("ora_fontsize", "Fontsize of Plots:", value = 15, width = "100%")
          ),
          conditionalPanel(
            "input.oraPlot_type == 'exprs_heatmap'",
            uiOutput("ora_termID2"), uiOutput("ora_exprs_group"),
            selectInput("ora_data_use", "Values Used To Visualize:", width = "100%",
                        c("rlog or vst transformed value" = "trans_value", "log2(normalized_counts + 1)" = "rel_value",
                          "DESeq2 normalized counts" = "norm_value")),
            numericRangeInput("ora_cluster_break", "Color bar value:", value = c(-2,2), width = "100%"),
            numericInput("ora_heatmap_fontsize", "Fontsize:", value = 15, width = "100%"),
            awesomeCheckbox(inputId = "ora_cluster_row", label = "Cluster genes:",
                            value = TRUE, status = "info", width = "100%")
            # actionButton("plotORA_heatmap", "Plot ORA results", width = "100%", style = "background-color:orange")
          ),
          disabled(
            actionButton("plotORA", "Plot ORA results", width = "100%", class = "plot-button")
          )
        ),
        column(
          6,
          wellPanel(
            style = "padding-top:5px",
            fluidRow(
              column(
                12, style = "padding-left:0px;margin-left:0px;padding-right:0px;margin-right:0px;border-bottom:solid 1px rgb(224,224,224)",
                column(
                  6, style = "padding-left:10px;",
                  tags$h4("Enrichment Results Visualization:")
                ),
                column(
                  6, align = "right", style = "padding-top:5px;",
                  dropdownButton(
                    numericInput('ora_width', 'Figure Width:', value = 10, width = "100%"),
                    numericInput('ora_height', 'Figure Height:', value = 10, width = "100%"),
                    downloadButton('ora_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
                    circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px",
                    right = TRUE, tooltip = tooltipOptions(title = "Click to download figures !")
                  )
                )
              )
            ),
            uiOutput("oraPlotsUI")
          )
        ),
        column(
          2,
          wellPanel(
            sliderInput("ora_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
            sliderInput("ora_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 400, step = 20, width = "100%")
          )
        )
        # )
      ),
      tabPanel(
        "Enrichment Results Table",
        fluidPage(
          style = "padding-top: 5px; padding-left: 0px; padding-right: 0px;margin-left: 0px; margin-right: 0px;",
          withSpinner(dataTableOutput("ora_Tab")),
          downloadButton('ora_Tab_Csv','Download .csv', class = "btn", width = "100%")
        )
      )
    )
  ),
  column(
    12,
    hr(),
    fluidRow(column(3, align = "right", actionLink("pORA", "<< Previous", style = "font-size: 20px")),
             column(6, align = "center", HTML('<p style = "text-align:center;">Copyright &copy; 2022.Shixue All rights reserved.</p>')),
             column(3, align = "left", actionLink("nORA", "Next >>", style = "font-size: 20px")))
  )
)
