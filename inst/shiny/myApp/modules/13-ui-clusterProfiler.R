fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "Runing GSEA or ORA:", id = "clpr_tab", width = 12, collapsible = TRUE,
    fluidRow(
      column(3, selectInput("clpr_types", "ORA or GSEA:",
                            c("Over-Represented Analysis (ORA)"="ORA", "Gene Set Enrichment Analysis (GSEA)"="GSEA"), width = "100%")),
      column(3,selectInput("clpr_source", "Function Source:",
                           c("Gene Ontology"="GO", "KEGG Pathway"="KEGG", "Reactome Pathway"="Reactome"),width = "100%")),
      conditionalPanel(
        "input.clpr_types == 'ORA'",
        column(3,selectInput("clpr_genes", "What genes to used:", width = "100%", c("DEGs", "DEG Patterns", "WGCNA Modules")))
      ),
      column(3, uiOutput('clpr_gsets')),
      conditionalPanel(
        "input.clpr_source=='GO'",
        column(3, selectInput("GO_ont", "Sub Ontologies:", c("BP", "CC", "MF"),width = "100%"))
      ),
      conditionalPanel(
        "input.clpr_source=='KEGG'",
        column(3, uiOutput("kegg_organism"))
      ),
      conditionalPanel(
        "input.clpr_source=='Reactome'",
        column(3, selectInput("Reactome_organism", "Select organism:", c("human", "mouse", "rat", "celegans", "fly", "yeast", "zebrafish"),width = "100%"))
      ),
      column(3, selectInput("clpr_pAdjustMethod", "pAdjustMethod:", width = "100%", c("BH", "holm", "hochberg", "hommel", "bonferroni", "BY", "fdr", "none"))),
      conditionalPanel(
        "input.clpr_types == 'GSEA'",
        column(3, selectInput("gsea_method", "Performed By:", choices = c("fgsea", "DOSE"), width = "100%"))
      ),
      column(3, numericInput("clpr_pval", "Pvalue threshold:", value = 0.05,  min = 0, max = 1, width = "100%")),
      column(3,numericInput("clpr_minGSSize", "minGSSize:", value = 10, width = "100%")),
      # column(3,numericInput("clpr_maxGSSize", "maxGSSize:", value = 500, width = "100%")),
      column(12, align = 'center', actionButton("start_clpr", "Start ClusterProfiler",
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
          title = "Visualization of Results", width = 4,
          conditionalPanel(
            "input.clpr_types == 'GSEA'",
            radioButtons("gseaPlot_type", "Methods to visualize:", c("gseaplot2", "ridgeplot", "exprs_heatmap"), inline = T, width = "100%" ),
            conditionalPanel(
              "input.gseaPlot_type == 'gseaplot2'",
              uiOutput("gseaID"),
              selectInput("gsea_ES","Geom for plotting running enrichment score:", choices = c("line", "dot"), width = "100%"),
              checkboxInput("gsea_pTable","whether add pvalue table:", value = TRUE, width = "100%")
            ),
            conditionalPanel(
              "input.gseaPlot_type == 'ridgeplot'",
              numericInput("gsea_nterms", "TopN terms:", value = 10, width = "100%"),
              selectInput("gsea_colorBy", "Color plots by:", c("p.adjust", "pvalue", "qvalue"), width = "100%")
            ),
            conditionalPanel(
              "input.gseaPlot_type == 'exprs_heatmap'",
              uiOutput("expr_gseaID"),
              uiOutput("gsea_exprs_group"),
              selectInput("gsea_data_use", "Values Used To Visualize:", width = "100%",
                          c("rlog or vst transformed value" = "trans_value", "log2(normalized_counts + 1)" = "rel_value",
                            "DESeq2 normalized counts" = "norm_value")),
              textInput("gsea_heatmap_color", "color:", value = "navy,white,red",  width = "100%"),
              numericRangeInput("gsea_cluster_break", "Color bar value:", value = c(-2,2), width = "100%")
            ),
            numericInput("gsea_fontsize", "Fontsize:", value = 10,  min = 0, max = 50, width = "100%"),
            conditionalPanel(
              "input.gseaPlot_type == 'exprs_heatmap'",
              awesomeCheckbox( inputId = "gsea_cluster_row", label = "Cluster genes:", value = TRUE, status = "info", width = "100%" )
            ),
            disabled(
              actionButton("PlotGSEA", "Plot gsea", class = "plot-button", width = "100%")
            )
          ),
          conditionalPanel(
            "input.clpr_types == 'ORA'",
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
            # conditionalPanel(
            #   "input.oraPlot_type != 'exprs_heatmap'",
            #   actionButton("plotORA", "Plot ORA results", width = "100%", style = "background-color:orange")
            # )
          )
        ),
        column(
          6,
          wellPanel(
            conditionalPanel(
              "input.clpr_types == 'GSEA'",
              dropdownButton(
                numericInput('gsea_width', 'Figure Width:', min = 1, max = 20, value = 12, width = "100%"),
                numericInput('gsea_height', 'Figure Height:', min = 1, max = 20, value = 4, width = "100%"),
                downloadButton('gsea_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
                circle = FALSE, status = "danger", size = "sm",
                icon = icon("save"), width = "200px",
                tooltip = tooltipOptions(title = "Click to download figures !")
              ),
              uiOutput("gseaPlotUI")
              # withSpinner(plotOutput("gseaPlot", height = "550px", width = "100%"))
            ),
            conditionalPanel(
              "input.clpr_types == 'ORA'",
              dropdownButton(
                numericInput('ora_width', 'Figure Width:', value = 10, width = "100%"),
                numericInput('ora_height', 'Figure Height:', value = 10, width = "100%"),
                downloadButton('ora_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
                circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px",
                tooltip = tooltipOptions(title = "Click to download figures !")
              ),
              uiOutput("oraPlotsUI")
              # withSpinner(plotOutput("oraPlots", height = "550px"))
            )
          )
        ),
        column(
          2,
          wellPanel(
            sliderInput("clpr_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
            sliderInput("clpr_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 400, step = 20, width = "100%")
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
    fluidRow(column(2), column(3, actionLink("pClpr", "<< Previous", style = "font-size: 20px")),
             column(4, p("You are in clusterProfiler page ...", style = "color: grey; font-size: 20px")),
             column(3, actionLink("nClpr", "Next >>", style = "font-size: 20px")))
  )
)
