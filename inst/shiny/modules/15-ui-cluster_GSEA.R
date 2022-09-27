fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "Runing GSEA or ORA:", id = "clp_gsea_tab", width = 12, collapsible = TRUE, solidHeader = TRUE,
    fluidRow(
      column(3,selectInput("clp_gsea_source", "Function Source:",
                           c("Gene Ontology"="GO", "KEGG Pathway"="KEGG", "Reactome Pathway"="Reactome"),width = "100%")),
      column(3, uiOutput('clp_gsea_gsets')),
      conditionalPanel(
        "input.clp_gsea_source=='GO'",
        column(3, selectInput("gsea_GO_ont", "Sub Ontologies:", c("BP", "CC", "MF"),width = "100%"))
      ),
      conditionalPanel(
        "input.clp_gsea_source=='Reactome'",
        column(3, selectInput("gsea_reactome_organism", "Select organism:", c("human", "mouse", "rat", "celegans", "fly", "yeast", "zebrafish"),width = "100%"))
      ),
      column(3, selectInput("clp_gsea_pAdjustMethod", "pAdjustMethod:", width = "100%", c("BH", "holm", "hochberg", "hommel", "bonferroni", "BY", "fdr", "none"))),
      column(3, selectInput("gsea_method", "Performed By:", choices = c("fgsea", "DOSE"), width = "100%")),
      column(3, numericInput("clp_gsea_pval", "Pvalue threshold:", value = 0.05,  min = 0, max = 1, width = "100%")),
      column(3, numericInput("clp_gsea_minGSSize", "minGSSize:", value = 10, width = "100%")),
      conditionalPanel(
        "input.clp_gsea_source=='KEGG'",
        column(3, numericInput("clp_gsea_maxGSSize", "maxGSSize:", value = 1000, width = "100%"))
      ),
      column(3, selectInput("clp_gsea_seed", "seed:", c("TRUE", "FALSE"), width = "100%")),
      column(12, align = 'center', actionButton("start_clp_gsea", "Start ClusterProfiler",
                                                style = "height:40px;font-size:18px", class = "run-button", width = "300px"))
    )
  ),
  conditionalPanel(
    "input.start_clp_gsea",
    tabsetPanel(
      tabPanel(
        "Enrichment Results Visualization",
        style = "padding-top: 5px; padding-left: 0px; padding-right: 0px;margin-left: 0px; margin-right: 0px;",
        box(
          title = "Visualization of Results", width = 4, solidHeader = TRUE,
          radioButtons("gseaPlot_type", "Methods to visualize:", c("dotplot", "gseaplot2", "ridgeplot", "exprs_heatmap"), inline = T, width = "100%" ),
          conditionalPanel(
            "input.gseaPlot_type == 'gseaplot2' | input.gseaPlot_type == 'exprs_heatmap'",
            uiOutput("expr_gseaID"),
          ),
          conditionalPanel(
            "input.gseaPlot_type == 'gseaplot2'",
            # uiOutput("expr_gseaID"),
            selectInput("gsea_ES","Geom for plotting running enrichment score:", choices = c("line", "dot"), width = "100%"),
            # selectizeInput("gsea_color", "color of score line:", choices = colors(), selected = "green", width = "100%"),
            selectInput("gsea_pTable","whether add pvalue table:", choices = c("FALSE", "TRUE"), width = "100%")
          ),
          conditionalPanel(
            "input.gseaPlot_type == 'dotplot' | input.gseaPlot_type == 'ridgeplot'",
            selectInput("gsea_useTop", "Auto or custom choices terms:", c("use topN terms", "custom select terms"), width = "100%"),
            conditionalPanel(
              "input.gsea_useTop == 'use topN terms'",
              numericInput("gsea_nterms", "TopN terms:", value = 10, width = "100%"),
            ),
            conditionalPanel(
              "input.gsea_useTop == 'custom select terms'",
              uiOutput("gseaID")
            ),
            selectInput("gsea_colorBy", "Color plots by:", c("p.adjust", "pvalue", "qvalue"), width = "100%")
          ),
          conditionalPanel(
            "input.gseaPlot_type == 'exprs_heatmap'",
            # uiOutput("expr_gseaID"),
            uiOutput("gsea_exprs_group"),
            selectInput("gsea_data_use", "Values Used To Visualize:", width = "100%",
                        c("rlog or vst transformed value" = "trans_value", "log2(normalized_counts + 1)" = "rel_value",
                          "DESeq2 normalized counts" = "norm_value")),
            # textInput("gsea_heatmap_color", "color:", value = "navy,white,red",  width = "100%"),
            # numericRangeInput("gsea_cluster_break", "Color bar value:", value = c(-2,2), width = "100%")
          ),
          numericInput("gsea_fontsize", "Fontsize:", value = 10,  min = 0, max = 50, width = "100%"),
          # conditionalPanel(
          #   "input.gseaPlot_type == 'exprs_heatmap'",
          #   awesomeCheckbox( inputId = "gsea_cluster_row", label = "Cluster genes:", value = TRUE, status = "info", width = "100%" )
          # ),
          actionButton("gsea_plot_modal_but", "Additional Parameters ...", width = "100%",
                       style = "background-color: rgb(255,255,255);text-align:left;margin-bottom:10px", icon = icon("plus-square")),
          disabled(
            actionButton("PlotGSEA", "Plot gsea", class = "plot-button", width = "100%")
          )
        ),
        bsModal(
          "gsea_plot_modal",  "Additional Parameters", "gsea_plot_modal_but", size = "large",
          fluidPage(
            style = "text-align:justify;color:black;background-color:lavender;border-radius:10px;border:1px solid black;",
            conditionalPanel(
              "input.gseaPlot_type=='dotplot'",
              h2("Additional Parameters of 'Volcano Plot':"), hr(),
              textAreaInput("gsea_dotplot_ggText", "ggplot2 codes:", value = 'theme(axis.title = element_text(size = 21, face = "bold", color = "black", family = "Times"),
                  axis.text = element_text(size = 18, face = "bold", color = "black", family = "Times"),
                  text = element_text(size = 18, face = "bold", color = "black", family = "Times"))', width = "100%", rows = 12)
            ),
            conditionalPanel(
              "input.gseaPlot_type=='gseaplot2'",
              h2("Additional Parameters of 'Volcano Plot':"), hr(),
              textAreaInput("gsea_gseaplot2_ggText", "ggplot2 codes:", value = 'theme(axis.title = element_text(size = 21, face = "bold", color = "black", family = "Times"),
                  axis.text = element_text(size = 18, face = "bold", color = "black", family = "Times"),
                  text = element_text(size = 18, face = "bold", color = "black", family = "Times"))', width = "100%", rows = 12)
            ),
            conditionalPanel(
              "input.gseaPlot_type=='ridgeplot'",
              h2("Additional Parameters of 'Volcano Plot':"), hr(),
              textAreaInput("gsea_ridgeplot_ggText", "ggplot2 codes:", value = 'theme(axis.title = element_text(size = 21, face = "bold", color = "black", family = "Times"),
                  axis.text = element_text(size = 18, face = "bold", color = "black", family = "Times"),
                  text = element_text(size = 18, face = "bold", color = "black", family = "Times"))', width = "100%", rows = 12)
            ),
            conditionalPanel(
              "input.gseaPlot_type == 'exprs_heatmap'",
              h2("Additional Parameters of 'DE Heatmap':"), hr(),
              conditionalPanel(
                "input.gsea_heat_colname",
                selectInput("gsea_heat_angle", "Column names angle:", choices = c('0', '45', '90', '270', '315'), selected = '315', width = "100%")
              ),
              numericRangeInput("gsea_cluster_break", "Color bar value:", value = c(-2,2), width = "100%"),
              numericInput("gsea_heatmap_fontsize", "Fontsize:", value = 15, width = "100%")
            )
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
                    numericInput('gsea_width', 'Figure Width:', min = 1, max = 20, value = 12, width = "100%"),
                    numericInput('gsea_height', 'Figure Height:', min = 1, max = 20, value = 4, width = "100%"),
                    downloadButton('gsea_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
                    circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px",
                    right = TRUE, tooltip = tooltipOptions(title = "Click to download figures !")
                  )
                )
              )
            ),
            uiOutput("gseaPlotUI")
          )
        ),
        column(
          2,
          wellPanel(
            sliderInput("clp_gsea_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
            sliderInput("clp_gsea_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 438, step = 2, width = "100%")
          ),
          conditionalPanel(
            "input.gseaPlot_type == 'exprs_heatmap'",
            wellPanel(
              checkboxInput("gsea_heat_rowname", "Showing rownames ?", value = TRUE, width = "100%"),
              checkboxInput("gsea_heat_colname", "Showing colnames ?", value = FALSE, width = "100%")
            )
          )
        )
      ),
      tabPanel(
        "Enrichment Results Table",
        fluidPage(
          style = "padding-top: 5px; padding-left: 0px; padding-right: 0px;margin-left: 0px; margin-right: 0px;",
          withSpinner(dataTableOutput("gsea_Tab")),
          downloadButton('gsea_Tab_Csv','Download .csv', class = "btn", width = "100%")
        )
      )
    )
  ),
  column(
    12,
    fluidRow(
      style = "background-color: rgb(248,249,250); border: 1px solid rgb(218,219,220); padding: 5px; margin:5px; border-radius: 15px;",
      column(
        4, style = "text-align:center;border-right: 2px solid white;",
        tags$img(src = "images/dist_demo.png",
                 width = "100%")
      ),
      column(
        8, style = "text-align:justify;",
        h3("What is sample-to-sample distance (SSD) ?"),
        p("Sample-to-sample distance (SSD) is another method to assess sequencing and sample replicates
          quality based on calculated distance between samples. SSDA calculated similarity between samples based on
          distance metrics, which specify how the distance between the input samples. A commonly used approach for
          measuring sample distance in RNA-seq data is to use Euclidean distance."),
        h3("How to interpret the SSD analysis results ?"),
        p("SSDA can elucidate samples distance in the high-dimensional space. In RNA-seq data, each gene is a dimension,
          so the data has tens of thousands of dimensions. SSDA uses Euclidean distance to elucidate samples distance in the
          high-dimensional space, which helps to understand the relationship of samples across exprimental conditions or sample replicates.
          The heatmap clusters samples with similar distances, which makes the results easier to interpret.")
      )
    )
  ),
  column(
    12,
    hr(),
    fluidRow(column(3, align = "right", actionLink("pGSEA", "<< Previous", style = "font-size: 20px")),
             column(6, align = "center", HTML('<p style = "text-align:center;">Copyright &copy; 2022.Shixue All rights reserved.</p>')),
             column(3, align = "left", actionLink("nGSEA", "Next >>", style = "font-size: 20px")))
  )
)
