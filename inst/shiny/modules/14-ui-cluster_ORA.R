fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "Runing Over-Represented Analysis (ORA) by clusterProfiler:", id = "clp_ora_tab", width = 12, collapsible = TRUE, solidHeader = TRUE,
    fluidRow(
      column(3, style = "height:74px", selectInput("clp_ora_source", "Function Source:",
                           c("Gene Ontology"="GO", "KEGG Pathway"="KEGG", "Reactome Pathway"="Reactome"),width = "100%")),
      column(3, style = "height:74px", selectInput("clp_ora_genes", "What genes to used:", width = "100%", c("DEGs", "DEG Patterns", "WGCNA Modules"))),
      column(3, style = "height:74px", uiOutput('clp_ora_gsets')),
      conditionalPanel(
        "input.clp_ora_source=='GO'",
        column(3, style = "height:74px", selectInput("GO_ont", "Sub Ontologies:", c("BP", "CC", "MF"),width = "100%"))
      ),
      conditionalPanel(
        "input.clp_ora_source=='Reactome'",
        column(3, style = "height:74px", selectInput("ora_reactome_organism", "Select organism:", c("human", "mouse", "rat", "celegans", "fly", "yeast", "zebrafish"),width = "100%"))
      ),
      column(3, style = "height:74px", selectInput("clp_ora_pAdjustMethod", "pAdjustMethod:", width = "100%", c("BH", "holm", "hochberg", "hommel", "bonferroni", "BY", "fdr", "none"))),
      column(3, style = "height:74px", numericInput("clp_ora_pval", "Pvalue threshold:", value = 0.05,  min = 0, max = 1, width = "100%")),
      column(3, style = "height:74px", numericInput("clp_ora_qval", "Qvalue threshold:", value = 0.2,  min = 0, max = 1, width = "100%")),
      column(3, style = "height:74px", numericInput("clp_ora_minGSSize", "minGSSize:", value = 10, width = "100%")),
      conditionalPanel(
        "input.clp_ora_source=='KEGG'", 
        column(3, style = "height:74px", numericInput("clp_ora_maxGSSize", "maxGSSize:", value = 1000, width = "100%"))
      ),
      column(12, align = 'center', actionButton("start_clp_ora", "Start ClusterProfiler",
                                                style = "height:40px;font-size:18px", class = "run-button", width = "300px"))
    )
  ),
  column(
    12,
    conditionalPanel(
      "input.start_clp_ora",
      tabsetPanel(
        tabPanel(
          "Enrichment Results Visualization",
          style = "padding-top: 5px; padding-left: 0px; padding-right: 0px;margin-left: 0px; margin-right: 0px;",
          box(
            title = "Visualization of Results", width = 4, solidHeader = TRUE,
            uiOutput("oraPlot_type"),
            conditionalPanel(
              "input.oraPlot_type != 'exprs_heatmap'",
              selectInput("ora_orderBy", "Order results by:", c("GeneRatio", "Count"), width = "100%")
            ),
            conditionalPanel(
              "input.oraPlot_type == 'dotplot' | input.oraPlot_type == 'barplot'",
              selectInput("useTop", "Auto or custom choices terms:", c("use topN terms", "custom select terms"), width = "100%")
            ),
            conditionalPanel(
              "(input.useTop == 'use topN terms' | input.oraPlot_type == 'cnetplot' | input.oraPlot_type == 'emapplot') & input.oraPlot_type != 'exprs_heatmap'",
              numericInput("n_terms", "TopN terms:", value = 20, width = "100%")
            ),
            conditionalPanel(
              "input.useTop == 'custom select terms' & (input.oraPlot_type == 'dotplot' | input.oraPlot_type == 'barplot')",
              uiOutput("ora_termID")
            ),
            conditionalPanel(
              "input.oraPlot_type != 'cnetplot' & input.oraPlot_type != 'exprs_heatmap'",
              selectInput("ora_colorBy", "Color plots by:", c("p.adjust", "pvalue", "qvalue"), width = "100%")
            ),
            conditionalPanel(
              "input.oraPlot_type == 'emapplot'",
              numericInput("cex_label_category", "Fontsize of category:", value = 10, width = "100%")
            ),
            conditionalPanel(
              "input.oraPlot_type == 'cnetplot'",
              selectInput("ora_node_label", "which labels to be displayed:", c("all", "category", "gene"), width = "100%"),
              selectInput("ora_circular", "whether using circular layout:", c("FALSE", "TRUE"), width = "100%")
            ),
            conditionalPanel(
              "input.oraPlot_type == 'exprs_heatmap'",
              uiOutput("ora_termID2"), uiOutput("ora_exprs_group"),
              selectInput("ora_data_use", "Values Used To Visualize:", width = "100%",
                          c("rlog or vst transformed value" = "trans_value", "log2(normalized_counts + 1)" = "rel_value",
                            "DESeq2 normalized counts" = "norm_value")),
              numericInput("ora_heatmap_fontsize", "Fontsize:", value = 15, width = "100%"),
            ),
            actionButton("cl_ora_modal_but", "Additional Parameters ...", width = "100%",
                         style = "background-color: rgb(255,255,255);text-align:left;margin-bottom:10px", icon = icon("plus-square")),
            disabled(
              actionButton("plotORA", "Plot ORA results", width = "100%", class = "plot-button")
            )
          ),
          bsModal(
            "cl_ora_modal", "Additional Parameters", "cl_ora_modal_but", size = "large",
            fluidPage(
              style = "text-align:justify;color:black;background-color:lavender;border-radius:10px;",
              h3("ggplot2 codes of 'ORA Dot Plot':"), hr(),
              conditionalPanel(
                "input.oraPlot_type != 'exprs_heatmap'",
                textAreaInput("cl_ora_ggText", "ggplot2 codes:", value = 'theme(axis.title = element_text(size = 21, face = "bold", color = "black", family = "Times"),
              axis.text = element_text(size = 18, face = "bold", color = "black", family = "Times"),
              text = element_text(size = 18, face = "bold", color = "black", family = "Times"))', width = "100%", rows = 12)
              ),
              conditionalPanel(
                "input.oraPlot_type == 'exprs_heatmap'",
                conditionalPanel(
                  "input.ora_heat_colname",
                  selectInput("ora_heat_angle", "Column names angle:", choices = c('0', '45', '90', '270', '315'), selected = '315', width = "100%")
                ),
                numericRangeInput("ora_cluster_break", "Color bar value:", value = c(-2,2), width = "100%")
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
              sliderInput("ora_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 438, step = 2, width = "100%")
            ),
            conditionalPanel(
              "input.oraPlot_type == 'exprs_heatmap'",
              wellPanel(
                checkboxInput("ora_heat_rowname", "Showing rownames ?", value = TRUE, width = "100%"),
                checkboxInput("ora_heat_colname", "Showing colnames ?", value = FALSE, width = "100%")
              )
            )
          )
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
    )
  ),
  column(
    12,
    fluidRow(
      style = "background-color: rgb(248,249,250); border: 1px solid rgb(218,219,220); padding: 5px; margin:5px; border-radius: 15px;",
      column(
        4, style = "text-align:center;border-right: 2px solid white; padding-top:15px",
        tags$img(src = "images/demo/ORA_dotplot.png",
                 width = "100%")
      ),
      column(
        8, style = "text-align:justify;",
        h3("What is Over-Representation Analysis (ORA) ?"),
        p("Over Representation Analysis (ORA) is a widely used method to determine whether an 
          experimentally-derived gene list, e.g. a list of differentially expressed genes (DEGs), 
          are enriched in known biological functions or processes. Over-representation analysis is 
          a statistical method that determines whether genes from pre-defined sets (ex: those beloging 
          to a specific GO term or KEGG pathway) are present more than would be expected (over-represented) 
          in a subset of your data. In this case, the subset of “interesting” genes can be 
          differentially expressed genes (DEGs) or other gene list like DEG expression pattern and WGCNA modules.
          
          The R package clusterProfiler was used to accomplish this step, and only organisms with OrgDb object 
          available can be supported in our application. A variety of visualization methods, such as dotplot, barplot, 
          centplot and emapplot, make it easay for users to choices a preferred way to display the 
          results and generate graphics for their publications.")
      )
    )
  ),
  column(
    12,
    hr(),
    fluidRow(
      style = "margin-bottom:20px",
      column(3, align = "right", actionLink("pORA", "<< Previous", style = "font-size: 20px")),
      column(6, align = "center"),
      column(3, align = "left", actionLink("nORA", "Next >>", style = "font-size: 20px"))
    )
  )
)
