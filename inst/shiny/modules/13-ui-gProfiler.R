fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "Runing gProfiler:", id = "gprofiler_tab", width = 12, collapsible = TRUE, solidHeader = TRUE,
    fluidRow(
      column(3, style = "height:74px", selectInput("gprofiler_genes", "What genes to used:",  c("DEGs", "DEG Patterns", "WGCNA Modules"),width = "100%")),
      column(3, style = "height:74px", uiOutput('gprofiler_gsets')),
      column(3, style = "height:74px", numericInput("gprofiler_pval", "Pvalue threshold:", value = 0.05,  min = 0, max = 1, width = "100%")),
      column(3, style = "height:74px", selectInput("gprofiler_cor_method", "Correction method:",
                            c("g_SCS", "bonferroni", "fdr", "false_discovery_rate", "gSCS","analytical"), width = "100%")),
      column(3, style = "height:74px", selectInput("gprofiler_evcodes", "Show evidence:", c("TRUE", "FALSE"), width = "100%")),
      column(
        3, style = "height:74px",
        virtualSelectInput(
          inputId = "gprofiler_sources",  label = "Data sources to use::", 
          choices = c("GO:BP", "GO:MF", "GO:CC", "KEGG", "REAC", "TF", "MIRNA", "CORUM", "HP", "HPA", "WP"),
          selected = c("GO:BP", "GO:MF", "GO:CC", "KEGG", "REAC"),  multiple = T, search = F, width = "100%"
        )
      ),
      column(3, style = "height:74px", selectInput("gprofiler_significant", "Only return significant results:", choices = c("TRUE", "FALSE"), width = "100%")),
      column(3, style = "height:74px", selectInput("gprofiler_excludeIEA", "Exclude GO electronic annotations:", choices = c("FALSE","TRUE"), width = "100%")),
      column( 12, align = "center", actionButton("runGprofiler", "Run gProfiler", class = "run-button", width = "30%") )
    )
  ),
  column(
    12,
    conditionalPanel(
      "input.runGprofiler",
      tabsetPanel(
        tabPanel(
          "Enrichment Results Visualization",
          fluidPage(
            style = "padding-top: 5px; padding-left: 0px; padding-right: 0px;margin-left: 0px; margin-right: 0px;",
            box(
              title = "Enrichment Results Visualiztion", width = 4, collapsible = T, solidHeader = TRUE,
              uiOutput("gprofiler_plot_type"),
              uiOutput("sourceTypes"),
              conditionalPanel(
                "input.gprofiler_type=='dotplot'",
                selectInput("gprofiler_orderBy", "Order results by:", c("precision", "recall"), width = "100%"),
                selectInput("gprofiler_Top", "Auto or custom choices terms:", c("use topN terms", "custom select terms"), width = "100%"),
                conditionalPanel(
                  "input.gprofiler_Top=='use topN terms'",
                  numericInput("gprofiler_n_terms", "TopN terms:", value = 20, width = "100%")
                )
              ),
              conditionalPanel(
                "input.gprofiler_type != 'exprs_heatmap'",
                uiOutput("gprofiler_termID")
              ),
              conditionalPanel(
                "input.gprofiler_type=='gostplot' | input.gprofiler_type=='gosttable'",
                numericInput("gprofiler_tbfontsize", "Fontsize:", value = 10,  min = 0, width = "100%"),
                selectInput("gprofiler_showLink", "Show Link in bottom:", choices = c("FALSE", "TRUE"), width = "100%"),
                pickerInput("gprofiler_showColumns", "Columns will be showed:", width = "100%", multiple = T,
                            choices = c("source", "term_name", "term_size", "intersection_size"),
                            selected = c("source", "term_name", "term_size", "intersection_size"))
              ),
              conditionalPanel(
                "input.gprofiler_type == 'exprs_heatmap'",
                uiOutput("gprofiler_termID2"),
                uiOutput("gprofiler_exprs_group"),
                selectInput("gprofiler_data_use", "Values Used To Visualize:", width = "100%",
                            c("rlog or vst transformed value" = "trans_value", "log2(normalized_counts + 1)" = "rel_value",
                              "DESeq2 normalized counts" = "norm_value"))
              ),
              actionButton("gprofiler_plot_modal_but", "Additional Parameters ...", width = "100%",
                           style = "background-color: rgb(255,255,255);text-align:left;margin-bottom:10px", icon = icon("plus-square")),
              disabled(actionButton("Plot_gprofiler", "Plot gProfiler", class = "plot-button", width = "100%"))
            ),
            bsModal(
              "gprofiler_plot_modal",  "Additional Parameters", "gprofiler_plot_modal_but", size = "large",
              fluidPage(
                style = "text-align:justify;color:black;background-color:lavender;border-radius:10px;border:1px solid black;",
                conditionalPanel(
                  "input.gprofiler_type=='dotplot'",
                  h2("Additional Parameters of 'Volcano Plot':"), hr(),
                  textAreaInput("gprofiler_dotplot_ggText", "ggplot2 codes:", value = 'theme(axis.title = element_text(size = 21, face = "bold", color = "black", family = "Times"),
                  axis.text = element_text(size = 18, face = "bold", color = "black", family = "Times"),
                  text = element_text(size = 18, face = "bold", color = "black", family = "Times"))', width = "100%", rows = 12)
                ),
                conditionalPanel(
                  "input.gprofiler_type=='gostplot'",
                  h2("Additional Parameters of 'Volcano Plot':"), hr(),
                  textAreaInput("gprofiler_gostplot_ggText", "ggplot2 codes:", value = 'theme(axis.title = element_text(size = 21, face = "bold", color = "black", family = "Times"),
                  axis.text = element_text(size = 18, face = "bold", color = "black", family = "Times"),
                  text = element_text(size = 18, face = "bold", color = "black", family = "Times"))', width = "100%", rows = 12)
                ),
                conditionalPanel(
                  "input.gprofiler_type=='gosttable'",
                  h2("Additional Parameters of 'Volcano Plot':"), hr(),
                  textAreaInput("gprofiler_gosttable_ggText", "ggplot2 codes:", value = 'theme(axis.title = element_text(size = 21, face = "bold", color = "black", family = "Times"),
                  axis.text = element_text(size = 18, face = "bold", color = "black", family = "Times"),
                  text = element_text(size = 18, face = "bold", color = "black", family = "Times"))', width = "100%", rows = 12)
                ),
                conditionalPanel(
                  "input.gprofiler_type == 'exprs_heatmap'",
                  h2("Additional Parameters of 'DE Heatmap':"), hr(),
                  conditionalPanel(
                    "input.gprofiler_heat_colname",
                    selectInput("gprofiler_heat_angle", "Column names angle (if showed):", choices = c('0', '45', '90', '270', '315'), selected = '315', width = "100%")
                  ),
                  numericRangeInput("gprofiler_cluster_break", "Color bar value:", value = c(-2,2), width = "100%"),
                  numericInput("gprofiler_heatmap_fontsize", "Fontsize:", value = 15, width = "100%")
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
                        numericInput('gprofiler_width', 'Figure Width:', min = 1, max = 20, value = 12, width = "100%"),
                        numericInput('gprofiler_height', 'Figure Height:', min = 1, max = 20, value = 6, width = "100%"),
                        downloadButton('gprofiler_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
                        circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px",
                        right = TRUE, tooltip = tooltipOptions(title = "Click to download figures !")
                      )
                    )
                  )
                ),
                uiOutput("gprofilerPlotUI")
              )
            ),
            column(
              2,
              wellPanel(
                sliderInput("gprofiler_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
                sliderInput("gprofiler_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 438, step = 2, width = "100%")
              ),
              conditionalPanel(
                "input.gprofiler_type == 'exprs_heatmap'",
                wellPanel(
                  checkboxInput("gprofiler_heat_rowname", "Showing rownames ?", value = TRUE, width = "100%"),
                  checkboxInput("gprofiler_heat_colname", "Showing colnames ?", value = FALSE, width = "100%")
                )
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
    fluidRow(column(3, align = "right", actionLink("pGprofiler", "<< Previous", style = "font-size: 20px")),
             column(6, align = "center", HTML('<p style = "text-align:center;">Copyright &copy; 2022.Shixue All rights reserved.</p>')),
             column(3, align = "left", actionLink("nGprofiler", "Next >>", style = "font-size: 20px")))
  )
)
