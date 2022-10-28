fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "Run parameters:", width = 4, collapsible = TRUE, solidHeader = TRUE,
    uiOutput("intfs_funcs"),
    selectInput("intfs_sources", "Souce of function terms:", c("GO:BP", "GO:MF", "GO:CC", "KEGG", "Reactome" = "REAC"), width = "100%"),
    numericInput("intf_venn_lsize","Size of intersection labels:", value = 1,  min = 0, max = 1, width = "100%"),
    numericInput("intf_venn_nsize","Size of set names:", value = 1,  min = 0, max = 1, width = "100%"),
    numericInput("intf_top_terms","Plot top terms:", value = 18,  min = 0, max = 100, width = "100%"),
    actionButton("get_int_function", "Get intersected function", class = "run-button", width = "100%")
  ),
  column(
    8,
    tabsetPanel(
      tabPanel(
        "VennDiagram", style = "margin-top: 5px",
        column(
          8,
          fluidRow(
            column(6),
            column(
              6, align = "right",
              dropdownButton(
                numericInput('intf_venn_width', 'Figure Width:', min = 1, max = 20, value = 10, width = "100%"),
                numericInput('intf_venn_height', 'Figure Height:', min = 1, max = 20, value = 10, width = "100%"),
                downloadButton('intf_venn_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
                circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px", 
                right = TRUE, tooltip = tooltipOptions(title = "Click to download figures !")
              )
            )
          ),
          uiOutput("intf_venn_plotUI")
        ),
        column(
          4,
          wellPanel(
            sliderInput("intf_venn_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
            sliderInput("intf_venn_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 400, step = 20, width = "100%")
          )
        )
      ),
      tabPanel(
        "Function BarPlot", style = "margin-top: 5px",
        column(
          8,
          fluidRow(
            column(6),
            column(
              6, align = "right",
              dropdownButton(
                numericInput('intf_plot_Pdf_width', 'Figure Width:', min = 1, max = 20, value = 10, width = "100%"),
                numericInput('intf_plot_Pdf_height', 'Figure Height:', min = 1, max = 20, value = 10, width = "100%"),
                downloadButton('intf_plot_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
                circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px", 
                right = TRUE, tooltip = tooltipOptions(title = "Click to download figures !")
              )
            )
          ),
          uiOutput("intf_plotUI")
        ),
        column(
          4,
          wellPanel(
            sliderInput("intf_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
            sliderInput("intf_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 400, step = 20, width = "100%"),
            sliderInput("intf_plot_text_size", "Text Size:", min = 10, max = 30, value = 16, step = 1, width = "100%")
          )
        )
      ),
      tabPanel(
        "Function Table", style = "margin-top: 5px",
        column(
          12,
          withSpinner(dataTableOutput("intfs_dfs")),
          downloadButton('intfs_dfs_Csv','Download .csv', class = "btn", width = "100%")
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
        tags$img(src = "images/demo/summarize_function.png",
                 width = "100%")
      ),
      column(
        8, style = "text-align:justify;",
        h3("Why should we summarize functions?"),
        p("At present, there are many functional enrichment analysis methods and software, each of which has its own advantages. 
          This application uses two kinds of software for functional enrichment analysis, namely g:Profiler and clusterProfiler. 
          If you pay attention to it, you will find that although the input genes are the same, there are some differences in the 
          enrichment results. In addition, different enrichment results can also be obtained by ORA and GSEA analysis methods of 
          clusterProfiler software. Such a situation may confuse users."),
        p("Based on the above considerations, we think that the results of functional enrichment analysis from different sources 
          should be summarized. When a signal pathway repeatedly appears in two or three enrichment results, it shows that the enrichment 
          result of the signal pathway is true and reliable. Otherwise, users should carefully consider whether to trust the enrichment 
          results according to the experimental results and experience.")
      )
    )
  ),
  column(
    12,
    hr(),
    fluidRow(
      style = "margin-bottom:20px",
      column(3, align = "right", actionLink("psfunc", "<< Previous", style = "font-size: 20px")),
      column(6, align = "center"),
      column(3, align = "left", actionLink("nsfunc", "Next >>", style = "font-size: 20px"))
    )
  )
)
