fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "Run parameters:", width = 4, collapsible = TRUE, solidHeader = TRUE,
    checkboxGroupInput("intgs_genesets", "Gene Sets to explore:", inline = T, width = "100%",
                       choices = c("DEGs", "DEG Pattern", "WGCNA module"),
                       selected = c("DEGs", "DEG Pattern", "WGCNA module")),
    uiOutput("intgs_degs"),
    uiOutput("intgs_degps"),
    uiOutput("intgs_wgcnaps"),
    numericInput("intg_venn_lsize","Size of intersection labels:", value = 1,  min = 0, max = 1, width = "100%"),
    numericInput("intg_venn_nsize","Size of set names:", value = 1,  min = 0, max = 1, width = "100%"),
    actionButton("get_int_genes", "Get intersected genes", class = "run-button", width = "100%")
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
                numericInput('intg_venn_width', 'Figure Width:', min = 1, max = 20, value = 10, width = "100%"),
                numericInput('intg_venn_height', 'Figure Height:', min = 1, max = 20, value = 10, width = "100%"),
                downloadButton('intg_venn_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
                circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px",
                right = TRUE, tooltip = tooltipOptions(title = "Click to download figures !")
              )
            )
          ),
          uiOutput("intg_plotUI")
        ),
        column(
          4,
          wellPanel(
            sliderInput("intg_venn_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
            sliderInput("intg_venn_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 420, step = 20, width = "100%")
          )
        )
      ),
      tabPanel(
        "Intersected Table", style = "margin-top: 5px",
        column(
          12,
          uiOutput("intgs_df_id"),
          withSpinner(dataTableOutput("intgs_dfs")),
          downloadButton('intgs_dfs_Csv','Download .csv', class = "btn", width = "100%")
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
        tags$img(src = "images/demo/summarize_gene.png",
                 width = "100%")
      ),
      column(
        8, style = "text-align:justify;",
        h3("Why should we summarize genes?"),
        p("In our application, user can detected differentially expressed genes (DEG) of interested contrast conditions,
          expression pattern of differentially expressed genes (DEG patterns) and WGCNA module genes (WGCNA modules).
          These genes are grouped because the criteria for defining them are different, for example, the standard for 
          differentially expressed genes is abs(log2 fold change) > 1 and p < 0.05.
          Some genes that are meaningful but do not change significantly may be lost by such rough criteria. 
          WGCNA analysis uses all the highly expressed genes for module detection and can correlate external phenotypic data, 
          which can make up for the missing information in differential expression analysis to some extent.
          Although the DEG patterns is based on the differential expression analysis, 
          it can help us to understand the expression of differentially expressed genes with expression trend at a specific time point."),
        p("Nevertheless, when the genes of three different modules point to the same biological meaning of interest, the 
          intersection genes between the three modules may be some of the more important and critical genes.")
      )
    )
  ),
  column(
    12,
    hr(),
    fluidRow(
      style = "margin-bottom:20px",
      column(3, align = "right", actionLink("psgene", "<< Previous", style = "font-size: 20px")),
      column(6, align = "center"),
      column(3, align = "left", actionLink("nsgene", "Next >>", style = "font-size: 20px"))
    )
  )
)
