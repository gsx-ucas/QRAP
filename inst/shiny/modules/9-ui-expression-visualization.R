fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "Plot Parameters:", width = 4, collapsible = TRUE, solidHeader = TRUE,
    textAreaInput("input_gene", "Input genes:", rows = 5, width = "100%", placeholder = "Gene1,Gene2,Gene3 ..."),
    selectInput("data_use", "Values Used To Visualize:", width = "100%",
                c("rlog or vst transformed value" = "trans_value", "log2(normalized_counts + 1)" = "rel_value",
                  "DESeq2 normalized counts" = "norm_value", "log2 FoldChange" = "log2flc")),
    conditionalPanel(
      "input.data_use != 'log2flc'",
      uiOutput("expr_groupby"),
      uiOutput('expr_group')
    ),
    conditionalPanel(
      "input.data_use == 'log2flc'", 
      uiOutput('expr_de_group'),
    ),
    uiOutput("expr_plotType"),
    conditionalPanel(
      "input.data_use == 'log2flc'",
      numericInput("exprsh_fontsize", "Fontsize:", value = 15, width = "100%")
    ),
    actionButton("exprs_modal_but", "Additional Parameters for Visualization ...", width = "100%",
                 style = "background-color: rgb(255,255,255);text-align:left;margin-bottom:10px", icon = icon("plus-square")),
    actionButton("plot_geneExpr", "Plotting", class = "plot-button", width = "100%")
  ),
  bsModal(
    "gene_exprs_modal", "Additional Parameters", "exprs_modal_but", size = "large",
    fluidRow(
      column(
        6, style = "text-align:justify;color:black;background-color:lavender;border-radius:10px;border:1px solid black;", br(),
        h6("Additional Parameters of 'BarPlot & BoxPlot':"), hr(),
        numericInput("Expr_cols", "Figure columns:", value = 3, width = "100%"),
        selectInput("Expr_error", "Error bar values:", width = "100%",
                    choices = c('standard error (SE)' = 'se',  'standard deviation (SD)' = 'sd', 'confidence interval (CI)' = 'ci')),
        numericInput("Expr_error_lwd", "Error bar line width:", value = 0.6, width = "100%"),
        HTML(
          paste0(
            '<div class="form-group shiny-input-container" style="width: 100%;">',
            '<label class="control-label" for="exprs_ggText">ggplot2 codes:</label>',
            '<textarea id="exprs_ggText" class="form-control" placeholder="theme(text = element_text(face = &#39;bold&#39;))" style="width: 100%;" rows="12"></textarea>',
            '</div>'
          )
        )
      ),
      column(
        6,
        style = "text-align:justify;color:black;background-color:papayawhip;border-radius:10px;border:1px solid black;",
        br(),
        h6("Additional Parameters of 'multiple (heatmap)':"),
        hr(),
        checkboxInput("exprsh_colname", "Specifying if column names are be shown.", value = FALSE, width = "100%"),
        checkboxInput("exprsh_colanno", "Specifying if column annotations are be shown.", value = TRUE, width = "100%"),
        checkboxInput("cluster_row", "Specifying if genes (rows side) should be clustered.", value = TRUE, width = "100%"),
        numericInput("exprsh_treeheight_row", "The height of a tree for rows:", value = 20, width = "100%"),
        numericInput("exprsh_fontsize", "Fontsize:", value = 15, width = "100%"),
        textInput("exprsh_color", "color:", value = "navy,white,red",  width = "100%"),
        numericRangeInput("Expr_break", "Color bar value range:", value = c(-2,2), width = "100%"),
        selectInput("exprsh_scale", "How to scale data:", choices = c('row', 'column', 'none'), selected = 'row', width = "100%"),
        selectInput("exprsh_angle", "Column names angle (if showed):", choices = c('0', '45', '90', '270', '315'), selected = '315', width = "100%")
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
            tags$h4("Gene Expression Visualization:")
          ),
          column(
            6, align = "right", style = "padding-top:5px;",
            dropdownButton(
              numericInput('geneExpr_width', 'Figure Width:', min = 1, max = 20, value = 7, width = "100%"),
              numericInput('geneExpr_height', 'Figure Height:', min = 1, max = 20, value = 5, width = "100%"),
              downloadButton('geneExpr_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
              circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px", right = TRUE,
              tooltip = tooltipOptions(title = "Click to download figures !")
            )
          )
        )
      ),
      # withSpinner(plotOutput("geneExpr_plot", height = "550px"))
      uiOutput("epv_plotUI")
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("epv_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("epv_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 518, step = 10, width = "100%")
    ),
    conditionalPanel(
      "input.expr_plotType=='BarPlot' | input.expr_plotType=='BoxPlot'",
      wellPanel(
        checkboxInput("Expr_split", "split figure by genes?", value = FALSE, width = "100%")
      )
    )
  ),
  column(
    12, style = "padding:0px;",
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
    fluidRow(column(3, align = "right", actionLink("pEpv", "<< Previous", style = "font-size: 20px")),
             column(6, align = "center", HTML('<p style = "text-align:center;">Copyright &copy; 2022.Shixue All rights reserved.</p>')),
             column(3, align = "left", actionLink("nEpv", "Next >>", style = "font-size: 20px")))
  )
)
