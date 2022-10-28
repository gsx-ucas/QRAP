fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "Plot Parameters:", width = 4, collapsible = TRUE, solidHeader = TRUE,
    textAreaInput("input_gene", "Input genes:", rows = 5, width = "100%", placeholder = "Gene1,Gene2,Gene3 ... or Gene1\nGene2\nGene3 ..."),
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
        textAreaInput("exprs_ggText", "ggplot2 codes:", value = 'scale_color_brewer(palette = "Set1")+
        theme(axis.title = element_text(size = 21, face = "bold", color = "black", family = "Times"),
        axis.text = element_text(size = 18, face = "bold", color = "black", family = "Times"),
        text = element_text(size = 18, face = "bold", color = "black", family = "Times"))', width = "100%", rows = 12)
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
      sliderInput("epv_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 520, step = 2, width = "100%")
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
        4, style = "text-align:center;border-right: 2px solid white; padding-top:15px",
        tags$img(src = "images/demo/expression_plot.png",
                 width = "100%")
      ),
      column(
        8, style = "text-align:justify;",
        h3("How to perform the gene expression visualization ?"),
        p("The visualization of gene expression values is also an important part of RNA-seq anaysis. 
        Choosing the appropriate visualization method can clearly observe the expression differences 
        of interested genes under different experimental conditions. Common visualization methods of 
        gene expression include bar-plot, box-plot, heatmap and so on."),
        p("When visualizing gene expression, it is also very important to select the appropriate value. 
          Usually, the original gene expression value is not normalized and is not suitable for direct visualization of gene expression.
          In the analysis of DESeq2, it first standardizes the original gene expression values (DESeq2 normalized counts),
          and then converts the normalized values through rlog or vst (rlog or vst transformed value).
          These normalized and transformed values can be directly used for the visualization of gene expression.
          The normalized value of DESeq2 may be relatively large, which requires logarithmic conversion (log2(normalized_count + 1)).
          TO more intuitively observe the changes of genes in each group compared with those in the control group, 
          foldchanges (log2 FoldChange) can be used to visualize gene expression.")
      )
    )
  ),
  column(
    12,
    hr(),
    fluidRow(
      style = "margin-bottom:20px",
      column(3, align = "right", actionLink("pEpv", "<< Previous", style = "font-size: 20px")),
      column(6, align = "center"),
      column(3, align = "left", actionLink("nEpv", "Next >>", style = "font-size: 20px"))
    )
  )
)
