fluidRow(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "Plotting Parameters:", width = 4, collapsible = TRUE, solidHeader = TRUE,
    prettyRadioButtons(inputId = "wgcna_exp_ptype", label = "Visualization method:", animation = "jelly", inline = TRUE,
                       choices = c("Pheatmap", "BarPlot"), icon = icon("check"), status = "info"),
    uiOutput("wgcna_exp_trait"),
    uiOutput("wgcna_exp_module"),
    conditionalPanel(
      "input.wgcna_exp_ptype == 'Pheatmap'",
      uiOutput("wgcna_exp_anno"),
      numericInput("wgcna_hiera_fontsize", "Fontsize:", value = 15, width = "100%"),
      textInput("wgcna_hiera_color", "color:", value = "navy,white,red", placeholder = "eg. navy,white,red or #000080,#FFFFFF,#FF0000", width = "100%")
    ),
    conditionalPanel(
      "input.wgcna_exp_ptype == 'BarPlot'",
      numericInput("wgcna_bar_cex", "Fontsize of base:", value = 18, width = "100%"),
      numericInput("wgcna_bar_lab", "Fontsize of label text:", value = 18, width = "100%"),
      numericInput("wgcna_bar_axis", "Fontsize of axis text:", value = 12, width = "100%"),
    ),
    actionButton("wgcna_expr_modal_but", "Additional Parameters ...", width = "100%",
                 style = "background-color: rgb(255,255,255);text-align:left; margin-bottom:10px", icon = icon("plus-square")),
    br(),
    actionButton("wgcna_plot_exp", "Plot Module-Traits Relationship Heatmap", width = "100%", class = "plot-button")
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
            tags$h4("Visualization of genes expression in Module:")
          ),
          column(
            6, align = "right", style = "padding-top:5px;",
            dropdownButton(
              numericInput('wgcna_exp_width', 'Figure Width:', value = 7, width = "100%"),
              numericInput('wgcna_exp_height', 'Figure Height:', value = 7, width = "100%"),
              downloadButton('wgcna_exp_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
              circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px",
              right = TRUE, tooltip = tooltipOptions(title = "Click to download figures !")
            )
          )
        )
      ),
      uiOutput("wgcna_expressionUI")
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("wgcna_expression_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("wgcna_expression_height", "Figure Height (px):", min = 200, max = 1000, value = 511, step = 2, width = "100%")
    )
  ),
  bsModal(
    "wgcna_expr_modal", "Additional Parameters", "wgcna_expr_modal_but", size = "large",
    fluidPage(
      style = "text-align:justify;color:black;background-color:lavender;border-radius:10px;",
      h3("Additional Parameters:"), hr(),
      conditionalPanel(
        "input.wgcna_exp_ptype == 'Pheatmap'",
        checkboxInput("wgcna_hiera_colname", "show column names ?", value = FALSE, width = "100%"),
        conditionalPanel(
          "input.wgcna_hiera_colname",
          selectInput("wgcna_hiera_angle", "Column names angle (if showed):", choices = c('0', '45', '90', '270', '315'), selected = '315', width = "100%")
        )
      ),
      conditionalPanel(
        "input.wgcna_exp_ptype == 'BarPlot'",
        textAreaInput("wgcna_expr_ggText", "ggplot2 codes:", value = 'theme(axis.title = element_text(face = "bold", color = "black", family = "Times"),
        axis.text = element_text(face = "bold", color = "black", family = "Times"),
        text = element_text(face = "bold", color = "black", family = "Times"))', width = "100%", rows = 12)
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
    fluidRow(column(3, align = "right", actionLink("pWGCNA_expression", "<< Previous", style = "font-size: 20px")),
             column(6, align = "center", HTML('<p style = "text-align:center;">Copyright &copy; 2022.Shixue All rights reserved.</p>')),
             column(3, align = "left", actionLink("nWGCNA_expression", "Next >>", style = "font-size: 20px")))
  )
)
