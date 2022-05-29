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
      numericInput("wgcna_hiera_fontsize", "Fontsize for legends:", value = 15, width = "100%"),
      numericInput("wgcna_hiera_fontsize_col", "Fontsize  for colnames:", value = 15, width = "100%"),
      textInput("wgcna_hiera_color", "color:", value = "navy,white,red", placeholder = "eg. navy,white,red or #000080,#FFFFFF,#FF0000", width = "100%"),
      checkboxInput("wgcna_hiera_colname", "Specifying if column names are be shown.", value = FALSE, width = "100%"),
      conditionalPanel(
        "input.wgcna_hiera_colname == 'TRUE'",
        selectInput("wgcna_hiera_angle", "Column names angle (if showed):", choices = c('0', '45', '90', '270', '315'), selected = '315', width = "100%")
      )
    ),
    conditionalPanel(
      "input.wgcna_exp_ptype == 'BarPlot'",
      numericInput("wgcna_bar_cex", "Fontsize of base:", value = 15, width = "100%"),
      numericInput("wgcna_bar_lab", "Fontsize of label text:", value = 15, width = "100%"),
      numericInput("wgcna_bar_axis", "Fontsize of axis text:", value = 10, width = "100%"),
      numericInput("wgcna_bar_ang", "Angle of text.x:", value = 45, width = "100%")
    ),
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
      sliderInput("wgcna_expression_height", "Figure Height (px):", min = 200, max = 1000, value = 460, step = 20, width = "100%")
    )
  ),
  column(
    12,
    hr()
    # fluidRow(column(3, align = "right", actionLink("pWGCNA_4", "<< Previous", style = "font-size: 20px")),
    #          column(6, align = "center", HTML('<p style = "text-align:center;">Copyright &copy; 2022.Shixue All rights reserved.</p>')),
    #          column(3, align = "left", actionLink("nWGCNA_4", "Next >>", style = "font-size: 20px")))
  )
)
