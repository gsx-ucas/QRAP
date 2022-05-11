fluidRow(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "Module-Traits Relationship:", width = 4, collapsible = TRUE, solidHeader = TRUE,
    # numericInput("xLabelsAngle", "xLabelsAngle:", value = 45, width = "100%"),
    numericInput("cex_text", "cex.text:", value = 1, width = "100%"),
    numericInput("yColorWidth", "yColorWidth:", value = 0.05, width = "100%"),
    textInput("module_colors", "Colors:", width = "100%", value = "blue,white,red"),
    selectInput("font_lab", "font.labs:", choices = c("1", "2", "3", "4"), selected = "2", width = "100%"),
    uiOutput("module_showRows"),
    uiOutput("module_showCols"),
    actionButton("plot_mtrs", "Plot Module-Traits Relationship Heatmap", width = "100%", class = "plot-button")
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
            tags$h4("Module-Trait Relationshipes Heatmap:")
          ),
          column(
            6, align = "right", style = "padding-top:5px;",
            dropdownButton(
              numericInput('mtrs_heatmap_width', 'Figure Width:', value = 10, width = "100%"),
              numericInput('mtrs_heatmap_height', 'Figure Height:', value = 10, width = "100%"),
              downloadButton('mtrs_heatmap_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
              circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px",
              right = TRUE, tooltip = tooltipOptions(title = "Click to download figures !")
            )
          )
        )
      ),
      uiOutput("mtrs_heatmapUI")
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("wgcna_heatmap_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("wgcna_heatmap_height", "Figure Height (px):", min = 200, max = 1000, value = 550, step = 20, width = "100%")
    )
  ),
  column(
    12,
    hr(),
    fluidRow(column(3, align = "right", actionLink("pWGCNA_4", "<< Previous", style = "font-size: 20px")),
             column(6, align = "center", HTML('<p style = "text-align:center;">Copyright &copy; 2022.Shixue All rights reserved.</p>')),
             column(3, align = "left", actionLink("nWGCNA_4", "Next >>", style = "font-size: 20px")))
  )
)
