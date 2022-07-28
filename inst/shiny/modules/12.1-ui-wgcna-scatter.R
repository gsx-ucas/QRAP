fluidRow(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "Plotting Parameters:", width = 4, collapsible = TRUE, solidHeader = TRUE,
    prettyRadioButtons(inputId = "WGCNA_scatter_method", label = "Visualize methods:",icon = icon("check"), status = "info",
                       choices = c("ggplot2 (external function)", "verboseScatterplot (WGCNA function)"), animation = "jelly", inline = TRUE),
    uiOutput("trait"),
    uiOutput("wgcna_scatter_module"),
    conditionalPanel(
      "input.WGCNA_scatter_method=='ggplot2 (external function)'",
      numericInput("wgcna_scatter_size", "Point size:", value = 1, min = 0, max = 5, width = "100%"),
      numericInput("wgcna_scatter_alpha", "Point alpha:", value = 0.8, min = 0, max = 1, width = "100%"),
      numericInput("wgcna_scatter_fontsize", "Fontsize:", value = 15, min = 0, max = 30, width = "100%"),
      actionButton("wgcna_scatter_modal_but", "Additional Parameters for Visualization ...", width = "100%",
                   style = "background-color: rgb(255,255,255);text-align:left;margin-bottom:10px", icon = icon("plus-square"))
    ),
    conditionalPanel(
      "input.WGCNA_scatter_method=='verboseScatterplot (WGCNA function)'",
      numericInput("wgcna_scatter_cex", "Size of pionts:", value = 1, width = "100%"),
      numericInput("wgcna_scatter_main", "Fontsize of main text:", value = 1.5, width = "100%"),
      numericInput("wgcna_scatter_lab", "Fontsize of label text:", value = 1.5, width = "100%"),
      numericInput("wgcna_scatter_axis", "Fontsize of axis text:", value = 1.5, width = "100%")
    ),
    actionButton("plot_wgcna_scatter", "Plot Module-Traits Relationship Heatmap", width = "100%", class = "plot-button")
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
            tags$h4("Gene Significance (GS) vs. Module Membership (MM):")
          ),
          column(
            6, align = "right", style = "padding-top:5px;",
            dropdownButton(
              numericInput('verboseScatter_width', 'Figure Width:', value = 7, width = "100%"),
              numericInput('verboseScatter_height', 'Figure Height:', value = 7, width = "100%"),
              downloadButton('verboseScatter_Pdf','Download .pdf', class = "btn btn-warning", width = "100%"),
              circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px",
              right = TRUE, tooltip = tooltipOptions(title = "Click to download figures !")
            )
          )
        )
      ),
      uiOutput("verboseScatterUI")
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("wgcna_scatter_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("wgcna_scatter_height", "Figure Height (px):", min = 200, max = 1000, value = 460, step = 20, width = "100%")
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
