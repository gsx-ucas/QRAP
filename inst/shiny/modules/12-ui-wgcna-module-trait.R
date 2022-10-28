fluidRow(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "Module-Traits Relationship:", width = 4, collapsible = TRUE, solidHeader = TRUE,
    prettyRadioButtons(inputId = "WGCNA_Heatmap_method", label = "Visualize methods:",icon = icon("check"), status = "info",
                       choices = c("pheatmap (external function)", "labeledHeatmap (WGCNA function)"), animation = "jelly", inline = TRUE),
    conditionalPanel(
      "input.WGCNA_Heatmap_method == 'labeledHeatmap (WGCNA function)'",
      numericInput("cex_text", "cex.text:", value = 1, width = "100%"),
      numericInput("yColorWidth", "yColorWidth:", value = 0.05, width = "100%"),
      selectInput("font_lab", "font.labs:", choices = c("1", "2", "3", "4"), selected = "2", width = "100%")
    ),
    conditionalPanel(
      "input.WGCNA_Heatmap_method == 'pheatmap (external function)'",
      numericInput("WGCNA_heatmap_fontsize", "Fontsize for legends:", value = 15, width = "100%"),
      numericInput("WGCNA_heatmap_fontsize_col", "Fontsize  for colnames:", value = 15, width = "100%"),
      numericInput("WGCNA_heatmap_fontsize_num", "Fontsize  for fill text:", value = 8, width = "100%")
    ),
    textInput("module_colors", "Colors:", width = "100%", value = "blue,white,red"),
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
      sliderInput("wgcna_heatmap_height", "Figure Height (px):", min = 200, max = 1000, value = 541, step = 20, width = "100%")
    ),
    conditionalPanel(
      "input.WGCNA_Heatmap_method == 'pheatmap (external function)'",
      wellPanel(
        checkboxInput("WGCNA_heatmap_cluster_cols", "clustering columns ?", value = TRUE, width = "100%"),
        checkboxInput("WGCNA_heatmap_cluster_rows", "clustering rows ?", value = TRUE, width = "100%")
      )
    )
  ),
  column(
    12, style = "padding:0px;",
    fluidRow(
      style = "background-color: rgb(248,249,250); border: 1px solid rgb(218,219,220); padding: 5px; margin:5px; border-radius: 15px;",
      column(
        4, style = "text-align:center;border-right: 2px solid white; padding-top:15px",
        tags$img(src = "images/demo/wgcna_mtrelationship.png",
                 width = "100%")
      ),
      column(
        8, style = "text-align:justify;",
        h3("How to relate network concepts to external gene or sample information ?"),
        p("A main purpose of many network analyses is to relate a connectivity measure to external gene or sample information.
          For example, to find that the intra-modular connectivity of a modular gene is highly related to the prognostic significance 
          of cancer survival. This facilitates novel strategies for screening for therapeutic targets."),
        p("In this analysis we would like to identify modules that are significantly associated with the measured clinical traits.
          To get a sense of how related the modules are one can summarize each module by its eigengene, 
          after have a summary profile (eigengene) for each module, we simply correlate eigengenes with external traits 
          and look for the most significant associations. Since we have a moderately large number of modules and traits, 
          a suitable graphical representation will help in reading the table. We color code each association by the correlation value
          in the heatmap graph. The analysis identifies the several significant module–trait associations. 
          We will concentrate on the genetic modules that are most relevant to the phenotypic traits of interest.")
      )
    )
  ),
  column(
    12,
    hr(),
    fluidRow(
      style = "margin-bottom:20px",
      column(3, align = "right", actionLink("pWGCNA_4", "<< Previous", style = "font-size: 20px")),
      column(6, align = "center"),
      column(3, align = "left", actionLink("nWGCNA_4", "Next >>", style = "font-size: 20px"))
    )
  )
)
