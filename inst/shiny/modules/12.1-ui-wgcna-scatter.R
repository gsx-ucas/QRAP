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
      numericInput("wgcna_scatter_size", "Point size:", value = 2, min = 0, max = 5, width = "100%"),
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
            9, style = "padding-left:10px;",
            tags$h4("Gene Significance (GS) vs. Module Membership (MM):")
          ),
          column(
            3, align = "right", style = "padding-top:5px;",
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
      uiOutput("render_wgcna_scatter_height")
      # sliderInput("wgcna_scatter_height", "Figure Height (px):", min = 200, max = 1000, value = 522, step = 20, width = "100%")
    )
  ),
  bsModal(
    "wgcna_scatter_modal", "Additional Parameters", "wgcna_scatter_modal_but", size = "large",
    fluidPage(
      style = "text-align:justify;color:black;background-color:lavender;border-radius:10px;",
      h3("Additional Parameters:"), hr(),
      textAreaInput("wgcna_scatter_ggText", "ggplot2 codes:", value = 'theme(axis.title = element_text(size = 21, face = "bold", color = "black", family = "Times"),
        axis.text = element_text(size = 18, face = "bold", color = "black", family = "Times"),
        text = element_text(size = 18, face = "bold", color = "black", family = "Times"))', width = "100%", rows = 12)
    )
  ),
  column(
    12, style = "padding:0px;",
    fluidRow(
      style = "background-color: rgb(248,249,250); border: 1px solid rgb(218,219,220); padding: 5px; margin:5px; border-radius: 15px;",
      column(
        4, style = "text-align:center;border-right: 2px solid white; padding-top:15px",
        tags$img(src = "images/demo/wgcna_gsmm.png",
                 width = "100%")
      ),
      column(
        8, style = "text-align:justify;",
        h3("Concept about Gene Significance (GS) and Module Membership (MM)"),
        p("We quantify associations of individual genes with our trait of interest by defining 
          Gene Significance GS as (the absolute value of) the correlation between the gene and the trait. 
          For each module, we also define a quantitative measure of module membership MM as the correlation 
          of the module eigengene and the gene expression profile. This allows us to quantify the similarity 
          of all genes on the array to every module. "),
        p("Using the GS and MM measures, we can identify genes that have a high significance for trait of interest 
          as well as high module membership in interesting modules. As an example, we look at the turquoise module 
          that has the highest association with HFD45 (hospital-free days at day 45) score of COVID-19 patients. 
          We plot a scatterplot of Gene Significance vs. Module Membership in the turquoise module in the left."),
        p("The scatter plot is shown in the left Clearly, GS and MM are highly correlated, illustrating that genes 
          highly significantly associated with a trait are often also the most important (central) elements of modules associated with the trait.")
      )
    )
  ),
  column(
    12,
    hr(),
    fluidRow(
      style = "margin-bottom:20px",
      column(3, align = "right", actionLink("pWGCNA_scatter", "<< Previous", style = "font-size: 20px")),
      column(6, align = "center"),
      column(3, align = "left", actionLink("nWGCNA_scatter", "Next >>", style = "font-size: 20px"))
    )
  )
)
