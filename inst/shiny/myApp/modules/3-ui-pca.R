fluidPage(
  style = "margin-left:10px;margin-right:10px;",
  box(
    title = "PCA Parameters", width = 4, status = NULL, solidHeader = TRUE,
    uiOutput("pca_samples"),
    numericInput("pca_size", "Point size:", value = 5, width = "100%"),
    selectInput("pca_theme", "Figure themes:",
                choices = c("theme_bw", "theme_classic", "theme_test", "theme_linedraw",
                            "theme_light", "theme_minimal", "theme_grey", "theme_gray", "theme_dark")),
    uiOutput("pca_colorby"),
    uiOutput("pca_shapeby"),
    actionButton("pca_modal_but", "Additional Parameters for Visualization ...", width = "100%",
                 style = "background-color: rgb(255,255,255);text-align:left;margin-bottom:10px", icon = icon("plus-square")),
    actionButton("plot_pca", "Plotting PCA", width = "100%", class = "plot-button")
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
            tags$h4("PCA ScatterPlot:")
          ),
          column(
            6, align = "right", style = "padding-top:5px;",
            dropdownButton(
              numericInput('pca_width', 'Figure Width (cm):', min = 1, max = 20, value = 7, width = "100%"),
              numericInput('pca_height', 'Figure Height (cm):', min = 1, max = 20, value = 5, width = "100%"),
              downloadButton('pca_Pdf','Download .pdf', class = "btn", width = "100%"),
              circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px", right = TRUE,
              tooltip = tooltipOptions(title = "Click to download figures !")
            )
          )
        )
      ),
      textOutput("text_pca_samples"),
      uiOutput("PCA_plotUI")
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("pca_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("pca_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 453, step = 2, width = "100%")
    ),
    wellPanel(
      selectInput("pca_text_repel", "show names on figure:", choices = c("TRUE","FALSE"), selected = "FALSE",width = "100%"),
      conditionalPanel("input.pca_text_repel == 'TRUE'", numericInput("pca_text_cex", "names text size:", value = 5, width = "100%")),
    )
  ),
  bsModal(
    "pca_modal", "Additional Parameters", "pca_modal_but", size = "large",
    fluidPage(
      style = "text-align:justify;color:black;background-color:lavender;border-radius:10px;",
      h3("Additional Parameters of 'PCA plot':"), hr(),
      # selectInput("pca_text_repel", "show names on figure:", choices = c("TRUE","FALSE"), selected = "FALSE",width = "100%"),
      # conditionalPanel("input.pca_text_repel == 'TRUE'", numericInput("pca_text_cex", "names text size:", value = 5, width = "100%")),
      textAreaInput("pca_ggText", "ggplot2 codes:", value = 'scale_color_brewer(palette = "Set1")+
        theme(axis.title = element_text(size = 21, face = "bold", color = "black", family = "Times"),
        axis.text = element_text(size = 18, face = "bold", color = "black", family = "Times"),
        text = element_text(size = 18, face = "bold", color = "black", family = "Times"))', width = "100%", rows = 12)
    )
  ),
  column(
    12, style = "padding:0px;",
    fluidRow(
      style = "background-color: rgb(248,249,250); border: 1px solid rgb(218,219,220); padding: 5px; margin:5px; border-radius: 15px;",
      column(
        4, style = "text-align:center;border-right: 2px solid white;",
        # strong("PCA Example", style = "font-size: 20px"),
        tags$img(src = "images/pca_demo.png",
                 width = "100%")
      ),
      column(
        8, style = "text-align:justify;",
        h3("What is Principal component analysis (PCA) ?"),
        p("Principal component analysis (PCA) is a mathematical algorithm that reduces the dimensionality
          of the data while retaining most of the variation in the data set1. It accomplishes this reduction
          by identifying directions, called principal components, along which the variation in the data is maximal.
          By using a few components, each sample can be represented by relatively few numbers instead of by values
          for thousands of variables. Samples can then be plotted, making it possible to visually assess similarities
          and differences between samples and determine whether samples can be grouped."),
        h3("How to interpret the PCA results ?"),
        p("PCA identifies new variables, the principal components, which are linear combinations of the original variables.
          The first principal component is the direction along which the samples show the largest variation. The second principal
          component is the direction uncorrelated to the first component along which the samples show the largest variation."),
        p(strong("Reference: "), "Ringnér M. What is principal component analysis?. Nat Biotechnol. 2008;26(3):303-304. ",
          a(href = "https://www.nature.com/articles/nbt0308-303", "doi:10.1038/nbt0308-303"))
      )
    )
  ),
  column(
    12, hr(),
    fluidRow(column(3, align = "right", actionLink("pPCA", "<< Previous", style = "font-size: 20px")),
             column(6, align = "center", HTML('<p style = "text-align:center;">Copyright &copy; 2022.Shixue All rights reserved.</p>')),
             column(3, align = "left", actionLink("nPCA", "Next >>", style = "font-size: 20px")))
  )
)
