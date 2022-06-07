fluidPage(
  style = "margin-left:10px;margin-right:10px;",
  box(
    title = "Hierarchical clustering Parameters", width = 4, status = NULL, solidHeader = TRUE,
    uiOutput("hiera_samples"),
    uiOutput("hiera_ancol"),
    numericInput("hiera_topn", "Top N variance genes:", value = 500, width = "100%"),
    selectInput("hiera_dist_method", "The distance measure:", width = "100%",
                choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")),
    selectInput("hiera_hclust_method", "The agglomeration measure:", width = "100%",
                choices = c("complete", "ward.D", "ward.D2", "single","average", "mcquitty", "median", "centroid")),
    textInput("hiera_color", "color:", value = "navy,white,red", placeholder = "eg. navy,white,red or #000080,#FFFFFF,#FF0000", width = "100%"),
    actionButton("hiera_modal_but", "Additional Parameters for Visualization ...", width = "100%",
                 style = "background-color: rgb(255,255,255);text-align:left;margin-bottom:10px", icon = icon("plus-square")),
    actionButton("plot_hiera", "Plotting HeatMap", width = "100%", class = "plot-button")
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
            tags$h4("Hierarchical Clustering Heatmap:")
          ),
          column(
            6, align = "right", style = "padding-top:5px;",
            dropdownButton(
              numericInput('hiera_width', 'Figure Width (cm):', min = 1, max = 20, value = 7, width = "100%"),
              numericInput('hiera_height', 'Figure Height (cm):', min = 1, max = 20, value = 5, width = "100%"),
              downloadButton('hiera_Pdf','Download .pdf', class = "btn", width = "100%"),
              circle = FALSE, status = "danger", size = "sm", icon = icon("save"), width = "200px", right = TRUE,
              tooltip = tooltipOptions(title = "Click to download figures !")
            )
          )
        )
      ),
      uiOutput("hiera_plotUI")
    )
  ),
  column(
    2,
    wellPanel(
      sliderInput("hiera_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
      sliderInput("hiera_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 525, step = 2, width = "100%")
    ),
    wellPanel(
      checkboxInput("hiera_cluster_rows", "Clustering genes?", value = TRUE, width = "100%"),
      checkboxInput("hiera_colname", "Showing column names?", value = FALSE, width = "100%"),
      checkboxInput("hiera_annotation", "Showing column annotation?", value = TRUE, width = "100%")
    )
  ),
  bsModal(
    "hiera_modal", "Additional Parameters", "hiera_modal_but", size = "large",
    fluidPage(
      style = "text-align:justify;color:black;background-color:lavender;border-radius:10px;",
      h3("Additional Parameters of 'HeatMap plot':"), hr(),
      numericInput("hiera_fontsize", "Fontsize for legends:", value = 15, width = "100%"),
      numericInput("hiera_fontsize_col", "Fontsize  of column names:", value = 15, width = "100%"),
      selectInput("hiera_angle", "Angle of column names:", choices = c('0', '45', '90', '270', '315'), selected = '315', width = "100%"),
      numericInput("hiera_cutree", "Number of clusters the rows are divided into:", value = 1, width = "100%"),
      numericInput("hiera_cutree_cols", "Number of clusters the columns are divided into:", value = 1, width = "100%"),
      numericInput("hiera_treeheight_row", "The height of a tree for rows:", value = 20, width = "100%"),
      numericInput("hiera_treeheight_col", "the height of a tree for columns:", value = 20, width = "100%"),

    )
  ),
  column(
    12, style = "padding:0px;",
    fluidRow(
      style = "background-color: rgb(248,249,250); border: 1px solid rgb(218,219,220); padding: 5px; margin:5px; border-radius: 15px;",
      column(
        4, style = "text-align:center;border-right: 2px solid white;",
        # strong("PCA Example", style = "font-size: 20px"),
        tags$img(src = "images/hca_demo.png",
                 width = "100%")
      ),
      column(
        8, style = "text-align:justify;",
        h3("What is Hierarchical cluster analysis (HCA) ?"),
        p("Hierarchical clustering analysis is an algorithm that groups similar objects into groups called
          clusters. The endpoint is a set of clusters, where each cluster is distinct from each other cluster,
          and the objects within each cluster are broadly similar to each other. In RNA-seq, the objects can
          be samples or genes. HCA is often combined with heatmap to show the cluster of samples in the column
          and genes in the rows"),
        h3("How to interpret the HCA results ?"),
        p("The agglomeration algorithm of hierarchical clustering combines the two most similar data points (genes or samples)
          of all data points by calculating the similarity between the two types of data points, and iterates this process
          repeatedly. To put it simply, the agglomeration algorithm of hierarchical clustering is to determine the similarity
          between each category of data points and all data points by calculating the distance between them. The smaller the
          distance, the higher the similarity. And the two nearest data points or categories are combined to generate a cluster tree."),
        p(strong("Reference: "), "Naomi Altman & Martin Krzywinski (2017) Clustering, Nature Methods 14, 545–546。 ",
          a(href = "https://www.nature.com/articles/nmeth.4299#citeas", "https://doi.org/10.1038/nmeth.4299"))
      )
    )
  ),
  column(
    12, hr(),
    fluidRow(
      style = "margin-bottom:20px",
      column(3, align = "right", actionLink("pHiera", "<< Previous", style = "font-size: 20px")),
      column(6, align = "center"),
      column(3, align = "left", actionLink("nHiera", "Next >>", style = "font-size: 20px"))
    )
  )
)
