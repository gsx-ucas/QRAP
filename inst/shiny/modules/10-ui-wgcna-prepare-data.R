fluidRow(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    id = "create_wgcna_exps", title = "Get Expression Matrix:", width = 4, collapsible = TRUE, solidHeader = TRUE,
    selectizeInput("filter_wgcna_genes", "What gene will be used:",
                   choices = c("all genes", "differential genes"), width = "100%"),
    conditionalPanel(
      "input.filter_wgcna_genes=='differential genes'",
      div(
        style = "border: 1px dashed black; border-radius:15px; padding:5px; margin-bottom:10px",
        p("WGCNA do not recommend filtering genes by differential expression, but we provide this option for special need.",
          tags$a("See WGCNA FAQ...", href = "https://horvath.genetics.ucla.edu/html/CoexpressionNetwork/Rpackages/WGCNA/faq.html", target = "_blank"),
          style = "text-align: justify")
      )
    ),
    conditionalPanel(
      "input.filter_wgcna_genes=='all genes'",
      div(
        style = "border: 1px dashed black; border-radius:15px; padding:5px; margin-bottom:10px",
        p("The options below means filter out genes which expression level less than 10 reads in 50% samples. As the ",
          tags$a("WGCNA", href = "https://horvath.genetics.ucla.edu/html/CoexpressionNetwork/Rpackages/WGCNA/faq.html", target = "_blank"),
          "documents recommend.", style = "text-align: justify")
      )
    ),
    conditionalPanel(
      "input.filter_wgcna_genes=='differential genes'",
      uiOutput("wgcna_degs")
    ),
    conditionalPanel(
      "input.filter_wgcna_genes=='all genes'",
      numericInput("mini_reads", "Minimum reads to be exceeded:", value = 10, width = "100%"),
      numericInput("sample_prop", "The proportion of samples over minimum reads:", value = 0.5, width = "100%")
    ),
    uiOutput("wgcna_condition"),
    uiOutput("wgcna_chcol"),
    uiOutput("wgcna_nucol"),
    actionButton("get_wgcna_exprs", "Get Expression Data >>", class = "run-button", width = "100%")
  ),
  column(
    8,
    tabsetPanel(
      tabPanel(
        "WGCNA Meta Data", style = "padding: 10px",
        withSpinner(dataTableOutput("wgcna_meta"))
      ),
      tabPanel(
        "WGCNA Expression Data", style = "padding: 10px",
        uiOutput("wgcna_warning"),
        withSpinner(dataTableOutput("wgcna_exprs"))
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
    fluidRow(column(3, align = "right", actionLink("pWGCNA_1", "<< Previous", style = "font-size: 20px")),
             column(6, align = "center", HTML('<p style = "text-align:center;">Copyright &copy; 2022.Shixue All rights reserved.</p>')),
             column(3, align = "left", actionLink("nWGCNA_1", "Next >>", style = "font-size: 20px")))
  )
)
