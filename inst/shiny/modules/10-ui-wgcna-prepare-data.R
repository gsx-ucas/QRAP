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
        4, style = "text-align:center;border-right: 2px solid white; padding-top:15px",
        tags$img(src = "images/demo/wgcna_table.png",
                 width = "100%")
      ),
      column(
        8, style = "text-align:justify;",
        h3("What is weighted correlation network analysis (WGCNA) ?"),
        p("weighted correlation network analysis (WGCNA) can be used for finding modules of highly correlated genes, 
          for relating modules to external sample traits and summarizing modules using an intramodular hub gene.
          WGCNA is a friendly method for analyzing clinical related sequencing data. We introduce WGCNA package to 
          our application for performing weighted correlation network analysis. We use the raw count matrix to filter 
          out low expression genes and use the variance stabilizing transformation (VST) normalized value as input matrix. 
          The clinical traits data can be upload when the experimental design session or transformed from the experimental condition.
          If the clinical traits data is a string, we will classify it according to the fields it contains, and if it is a number, 
          we will keep its value as the feature vector.")
        # h3("How to interpret the SSD analysis results ?"),
        # p("SSDA can elucidate samples distance in the high-dimensional space. In RNA-seq data, each gene is a dimension,
        #   so the data has tens of thousands of dimensions. SSDA uses Euclidean distance to elucidate samples distance in the
        #   high-dimensional space, which helps to understand the relationship of samples across exprimental conditions or sample replicates.
        #   The heatmap clusters samples with similar distances, which makes the results easier to interpret.")
      )
    )
  ),
  column(
    12,
    hr(),
    fluidRow(
      style = "margin-bottom:20px",
      column(3, align = "right", actionLink("pWGCNA_1", "<< Previous", style = "font-size: 20px")),
      column(6, align = "center"),
      column(3, align = "left", actionLink("nWGCNA_1", "Next >>", style = "font-size: 20px"))
    )
  )
)
