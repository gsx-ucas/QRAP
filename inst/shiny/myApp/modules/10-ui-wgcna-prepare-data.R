fluidRow(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    id = "create_wgcna_exps", title = "Get Expression Matrix:", width = 4, collapsible = TRUE,
    selectizeInput("filter_wgcna_genes", "What gene will be used:",
                   choices = c("all genes", "differential genes"), width = "100%"),
    conditionalPanel(
      "input.filter_wgcna_genes=='differential genes'",
      div(
        style = "border: 1px dashed black; border-radius:15px; padding:5px; margin-bottom:10px",
        p("We do not recommend filtering genes by differential expression, although we provide this option.",
          tags$a("See WGCNA FAQ...", href = "https://horvath.genetics.ucla.edu/html/CoexpressionNetwork/Rpackages/WGCNA/faq.html", target = "_blank"),
          style = "text-align: justify")
      )
    ),
    conditionalPanel(
      "input.filter_wgcna_genes=='all genes'",
      div(
        style = "border: 1px dashed black; border-radius:15px; padding:5px; margin-bottom:10px",
        p("The options below means filter out genes which in 50% samples it's expression level less than 10 reads. As the ",
          tags$a("WGCNA", href = "https://horvath.genetics.ucla.edu/html/CoexpressionNetwork/Rpackages/WGCNA/faq.html", target = "_blank"),
          "documents recommend.", style = "text-align: justify")
      )
    ),
    uiOutput("wgcna_condition"),
    conditionalPanel(
      "input.filter_wgcna_genes=='differential genes'",
      uiOutput("wgcna_degs")
    ),
    conditionalPanel(
      "input.filter_wgcna_genes=='all genes'",
      numericInput("mini_reads", "Minimum reads to be exceeded:", value = 10, width = "100%"),
      numericInput("sample_prop", "The proportion of samples over minimum reads:", value = 0.5, width = "100%")
    ),

    selectizeInput("wgcna_meta_source", "Generate clinical traits data :",
                   choices = c("generate from designTab", "upload from local"), width = "100%"),
    conditionalPanel(
      "input.wgcna_meta_source == 'upload from local'",
      fileInput("traitfile", label = "Upload traitData", width = "100%"),
      checkboxInput("trait_header", "First row as header ?", value = TRUE, width = "100%"),
      p("*Clinical traits data format should be samples in the rownames, clinical conditions in the column names
        and numeric values in the content of this table.", style = "font-weight: 800; padding-top: 3px; color: orange;")
      ),
    conditionalPanel(
      "input.wgcna_meta_source == 'generate from designTab'",
      p("*Conditions will be used to generate a trait data table, which value 1 means sample belongs to this condition and 0 means not!",
        style = "font-weight: 800; padding-top: 3px; color: orange;")
    ),
    actionButton("get_wgcna_exprs", "Get Expression Data >>", class = "run-button", width = "100%")
  ),
  column(
    8,
    box(
      width = 12, title = "WGCNA Expression Data",
      p("*please note that the expression matrix have been transformed to genes in the column and sample in the row,
        so we only present the first 20 genes in the column."),
      withSpinner(dataTableOutput("wgcna_exprs"))
      ),
    box(
      width = 12, title = "WGCNA Meta Data",
      withSpinner(dataTableOutput("wgcna_meta"))
    )
  ),
  column(
    12,
    hr(),
    fluidRow(column(2), column(3, actionLink("pWGCNA_1", "<< Previous", style = "font-size: 20px")),
             column(4, p("You are in WGCNA data preparation page ...", style = "color: grey; font-size: 20px")),
             column(3, actionLink("nWGCNA_1", "Next >>", style = "font-size: 20px")))
  )
)
