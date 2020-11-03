fluidPage(
  style = "margin-left:200px;margin-right:200px;",
  column(
    3,
    br(),
    tags$nav(
      id = "home_nav", class="affix",
      # style = "border-left: 1px solid black;",
      tags$ul(
        class="nav",
        tags$li(
          class = "",
          tags$a("Introduction", href="#introduction"),
          tags$ul(
            class = "nav",
            tags$li(tags$a("Abstract", href="#abstract")),
            tags$li(tags$a("Pipeline", href="#pipeline")),
            tags$li(tags$a("Features", href="#features"))
          )
        ),
        tags$li(
          class = "",
          tags$a("User Guide", href="#user-guide"),
          tags$ul(
            class = "nav",
            tags$li(tags$a("Installing", href="#install")),
            tags$li(tags$a("Data Input", href="#data-input")),
            tags$li(tags$a("Design & Run", href="#design-run")),
            tags$li(tags$a("Quality Assessment", href="#data-quality")),
            tags$li(tags$a("Differential Analysis", href="#diff-analysis")),
            tags$li(tags$a("Biological Patterns", href="#bioc-pattern")),
            tags$li(tags$a("Functional Analysis", href="#function-analysis")),
            tags$li(tags$a("Network Analysis", href="#network-analysis")),
            tags$li(tags$a("Get Help", href="#get-help"))
          )
        )
      )
    )
  ),
  column(
    9,
    h1("Introduction", id = "introduction", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    hr(style = "border-top: 1px solid orange;"),
    
    h3("Abstract", id = "abstract", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    p("We propose a web application (RNASEQ-DASH) for exploring and analyzing RNA sequencing data."),
    
    h3("Pipeline", id = "pipeline", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    p("We propose a web application (RNASEQ-DASH) for exploring and analyzing RNA sequencing data."),
    
    h3("Features", id = "features", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    p("We propose a web application (RNASEQ-DASH) for exploring and analyzing RNA sequencing data."),
    
    h1("Data Input", id = "data-input", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    hr(style = "border-top: 1px solid orange;"),

    h3("Installing", id = "install", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    p("We propose a web application (RNASEQ-DASH) for exploring and analyzing RNA sequencing data."),
    
    h3("Data Input", id = "data-input", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    p("We propose a web application (RNASEQ-DASH) for exploring and analyzing RNA sequencing data."),
    
    h3("Design & Run", id = "design-run", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    p("We propose a web application (RNASEQ-DASH) for exploring and analyzing RNA sequencing data."),
    
    h3("Quality Assessment", id = "data-quality", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    p("We propose a web application (RNASEQ-DASH) for exploring and analyzing RNA sequencing data."),
    
    h3("Biological Patterns", id = "bioc-pattern", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    p("We propose a web application (RNASEQ-DASH) for exploring and analyzing RNA sequencing data."),
    
    h3("Functional Analysis", id = "function-analysis", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    p("We propose a web application (RNASEQ-DASH) for exploring and analyzing RNA sequencing data."),
    
    h3("Network Analysis", id = "network-analysis", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    p("We propose a web application (RNASEQ-DASH) for exploring and analyzing RNA sequencing data."),
    
    h3("Get Help", id = "get-help", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    p("We propose a web application (RNASEQ-DASH) for exploring and analyzing RNA sequencing data.")
    
  )
)