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
            tags$li(tags$a("Features", href="#features"))
            # tags$li(tags$a("Features", href="#features"))
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
    p("Here we developed QRseq, a R shiny application can easily launched from local web browser for analyzing sequenced or
      published RNA-seq data. QRseq allows user to upload RNA-seq data from local or to input some keyworks or an accession
      number of GEO DataSets within the app to start their analysis. This application start from data input, followed by preprocessing
      data by filtering low expressed genes and poorly reproducible samples, correcting batch effects, normalizing and transforming data,
      identifying differential expressed genes and other biological patterns, exploring the enrichment of functions, analyzing and visualizing
      the protein to protein networks or gene regulation networks. QRseq provide a clear analysis flow and an user friendly GUI interface but
      keep most important parameter of involved functions, which suite for both non-programing experience researchers and expert bioinformatic
      researchers. User can accomplish a standard RNA-seq analysis in hours depend on the size of their dataset and requires using QRseq.",
      style = "text-align:justify;font-family:'Times New Roman', Times, serif;"),

    h3("Features", id = "features", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    p(strong("The main Features was showed below:")),
    tags$img(src = "images/workflow.tiff",width = "70%"),

    # h3("Features", id = "features", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    # p("We propose a web application (QRseq) for exploring and analyzing RNA sequencing data."),

    h1("User Guide", id = "data-input", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    hr(style = "border-top: 1px solid orange;"),

    h3("Installing", id = "install", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    p("We propose a web application (QRseq) for exploring and analyzing RNA sequencing data."),

    h3("Data Input", id = "data-input", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    p("We propose a web application (QRseq) for exploring and analyzing RNA sequencing data."),

    h3("Design & Run", id = "design-run", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    p("We propose a web application (QRseq) for exploring and analyzing RNA sequencing data."),

    h3("Quality Assessment", id = "data-quality", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    p("We propose a web application (QRseq) for exploring and analyzing RNA sequencing data."),

    h3("Biological Patterns", id = "bioc-pattern", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    p("We propose a web application (QRseq) for exploring and analyzing RNA sequencing data."),

    h3("Functional Analysis", id = "function-analysis", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    p("We propose a web application (QRseq) for exploring and analyzing RNA sequencing data."),

    h3("Network Analysis", id = "network-analysis", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    p("We propose a web application (QRseq) for exploring and analyzing RNA sequencing data."),

    h3("Get Help", id = "get-help", style = "width:100%; text-align:left;font-family:'Times New Roman', Times, serif;"),
    p("We propose a web application (QRseq) for exploring and analyzing RNA sequencing data.")

  )
)
