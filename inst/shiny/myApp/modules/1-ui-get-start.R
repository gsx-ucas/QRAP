fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  column(
    width = 4, style = "padding: 0px; margin:0px; border-radius: 10px; background-color: transparent",
    fluidRow(
      uiOutput("search_geo_card"),
      uiOutput("preview_card"),
      uiOutput("filter_data_card")
    )
  ),
  column(
    width = 8, style = "padding: 0px; margin:0px",
    uiOutput("geo_search_res_card"),
    uiOutput("preview_mat_card")
  ),
  # column(
  #   4,
  #   style = "padding: 0px; margin:0px",
  #   uiOutput("search_geo_card"),
  #   uiOutput("preview_card"),
  #   uiOutput("filter_data_card")
  # ),
  # column(
  #   8,
  #   style = "padding: 0px; margin:0px",
  #   uiOutput("geo_search_res_card"),
  #   uiOutput("preview_mat_card")
  # ),
  column(
    12,
    hr(),
    fluidRow(column(2), column(3, actionLink("pStart", "<< Home", style = "font-size: 20px")),
             column(4, p("You are in get start page ...", style = "color: grey; font-size: 20px")),
             column(3, actionLink("nStart", "Next >>", style = "font-size: 20px"))),
    br()
  )
)
