fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  column(
    width = 4, style = "padding: 0px; margin:0px; border-radius: 10px; background-color: transparent",
    fluidRow(
      # uiOutput("search_geo_card"),
      uiOutput("preview_card"),
      uiOutput("filter_data_card")
    )
  ),
  column(
    width = 6, style = "padding: 0px; margin:0px",
    uiOutput("geo_search_res_card"),
    uiOutput("preview_mat_card")
  ),
  uiOutput("info_boxs"),
  uiOutput("intro_start"),
  column(
    12,
    hr(),
    fluidRow(
      column(3, align = "right", actionLink("pStart", "<< Home", style = "font-size: 20px")),
      column(6, align = "center", HTML('<p style = "text-align:center;">Copyright &copy; 2022.Shixue All rights reserved.</p>')),
      column(3, align = "left", actionLink("nStart", "Next >>", style = "font-size: 20px"))
    ),
  )
)
