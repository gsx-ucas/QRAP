fluidPage(
  style = "margin-left:10px;margin-right:10px;",
  box(
    title = "Experiment Design Table", width = 7, status = NULL, solidHeader = TRUE,
    # conditionalPanel(
    #   "input.reset_condition == 'Re-upload'",
    #   fileInput("ds_file", "Upload group info tab:",
    #             accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
    #             placeholder = "upload your design tab ...", width = "100%")
    # ),
    withSpinner(DT::dataTableOutput("conditionTab")),
    conditionalPanel(
      "input.reset_condition != 'Re-upload'",
      downloadButton('ConditionTab','Download CSV', class = "btn")
    )
  ),
  box(
    title = "DESeq2 Running Table", width = 5, status = NULL, solidHeader = TRUE, collapsible = TRUE,
    # switchInput("run_cache", "Use Cache", value = F,
    #             onStatus = "success", offStatus = "danger", inline = T,labelWidth = "100px"),
    tags$table(
      style = "width: 100%",
      tags$td(
        # style = "width: 100%",
        prettyRadioButtons(
          inputId = "reset_condition", label = "How to generate design table:",
          choices = c("Auto produce", "Re-upload"), icon = icon("check"),
          status = "info", animation = "jelly", inline = TRUE)
      ),
      tags$td(
        style = "text-align: right",
        switchInput("run_cache", "Use Cache", value = F,
                    onStatus = "success", offStatus = "danger", inline = T,labelWidth = "100px")
      )
    ),
    conditionalPanel(
      "input.reset_condition == 'Auto produce'",
      style = "color: rgb(255, 135, 0); border: 1px dashed grey; padding: 10px; margin-bottom: 10px; border-radius: 10px;",
      strong("Note. Do not change the column name 'condition'")
    ),
    conditionalPanel(
      "input.reset_condition == 'Re-upload'",
      fileInput("ds_file", "Upload group info tab:",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                placeholder = "upload your design tab ...", width = "100%")
    ),
    # prettyRadioButtons(
    #   inputId = "reset_condition", label = "How to generate design table:",
    #   choices = c("Auto produce", "Re-upload"), icon = icon("check"),
    #   status = "info", animation = "jelly", inline = TRUE),
    uiOutput("formula"),
    selectInput(inputId = "trans_method", label = "Transformation functions:", width = "100%",
                choices = c("Regularized log transformation (rlog)" = "rlog", "Variance stabilizing transformation (vst)" = "vst")),
    p("VST is faster with more than 30 samples.", style = "text-align: justify"),
    actionButton("deseq_modal_but", "Additional Parameters for Normalization ...", width = "100%",
                 style = "background-color: rgb(255,255,255);text-align:left;margin-bottom:10px", icon = icon("plus-square")),
    actionButton("runDESeq", "Run DESeq >>", class = "run-button", width = "100%")
  ),
  bsModal(
    "deseq_modal", "Additional Parameters", "deseq_modal_but", size = "large",
    fluidRow(
      column(
        6, style = "text-align:justify;color:black;background-color:lavender;border-radius:10px;border:1px solid black;", br(),
        h5("Additional Parameters of 'DESeq':"), hr(),
        selectInput("deseq_test", "test", choices = c("Wald", "LRT"), width = "100%"),
        selectInput("deseq_fitType", "fitType", choices = c("parametric","local", "mean"), width = "100%"),
        selectInput("deseq_sfType", "sfType", choices = c("ratio", "poscounts", "iterate"), width = "100%"),
        selectInput("deseq_betaPrior", "betaPrior", choices = c("FALSE", "TRUE"), width = "100%"),
        conditionalPanel("input.deseq_test=='LRT'", textInput("deseq_reduced",
                                                              "reduced", placeholder = "~ factors", width = "100%")),
        numericInput("deseq_minReplicatesForReplace", "minReplicatesForReplace", value = 7, width = "100%"),
        selectInput("deseq_modelMatrixType", "modelMatrixType", choices = c("standard", "expanded"), width = "100%"),
        selectInput("deseq_useT", "useT", choices = c("FALSE", "TRUE"), width = "100%"),
        numericInput("deseq_minmu", "minmu", value = 0.5, width = "100%")
      ),
      column(
        6,
        fluidRow(
          column(
            12, style = "text-align:justify;color:black;background-color:papayawhip;border-radius:10px;border:1px solid black;", br(),
            h5("Additional Parameters of 'rlog' or 'vst':"), hr(),
            selectInput("trans_blind", "blind (rlog or vst):", choices = c("TRUE", "FALSE"), width = "100%"),
            numericInput("trans_nsub", "nsub (vst):", value = 1000, width = "100%"),
            selectInput("trans_fitType", "fitType (rlog or vst):", choices = c("parametric","local", "mean"), width = "100%")
          ),
          column(
            12, style = "text-align:justify;color:black;background-color:#BFF7BB;border-radius:10px;border:1px solid black;", br(),
            h5("Additional Parameters of 'batch remove':"), hr(),
            selectInput("batch_methods", "Mehtods to remove batch:", choices = c("NULL", "ComBat", "removeBatchEffect"), width = "100%"),
            conditionalPanel("input.batch_methods != 'NULL'", uiOutput("batch_col"), uiOutput("batch_col2"))
          )
        )
      )
    )
  ),
  box(
    title = "DESeq2 Running Table", width = 12, status = NULL, solidHeader = TRUE, collapsible = TRUE,
    fluidRow(
      style = "background-color: rgb(248,249,250); border: 1px solid rgb(218,219,220); padding: 5px; margin:5px; border-radius: 15px;",
      column(
        width = 6,
        strong("Design Example", style = "font-size: 20px"),
        tags$img(src = "images/design-example.png",
                 width = "100%")
      ),
      column(
        width = 6,
        strong(style = "font-size: 20px", tags$a("The DESeqDataSet: (click to see more)",
                                                 target = "_blank", href = "http://www.bioconductor.org/packages/release/bioc/vignettes/DESeq2/inst/doc/DESeq2.html#the-deseqdataset")),
        p("A DESeqDataSet object must have an associated design formula. The design formula expresses the variables which will be used
          in modeling. The formula should be a tilde (~) followed by the variables with plus signs between them (it will be coerced into
          an formula if it is not already). The design can be changed later, however then all differential analysis steps should be repeated, as
          the design formula is used to estimate the dispersions and to estimate the log2 fold changes of the model.", style="text-align: justify;"),
        strong(style = "font-size: 20px", tags$a("Multi-factor designs: (click to see more)", target = "_blank",
                                                 href = "http://www.bioconductor.org/packages/release/bioc/vignettes/DESeq2/inst/doc/DESeq2.html#multi-factor-designs")),
        p("Experiments with more than one factor influencing the counts can be analyzed
          using design formula that include the additional variables.", style="text-align: justify;"),
        p("By adding variables to the design, one can control for additional variation in the counts. For example, if the condition samples are balanced across experimental batches,
          by including the batch factor to the design, one can increase the sensitivity for finding differences due to condition.", style="text-align: justify;")
      )
    )
  ),
  # column(
  #   12,
  #   fluidRow(
  #     style = "background-color: rgb(248,249,250); border: 1px solid rgb(218,219,220); padding: 5px; margin:5px; border-radius: 15px;",
  #     column(
  #       width = 6,
  #       strong("Design Example", style = "font-size: 20px"),
  #       tags$img(src = "images/design-example.png",
  #                width = "100%")
  #     ),
  #     column(
  #       width = 6,
  #       strong(style = "font-size: 20px", tags$a("The DESeqDataSet: (click to see more)",
  #                                                target = "_blank", href = "http://www.bioconductor.org/packages/release/bioc/vignettes/DESeq2/inst/doc/DESeq2.html#the-deseqdataset")),
  #       p("A DESeqDataSet object must have an associated design formula. The design formula expresses the variables which will be used
  #         in modeling. The formula should be a tilde (~) followed by the variables with plus signs between them (it will be coerced into
  #         an formula if it is not already). The design can be changed later, however then all differential analysis steps should be repeated, as
  #         the design formula is used to estimate the dispersions and to estimate the log2 fold changes of the model.", style="text-align: justify;"),
  #       strong(style = "font-size: 20px", tags$a("Multi-factor designs: (click to see more)", target = "_blank",
  #                                                href = "http://www.bioconductor.org/packages/release/bioc/vignettes/DESeq2/inst/doc/DESeq2.html#multi-factor-designs")),
  #       p("Experiments with more than one factor influencing the counts can be analyzed
  #         using design formula that include the additional variables.", style="text-align: justify;"),
  #       p("By adding variables to the design, one can control for additional variation in the counts. For example, if the condition samples are balanced across experimental batches,
  #         by including the batch factor to the design, one can increase the sensitivity for finding differences due to condition.", style="text-align: justify;")
  #     )
  #   )
  # ),
  column(
    12,
    hr(),
    fluidRow(
      style = "margin-bottom:20px",
      column(3, align = "right", actionLink("pCondition", "<< Previous", style = "font-size: 20px")),
      column(6, align = "center"),
      column(3, align = "left", actionLink("nCondition", "Next >>", style = "font-size: 20px"))
    )
  )
)
