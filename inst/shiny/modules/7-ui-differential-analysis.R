fluidPage(
  style = "margin-left: 10px; margin-right:10px;",
  box(
    title = "Detecting Differentially Expressed Genes:", id = "dea_tab", width = 12, collapsible = TRUE, solidHeader = TRUE,
    fluidRow(
      column(3, uiOutput('dea_ref')),
      column(3, uiOutput('dea_group')),
      column(3, numericInput("dea_pval", "De Genes Pvalue Threashold:", value = 0.05, width = "100%")),
      column(3,numericInput("dea_lfc", "Abs log2FoldChange Threashold:", value = 1, width = "100%")),
      column(12, align = 'center', actionButton("get_DEGs", "Get Differentially Expressed Genes",
                                                style = "height:40px;font-size:18px", class = "run-button", width = "300px"))
    )
  ),
  column(12, align = "center", actionLink("showDEVis", label = NULL, icon = icon("angle-double-down", style = "font-size: 20px;"))),
  conditionalPanel(
    "input.get_DEGs | input.showDEVis",
    tabsetPanel(
      tabPanel(
        "DEGs Visualization",
        fluidPage(
          style = "padding-top: 5px; padding-left: 0px; padding-right: 0px;margin-left: 0px; margin-right: 0px;",
          box(
            title = "Plot parameters:", width = 4, status = NULL, solidHeader = TRUE,
            # radioButtons("dePlot", "Visualize methods:", choices = c("Volcano", "Heatmap", "Venn", "BarPlot"), inline = T, width = "100%"),
            prettyRadioButtons(inputId = "dePlot", label = "Visualize methods:",icon = icon("check"), status = "info",
                               choices = c("Volcano", "Heatmap", "Venn", "BarPlot"), animation = "jelly", inline = TRUE),
            uiOutput("dea_genes"),
            conditionalPanel(
              "input.dePlot=='Volcano'",
              numericInput("show_topn", "Show top N significant genes:", value = 10, min = 0, width = "100%"),
              numericInput("vol_text_size", "Label text size:", value = 5, min = 0, max = 15, width = "100%"),
              # numericRangeInput("vol_xlimits","xlim range:", value = c(-20, 20), width = "100%"),
              # numericInput("vol_ylimit","ylim range:", value = 50, min = 0,  width = "100%"),
              numericInput("vol_size", "Point size:", value = 1, min = 0, max = 5, width = "100%")
            ),
            conditionalPanel(
              "input.dePlot=='Venn'",
              selectInput("venn_genes", "Genes used to plot:", choices = c("Both", "Up Regulated Genes", "Down Regulated Genes"), width = "100%"),
              # selectInput("venn_percentage", "Show percentage:", choices = c("TRUE", "FALSE"), width = "100%"),
              numericInput("venn_lsize","Size of intersection labels:", value = 5,  min = 0, max = 10, width = "100%"),
              numericInput("venn_nsize","Size of set names:", value = 5,  min = 0, max = 10, width = "100%")
            ),
            conditionalPanel(
              "input.dePlot=='Heatmap'",
              uiOutput("deg_hiera_ancol"),
              numericInput("deheat_fontsize", "Fontsize:", value = 15, width = "100%"),
              textInput("deheat_color", "color:", value = "navy,white,red",  width = "100%")
            ),
            conditionalPanel(
              "input.dePlot=='BarPlot'",
              selectInput("debar_number", "Show numbers on barplot:", c("yes", "no"), width = "100%"),
              selectInput("debar_split", "Whether split up and down genes:", choices = c("yes", "no"), width = "100%"),
              numericInput("debar_text_fontsize", "Label text size:", value = 5, width = "100%")
            ),
            actionButton("diff_modal_but", "Additional Parameters for Visualization ...", width = "100%",
                         style = "background-color: rgb(255,255,255);text-align:left;margin-bottom:10px", icon = icon("plus-square")),
            conditionalPanel(
              "input.dePlot=='Volcano'",
              actionButton("plot_volcano", "Plotting", class = "plot-button", width = "100%")
            ),
            conditionalPanel(
              "input.dePlot=='Venn'",
              actionButton("plot_venn", "Plotting", class = "plot-button", width = "100%")
            ),
            conditionalPanel(
              "input.dePlot=='Heatmap'",
              actionButton("plot_deheatmap", "Plotting", class = "plot-button", width = "100%")
            ),
            conditionalPanel(
              "input.dePlot=='BarPlot'",
              actionButton("plot_debar", "Plotting", class = "plot-button", width = "100%")
            )
          ),
          column(
            6,
            uiOutput("dea_plotUI")
          ),
          column(
            2,
            wellPanel(
              sliderInput("dea_plot_width", "Figure Width (%):", min = 50, max = 100, value = 100, step = 2, width = "100%"),
              sliderInput("dea_plot_height", "Figure Height (px):", min = 200, max = 1000, value = 440, step = 10, width = "100%")
            )
          ),
          bsModal(
            "diff_exprs_modal",  "Additional Parameters", "diff_modal_but", size = "large",
            fluidPage(
              style = "text-align:justify;color:black;background-color:lavender;border-radius:10px;border:1px solid black;",
              conditionalPanel(
                "input.dePlot=='Volcano'",
                h2("Additional Parameters of 'Volcano Plot':"), hr(),
                numericRangeInput("vol_threasholds","P-value and Abs log2FoldChange:", value = c(0.05, 1), width = "100%"),
                numericInput("vol_alpha", "Point alpha:", value = 0.8, min = 0, max = 1, width = "100%"),
                textAreaInput("deVol_ggText", "ggplot2 codes:", value = 'scale_color_brewer(palette = "Set1")+
                  theme(axis.title = element_text(size = 21, face = "bold", color = "black", family = "Times"),
                  axis.text = element_text(size = 18, face = "bold", color = "black", family = "Times"),
                  text = element_text(size = 18, face = "bold", color = "black", family = "Times"))', width = "100%", rows = 12)
              ),
              conditionalPanel(
                "input.dePlot=='Heatmap'",
                h2("Additional Parameters of 'DE Heatmap':"), hr(),
                checkboxInput("deheat_row", "Specifying if genes (row side) should be clustered.", value = TRUE, width = "100%"),
                checkboxInput("deheat_cols", "Specifying if samples (column side) should be clustered.", value = FALSE, width = "100%"),
                checkboxInput("deheat_colname", "Specifying if column names are be shown.", value = FALSE, width = "100%"),
                checkboxInput("deheat_colanno", "Specifying if column annotations are be shown.", value = TRUE, width = "100%"),
                numericInput("deheat_rowh","The height of a tree for rows:", value = 20,  min = 0, max = 50, width = "100%"),
                numericInput("deheat_colh","The height of a tree for columns:", value = 20,  min = 0, max = 50, width = "100%"),
                selectInput("deheat_angle", "Column names angle (if showed):", choices = c('0', '45', '90', '270', '315'), selected = '315', width = "100%")
              ),
              conditionalPanel(
                "input.dePlot=='Venn'",
                h2("Additional Parameters of 'ggplots Plot':"), hr(),
                textAreaInput("deVenn_ggText", "ggplot2 codes:", value = 'scale_fill_brewer(palette = "Set2")+xlim(-3, 3)', width = "100%", rows = 12)
              ),
              conditionalPanel(
                "input.dePlot=='BarPlot'",
                h2("Additional Parameters of 'ggplots Plot':"), hr(),
                textAreaInput("deBar_ggText", "ggplot2 codes:", value = 'scale_fill_brewer(palette = "Set1")+
                  theme(axis.title = element_text(size = 21, face = "bold", color = "black", family = "Times"),
                  axis.text = element_text(size = 18, face = "bold", color = "black", family = "Times"),
                  text = element_text(size = 18, face = "bold", color = "black", family = "Times"))', width = "100%", rows = 12)
              )
            )
          )
        )
      ),
      tabPanel(
        "DEGs DataTable Detials",
        fluidPage(
          style = "padding-top: 5px; padding-left: 0px; padding-right: 0px;margin-left: 0px; margin-right: 0px;",
          column(
            12,
            uiOutput("DeResult_Groups"),
            withSpinner(dataTableOutput("DeGeneTab")),
            downloadButton('DeGeneTab_CSV','Download .csv', class = "btn", width = "20%")
          )
        )
      )
    )
  ),
  column(
    12, style = "padding:0px;",
    fluidRow(
      style = "background-color: rgb(248,249,250); border: 1px solid rgb(218,219,220); padding: 5px; margin:5px; border-radius: 15px;",
      column(
        4, style = "text-align:center;border-right: 2px solid white; padding-top:15px",
        tags$img(src = "images/demo/volcano_plot.png",
                 width = "100%")
      ),
      column(
        8, style = "text-align:justify;",
        h3("What is differential expression analysis (DEA) ?"),
        p("The most important application of RNA-seq is to detect differentially expressed genes between variance conditions or groups.
          Differential expression analysis (DEA) requires that gene expression values should be compared among samples.
          We chose the DESeq2 package to perform the expression normalization to adjust abundance quantities in order to 
          account for differences in read depth across the samples. After that, the function ‘results’ of DESeq2 wase used 
          for calculating the foldchange of gene expression levels and P-values across experimental and control condition. 
          Most commonly used standard to define differentially expressed gene (DEG) is |log2FoldChange| > 1 and P-value < 0.05, 
          but we also provide the options for user’s custom defined standard to filter DEGs. We provide five approaches to visualize the 
          DEGs, include volcano plot, MA-plot, bar-plot, heatmap, venn diagram."),
        h3("How to perform differential expression analysis ?"),
        p("Differential expression analysis with DESeq2 involves multiple steps as displayed in the flowchart below in blue. Briefly, DESeq2 
          will model the raw counts, using normalization factors (size factors) to account for differences in library depth. Then, it will 
          estimate the gene-wise dispersions and shrink these estimates to generate more accurate estimates of dispersion to model the counts. 
          Finally, DESeq2 will fit the negative binomial model and perform hypothesis testing using the Wald test or Likelihood Ratio Test.")
      )
    )
  ),
  column(
    12, hr(),
    fluidRow(
      style = "margin-bottom:20px",
      column(3, align = "right", actionLink("pDEA", "<< Previous", style = "font-size: 20px")),
      column(6, align = "center"
             # HTML('<p style = "text-align:center;">Copyright &copy; 2022.Gou Shixue All rights reserved.</p>')
      ),
      column(3, align = "left", actionLink("nDEA", "Next >>", style = "font-size: 20px"))
    )
  )
)
