

output$trait <- renderUI({
  pickerInput(
    inputId = "trait", label = "Select interested trait:",
    choices = colnames(traitDataTab()), selected = colnames(traitDataTab())[1],
    multiple = F, width = "100%", options = list(`live-search` = TRUE, size = 5)
  )
})

output$wgcna_scatter_module <- renderUI({
  pickerInput(
    inputId = "wgcna_scatter_module", label = "Select interested module:",
    choices = moduleColors() %>% unique, selected = (moduleColors() %>% unique)[1],
    multiple = F, width = "100%", options = list(`live-search` = TRUE, size = 5)
  )
})

verboseScatter <- eventReactive(input$plot_wgcna_scatter, {
  trait_condition = as.data.frame(traitDataTab()[, input$trait])
  names(trait_condition) = input$trait

  MEs0 = moduleEigengenes(datExpr(), moduleColors())$eigengenes
  MEs = orderMEs(MEs0)

  modNames = substring(names(MEs), 3)

  nSamples <- dim(datExpr())[1]
  geneModuleMembership = as.data.frame(cor(datExpr(), MEs, use = "p"))
  MMPvalue = as.data.frame(corPvalueStudent( as.matrix(geneModuleMembership), nSamples))

  names(geneModuleMembership) = paste("MM", modNames, sep="");
  names(MMPvalue) = paste("p.MM", modNames, sep="");

  geneTraitSignificance = as.data.frame(cor(datExpr(), trait_condition, use = "p"));
  GSPvalue = as.data.frame(corPvalueStudent(as.matrix(geneTraitSignificance), nSamples));

  names(geneTraitSignificance) = paste("GS.", names(trait_condition), sep="");
  names(GSPvalue) = paste("p.GS.", names(trait_condition), sep="");

  module = input$wgcna_scatter_module
  column = match(module, modNames);
  moduleGenes = moduleColors() == module;

  par(mar=c(5,5,5,5))
  verboseScatterplot(abs(geneModuleMembership[moduleGenes, column]),
                     abs(geneTraitSignificance[moduleGenes, 1]),
                     xlab = paste("Module Membership in", module, "module"),
                     ylab = paste("Gene significance for ", input$trait),
                     main = paste("Module membership vs. gene significance\n"),
                     cex = input$wgcna_scatter_cex, cex.main = input$wgcna_scatter_main,
                     cex.lab = input$wgcna_scatter_lab, cex.axis = input$wgcna_scatter_axis, col = module)
})

output$verboseScatter <- renderPlot({
  verboseScatter()
})

output$verboseScatterUI <- renderUI({
  withSpinner(plotOutput("verboseScatter", width = paste0(input$wgcna_scatter_width, "%"), height = paste0(input$wgcna_scatter_height, "px")))
})

output$verboseScatter_Pdf <- downloadHandler(
  filename = function()  {paste0("WGCNA_GS-MM-verboseScatterplot",".pdf")},
  content = function(file) {
    pdf(file, width = input$verboseScatter_width, height = input$verboseScatter_height)

    trait_condition = as.data.frame(traitDataTab()[, input$trait])
    names(trait_condition) = input$trait

    MEs0 = moduleEigengenes(datExpr(), moduleColors())$eigengenes
    MEs = orderMEs(MEs0)

    modNames = substring(names(MEs), 3)

    nSamples <- dim(datExpr())[1]
    geneModuleMembership = as.data.frame(cor(datExpr(), MEs, use = "p"))
    MMPvalue = as.data.frame(corPvalueStudent( as.matrix(geneModuleMembership), nSamples))

    names(geneModuleMembership) = paste("MM", modNames, sep="");
    names(MMPvalue) = paste("p.MM", modNames, sep="");

    geneTraitSignificance = as.data.frame(cor(datExpr(), trait_condition, use = "p"));
    GSPvalue = as.data.frame(corPvalueStudent(as.matrix(geneTraitSignificance), nSamples));

    names(geneTraitSignificance) = paste("GS.", names(trait_condition), sep="");
    names(GSPvalue) = paste("p.GS.", names(trait_condition), sep="");

    module = input$wgcna_scatter_module
    column = match(module, modNames);
    moduleGenes = moduleColors() == module;

    par(mar=c(5,5,5,5))
    verboseScatterplot(abs(geneModuleMembership[moduleGenes, column]),
                       abs(geneTraitSignificance[moduleGenes, 1]),
                       xlab = paste("Module Membership in", module, "module"),
                       ylab = "Gene significance for body weight",
                       main = paste("Module membership vs. gene significance\n"),
                       cex = input$wgcna_scatter_cex, cex.main = input$wgcna_scatter_main,
                       cex.lab = input$wgcna_scatter_lab, cex.axis = input$wgcna_scatter_axis, col = module)

    dev.off()
  }
)

