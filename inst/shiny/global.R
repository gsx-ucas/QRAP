# Depends <- c("DT", "venn", "shiny", "limma", "DESeq2", "gplots", "stringr", "fdrtool",
#              "ggplot2", "ggrepel", "shinyjs", "reshape2", "pheatmap", "enrichplot", "ggbeeswarm",
#              "geneplotter", "RColorBrewer", "shinyWidgets", "clusterProfiler", "shinycssloaders",
#              "sva", "plyr", "dplyr", "Rmisc", "psych", "WGCNA", "doRNG", "igraph", "GENIE3", "plotly",
#              "shinyBS", "STRINGdb", "GEOquery", "networkD3", "DEGreport", "gprofiler2", "shinyalert",
#              "ReactomePA", "genefilter", "visNetwork", "edgebundleR", "rentrez", "shinydashboard",
#              "ggplotify", "ggnewscale", "pathview"
#              )
#
# for (i in Depends) {
#   if (!requireNamespace(i)) {
#     BiocManager::install(i)
#     # print("Not install")
#   }
# }

# library(QRAP)
# library(shiny)
# library(DT)
# library(gplots)
# library(venn)
# library(limma)
# library(tidyr)
# library(DOSE)
# library(tibble)
# library(DESeq2)
# library(stringr)
# library(fdrtool)
# library(ggplot2)
# library(ggraph)
# # library(ggridges)
# library(ggrepel)
# library(shinyjs)
# library(rentrez)
# library(reshape2)
# library(pheatmap)
# library(pathview)
# library(enrichplot)
# library(ggbeeswarm)
# library(geneplotter)
# library(RColorBrewer)
# library(shinyWidgets)
# library(clusterProfiler)
# library(shinycssloaders)
# 
# 
# library(sva)
# library(plyr)
# library(dplyr)
# library(Rmisc)
# library(psych)
# library(WGCNA)
# library(doRNG)
# library(igraph)
# library(GENIE3)
# library(plotly)
# library(shinyBS)
# library(STRINGdb)
# library(GEOquery)
# # library(networkD3)
# library(DEGreport)
# library(gprofiler2)
# library(shinyalert)
# library(ReactomePA)
# library(genefilter)
# library(visNetwork)
# library(edgebundleR)

# options(url.method='libcurl')
# 
# get.DEGs <- function(dds, ctrl, treat, p.adjust = 0.05, abs.lfc = 1, save = FALSE) {
#   if (length(treat) == 0) {
#     stop("Treatment groups can not be null ...", call. = FALSE)
#   }
#   DEGs.List <- lapply(treat, function(x){
#     Res <- as.data.frame(results(dds, contrast = c("condition", x, ctrl)))
#     Des <- subset(Res, padj < p.adjust & abs(log2FoldChange) > abs.lfc)
#     if (save == TRUE) {
#       if (!dir.exists("REGs")) {
#         dir.create("REGs")
#       }
#       write.csv(Res, paste0("REGs/", x, "_vs_", ctrl, ".csv"))
# 
#       if (!dir.exists("DEGs")) {
#         dir.create("DEGs")
#       }
#       write.csv(Des, paste0("DEGs/", x, "_vs_", ctrl, ".csv"))
#     }
#     return(Res)
#   })
#   names(DEGs.List) <- paste(treat, ctrl, sep = "_vs_")
#   return(DEGs.List)
# }
# 
# load.DEGs <- function(filesName) {
#   DEGs.files <- paste0("DEGs/", filesName, ".csv")
#   DEGs.List <- lapply(DEGs.files, function(x){
#     read.csv(x, row.names = 1, header = T)
#   })
#   names(DEGs.List) <- filesName
#   return(DEGs.List)
# }
# 
# load.REGs <- function(filesName) {
#   REGs.files <- paste0("REGs/", filesName, ".csv")
#   REGs.List <- lapply(REGs.files, function(x){
#     read.csv(x, row.names = 1, header = T)
#   })
#   names(REGs.List) <- filesName
#   return(REGs.List)
# }
# 
# subset.Tab <- function(dds, condition) {
#   sampleTable <- as.data.frame(colData(dds))
#   rownames(sampleTable) <- sampleTable$samples
#   idx <- lapply(condition, function(x){
#     sampleTable[sampleTable$condition == x, "samples"]
#   }) %>% unlist
#   idx <- idx[idx %in% rownames(sampleTable)]
#   sampleTable <- sampleTable[idx, ]
#   return(sampleTable)
# }
# 
# 
