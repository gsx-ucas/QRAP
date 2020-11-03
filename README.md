# QRseq
An interactive web tool for analyzing RNA-seq data, which allows you to complete a standard RNAseq analysis process in a short time, including differential analysis, WGCNA, functional enrichment and protein-to-protein network.

# Installation
> 1. Install the dependencies from Bioconductor:
```
## try http:// if https:// URLs are not supported
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
## BiocManager::install("BiocUpgrade") ## you may need this
bio_pkgs <- c("impute", "preprocessCore", "GO.db", "AnnotationDbi", "SummarizedExperiment", "DOSE", "genefilter", 
             "ReactomePA", "DEGreport", "GEOquery", "STRINGdb", "GENIE3", "sva", "clusterProfiler", "geneplotter", "enrichplot", "DESeq2", "limma")
for (i in bio_pkgs) {
  if (!requireNamespace(i, quietly=TRUE))
  BiocManager::install(i)
}
```
> 2. Install the QRseq package from github:
```
## install.packages("devtools") ## you may need install devtools first
devtools::install_github("goushixue/QRseq")
```
