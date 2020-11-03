# QRseq
An interactive web tool for analyzing RNAseq sequencing data, which allows you to complete a standard RNAseq analysis process in a short time, including differential analysis, WGCNA, functional enrichment and protein-to-protein network.

# Installation
Install the dependencies from Bioconductor:
```
## try http:// if https:// URLs are not supported
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
## BiocManager::install("BiocUpgrade") ## you may need this
BiocManager::install(c("impute", "preprocessCore", "GO.db", "AnnotationDbi"))
```
Install the QRseq package from github:
```
## install.packages("devtools") ## 
devtools::install_github("goushixue/QRseq")
```
