## QRAP
RNA-Sequencing (RNA-seq) has become the most commonly used tool in life science researches for exploring whole transcript profiles. The advance of second-generation sequencing (NGS) has promoted a large number of RNA-seq data. However, the popularity of bioinformatics lags far behind the generation of sequencing data, resulting in the inability of most researchers to analyze RNA-seq data. Although a large number of tools are currently available for RNA-seq analysis, data uploading, analysis, and visualization through an interactive interface are more acceptable to researchers than command-line code. Therefore, we have designed an interactive analysis platform based on Shiny, named QRAP, which can easily accomplish RNA-seq data analysis through an intuitive graphical interface on the web page. QRAP support to analysis publicly available and user generated data, including multiple RNA-seq analysis modules, and provide more than 500 species’s function annotation.

## Workflow
![The main Features](https://github.com/gsx-ucas/QRAP/tree/main/inst/shiny/www/images/workflow.jpg)

## Features

![The main Features](https://github.com/gsx-ucas/QRAP/tree/main/inst/shiny/www/images/features.jpg)

## Installing

### Depends
- R (>= 3.5.2)
- STRINGdb (>= 2.3.0)
- DOSE (>= 3.16.0)
- enrichplot (>= 1.10.0)
- fgsea (>= 1.16.0)
- clusterProfiler (>= 3.18.0)

### Installation
>1. Install the dependencies from Bioconductor:
```
## try http:// if https:// URLs are not supported
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
    
## BiocManager::install("BiocUpgrade") ## you may need this
bio_pkgs <- c("impute", "preprocessCore", "GO.db", "AnnotationDbi", "STRINGdb", "SummarizedExperiment", "DOSE", "genefilter", 
             "ReactomePA", "DEGreport", "GEOquery", "GENIE3", "sva", "clusterProfiler", "geneplotter", "enrichplot", "DESeq2", "limma")
             
for (i in bio_pkgs) {
  if (!requireNamespace(i, quietly=TRUE))
  BiocManager::install(i)
}
```
>2. Install STRINGdb (2.3.0)
```
## download the development version of STRINGdb 
download.file("http://www.bioconductor.org/packages/devel/bioc/src/contrib/STRINGdb_2.3.0.tar.gz", "STRINGdb_2.3.0.tar.gz")
## install from local downloaded source file
install.packages("STRINGdb_2.3.0.tar.gz", repos = NULL, type = "source")

## remove download source files
file.remove("STRINGdb_2.3.0.tar.gz")
```
>3. Install DOSE (3.16.0), enrichplot (1.10.0), fgsea (1.16.0) and clusterProfiler (3.18.0) if your R version < 4.0
```
## download DOSE (3.16.0) 
download.file("http://www.bioconductor.org/packages/release/bioc/src/contrib/DOSE_3.16.0.tar.gz", "DOSE_3.16.0.tar.gz")
## install from local downloaded source file
install.packages("DOSE_3.16.0.tar.gz", repos = NULL, type = "source")

## download enrichplot (1.10.0)
download.file("http://www.bioconductor.org/packages/release/bioc/src/contrib/enrichplot_1.10.0.tar.gz", "enrichplot_1.10.0.tar.gz")
## install from local downloaded source file
install.packages("enrichplot_1.10.0.tar.gz", repos = NULL, type = "source")

## download fgsea (>= 1.16.0)
download.file("http://www.bioconductor.org/packages/release/bioc/src/contrib/fgsea_1.16.0.tar.gz", "fgsea_1.16.0.tar.gz")
## install from local downloaded source file
install.packages("fgsea_1.16.0.tar.gz", repos = NULL, type = "source")

## download clusterProfiler (>= 3.18.0)
download.file("http://www.bioconductor.org/packages/release/bioc/src/contrib/clusterProfiler_3.18.0.tar.gz", "clusterProfiler_3.18.0.tar.gz")
## install from local downloaded source file
install.packages("clusterProfiler_3.18.0.tar.gz", repos = NULL, type = "source")

## remove download source files
file.remove(c("DOSE_3.16.0.tar.gz", "enrichplot_1.10.0.tar.gz", "fgsea_1.16.0.tar.gz", "clusterProfiler_3.18.0.tar.gz"))
```
>4. Install the QRseq package from github:
```
## install.packages("devtools") ## you may need install devtools first
devtools::install_github("gsx-ucas/QRAP")
```
## Getting Start

### Launching QRseq
```
library(QRAP)
startQRAP()
```
### Start your anlysis

![Screenshot of home page](https://github.com/gsx-ucas/QRAP/tree/main/inst/shiny/www/images/page_demo.jpg)

## Documentation
The documentation is available at <a href="https://gsx-ucas.github.io/QRAP" target="_blank"> here </a>, the doc include a tutorial and example gallery.

## Development

QRAP development takes place on Github: <a href="https://github.com/gsx-ucas/QRAP" target="_blank">https://github.com/gsx-ucas/QRAP</a>

Please submit any reproducible bugs you encounter to the <a href="https://github.com/gsx-ucas/QRAP/issues" target="_blank">issue tracker</a>

We will also put most commonly encountered issues in the ***FAQ*** page.
