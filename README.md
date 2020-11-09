## QRseq
Here we developed an R shiny application QRseq, which can easily launched from local web browser for analyzing sequenced or published RNA-seq data. QRseq allows users to upload RNA-seq data from local or to input some keyworks or an accession number of GEO DataSets within the app to start their analysis. This application start from data input, followed by preprocessing data through filtering out low expressed genes and poorly reproducible samples, correcting batch effects, normalizing and transforming expression values, identifying differential expressed genes and other biological patterns, exploring the enrichment of functions, analyzing and visualizing the protein to protein networks or gene regulation networks. QRseq provide a clear analysis flow and an user friendly GUI interface but keep most important parameter of involved functions, which suite for both non-programing experience researchers and expert bioinformatic researchers. User can accomplish a standard RNA-seq analysis in hours depend on the size of their dataset and requires using QRseq.

## Features
![The main Features](https://github.com/goushixue/QRseq/blob/main/inst/shiny/myApp/www/images/workflow.tiff)

## Installing

### Depends
- R (>= 3.5.2)
- DOSE (>= 3.16.0)
- enrichplot (>= 1.10.0)
- STRINGdb (>= 2.3.0)

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
>2. Install STRINGdb (2.3.0) \
>   also install DOSE (3.16.0) and enrichplot (1.10.0) by this way (If need)
```
## download the development version of STRINGdb 
download.file("http://www.bioconductor.org/packages/devel/bioc/src/contrib/STRINGdb_2.3.0.tar.gz", "STRINGdb_2.3.0.tar.gz")
## install from local downloaded source file
install.packages("STRINGdb_2.3.0.tar.gz", repos = NULL, type = "source")
```
>3. Install DOSE (3.16.0) and enrichplot (1.10.0) if your R version < 4.0
```
## download DOSE (3.16.0) 
download.file("http://www.bioconductor.org/packages/release/bioc/src/contrib/DOSE_3.16.0.tar.gz", "DOSE_3.16.0.tar.gz")
## install from local downloaded source file
install.packages("DOSE_3.16.0.tar.gz", repos = NULL, type = "source")

## download enrichplot (1.10.0)
download.file("http://www.bioconductor.org/packages/release/bioc/src/contrib/enrichplot_1.10.0.tar.gz", "enrichplot_1.10.0.tar.gz")
## install from local downloaded source file
install.packages("enrichplot_1.10.0.tar.gz", repos = NULL, type = "source")
```
>4. Install the QRseq package from github:
```
## install.packages("devtools") ## you may need install devtools first
devtools::install_github("goushixue/QRseq")
```
## Getting Start

### Launching QRseq
```
library(QRseq)
startQRseq()
```
### Start your anlysis
There are two action buttons in the home page above, click `Get Started Local` if you want upload your own data, or click `Get started GEO` to explore the published data sets.
![Screenshot of home page](https://github.com/goushixue/QRseq/blob/main/inst/shiny/myApp/www/images/get-start-button.jpeg)

## Documentation
The documentation is available at <a href="https://github.com/goushixue/QRseq" target="_blank"> here </a>, the doc include a tutorial and example gallery.

## Development

QRseq development takes place on Github: <a href="https://github.com/goushixue/QRseq" target="_blank">https://github.com/goushixue/QRseq</a>

Please submit any reproducible bugs you encounter to the <a href="https://github.com/goushixue/QRseq/issues" target="_blank">issue tracker</a>

We will also put most commonly encountered issues in the ***FAQ*** page.
