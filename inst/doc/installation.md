<h2 id="abstract" style="width:100%; text-align:left;font-family:&#39;Times New Roman&#39;, Times, serif;">Abstract</h2>

<p style="text-align:justify;font-family:&#39;Times New Roman&#39;, Times, serif;">Here we developed QRseq, a R shiny application can easily launched from local web browser for analyzing sequenced or
      published RNA-seq data. QRseq allows user to upload RNA-seq data from local or to input some keyworks or an accession
      number of GEO DataSets within the app to start their analysis. This application start from data input, followed by preprocessing
      data by filtering low expressed genes and poorly reproducible samples, correcting batch effects, normalizing and transforming data,
      identifying differential expressed genes and other biological patterns, exploring the enrichment of functions, analyzing and visualizing
      the protein to protein networks or gene regulation networks. QRseq provide a clear analysis flow and an user friendly GUI interface but
      keep most important parameter of involved functions, which suite for both non-programing experience researchers and expert bioinformatic
      researchers. User can accomplish a standard RNA-seq analysis in hours depend on the size of their dataset and requires using QRseq.</p>
 
<h2 id="features" style="width:100%; text-align:left;font-family:&#39;Times New Roman&#39;, Times, serif;">Features</h2>

<p style="text-align:center;">
  <strong>The main Features was showed below:</strong>
</p>
<img src="../shiny/myApp/www/images/workflow.tiff" width="70%" style="clear: both;display: block;margin: auto;"/>


<h2 id="install" style="width:100%; text-align:left;font-family:&#39;Times New Roman&#39;, Times, serif;">Installing</h2>

<h3 style="width:100%; text-align:left;font-family:&#39;Times New Roman&#39;, Times, serif;"> Depends </h3>

- R (>= 3.5.2)
- DOSE (>= 3.16.0)
- enrichplot (>= 1.10.0)
- STRINGdb (>= 2.3.0)

<h3 style="width:100%; text-align:left;font-family:&#39;Times New Roman&#39;, Times, serif;"> Installation </h3>

- 1. Install the dependencies from Bioconductor:

```
## try http:// if https:// URLs are not supported
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
    
## BiocManager::install("BiocUpgrade") ## you may need this
bio_pkgs <- c("impute", "preprocessCore", "GO.db", "AnnotationDbi", "SummarizedExperiment", "DOSE", "genefilter", 
             "ReactomePA", "DEGreport", "GEOquery", "GENIE3", "sva", "clusterProfiler", "geneplotter", "enrichplot", "DESeq2", "limma")
             
for (i in bio_pkgs) {
  if (!requireNamespace(i, quietly=TRUE))
  BiocManager::install(i)
}
```

- 2. Install STRINGdb (2.3.0)

```
## download the development version of STRINGdb 
download.file("http://www.bioconductor.org/packages/devel/bioc/src/contrib/STRINGdb_2.3.0.tar.gz", "STRINGdb_2.3.0.tar.gz")
## install from local downloaded source file
install.packages("STRINGdb_2.3.0.tar.gz", repos = NULL, type = "source")
```

- 3. Install the QRseq package from github:

```
## install.packages("devtools") ## you may need install devtools first
devtools::install_github("goushixue/QRseq")
```

<h2 id="getting-start" style="width:100%; text-align:left;font-family:&#39;Times New Roman&#39;, Times, serif;">Getting Start</h2>

<h3 style="width:100%; text-align:left;font-family:&#39;Times New Roman&#39;, Times, serif;"> Launching QRseq </h3>

```
library(QRseq)
startQRseq()
```

<h3 style="width:100%; text-align:left;font-family:&#39;Times New Roman&#39;, Times, serif;"> Start your anlysis </h3>

<p style="text-align:justify;font-family:&#39;Times New Roman&#39;, Times, serif;">As you can see in this page above, there are two action buttons can let you start your data exploring. Click <code>Get Started Local</code> if you want upload your own data, or click <code>Get started GEO</code> to explore the published data sets.</p>

<img src="../shiny/myApp/www/images/get-start-button.jpeg" width="100%" style="clear: both;display: block;margin: auto;"/>

<h2 id="documentation" style="width:100%; text-align:left;font-family:&#39;Times New Roman&#39;, Times, serif;">Documentation</h2>

The documentation is available at <a href="https://github.com/goushixue/QRseq" target="_blank"> here </a>, the doc include a tutorial and example gallery.

<h2 id="development" style="width:100%; text-align:left;font-family:&#39;Times New Roman&#39;, Times, serif;"> Development </h2>

QRseq development takes place on Github: <a href="https://github.com/goushixue/QRseq" target="_blank">https://github.com/goushixue/QRseq</a>

Please submit any reproducible bugs you encounter to the <a href="https://github.com/goushixue/QRseq/issues" target="_blank">issue tracker</a>

We will also pub most commonly encountered issues in the ***FAQ*** page.

