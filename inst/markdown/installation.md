<h2 id="abstract" style="width:100%; text-align:left;font-family:&#39;Times New Roman&#39;, Times, serif;">Abstract</h2>

<p style="text-align:justify;font-family:&#39;Times New Roman&#39;, Times, serif;">RNA-Sequencing (RNA-seq) has become the most commonly used tool in life science researches for exploring whole transcript profiles. The advance of second-generation sequencing (NGS) has promoted a large number of RNA-seq data. However, the popularity of bioinformatics lags far behind the generation of sequencing data, resulting in the inability of most researchers to analyze RNA-seq data. Although a large number of tools are currently available for RNA-seq analysis, data uploading, analysis, and visualization through an interactive interface are more acceptable to researchers than command-line code. Therefore, we have designed an interactive analysis platform based on Shiny, named QRAP, which can easily accomplish RNA-seq data analysis through an intuitive graphical interface on the web page. QRAP support to analysis publicly available and user generated data, including multiple RNA-seq analysis modules, and provide more than 500 species’s function annotation.</p>

<h2 id="features" style="width:100%; text-align:left;font-family:&#39;Times New Roman&#39;, Times, serif;">Workflow</h2>

<img src="../shiny/www/images/workflow.jpg" width="100%" style="clear: both;display: block;margin: auto;"/>

<h2 id="features" style="width:100%; text-align:left;font-family:&#39;Times New Roman&#39;, Times, serif;">Features</h2>

<img src="../shiny/www/images/features.jpg" width="100%" style="clear: both;display: block;margin: auto;"/>


<h2 id="install" style="width:100%; text-align:left;font-family:&#39;Times New Roman&#39;, Times, serif;">Installing</h2>

<h3 style="width:100%; text-align:left;font-family:&#39;Times New Roman&#39;, Times, serif;"> Depends </h3>

- R (>= 3.5.2)
- STRINGdb (>= 2.3.0)
- DOSE (>= 3.16.0)
- enrichplot (>= 1.10.0)
- fgsea (>= 1.16.0)
- clusterProfiler (>= 3.18.0)

<h3 style="width:100%; text-align:left;font-family:&#39;Times New Roman&#39;, Times, serif;"> Installation </h3>

- Install the QRAP from github:

```
## install.packages("devtools") ## you may need install devtools first
devtools::install_github("gsx-ucas/QRAP")
```

<h2 id="getting-start" style="width:100%; text-align:left;font-family:&#39;Times New Roman&#39;, Times, serif;">Getting Start</h2>

<h3 style="width:100%; text-align:left;font-family:&#39;Times New Roman&#39;, Times, serif;"> Launching QRseq </h3>

```
library(QRAP)
startQRAP()
```

<h3 style="width:100%; text-align:left;font-family:&#39;Times New Roman&#39;, Times, serif;"> Start your anlysis </h3>

<img src="../shiny/www/images/page_demo.jpg" width="100%" style="clear: both;display: block;margin: auto;"/>

<h2 id="documentation" style="width:100%; text-align:left;font-family:&#39;Times New Roman&#39;, Times, serif;">Documentation</h2>

The documentation is available at <a href="https://github.com/gsx-ucas/QRAP" target="_blank"> here </a>, the doc include a tutorial and example gallery.

<h2 id="development" style="width:100%; text-align:left;font-family:&#39;Times New Roman&#39;, Times, serif;"> Development </h2>

QRseq development takes place on Github: <a href="https://github.com/gsx-ucas/QRseq" target="_blank">https://github.com/gsx-ucas/QRAP</a>

Please submit any reproducible bugs you encounter to the <a href="https://github.com/gsx-ucas/QRAP/issues" target="_blank">issue tracker</a>

We will also put most commonly encountered issues in the ***FAQ*** page.

