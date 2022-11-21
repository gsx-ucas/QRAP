## Overview

We designed an interactive RNA-seq analysis toolkit based on R Shiny package, named QRAP (Quick RNA-seq Analysis Platform), which can easily accomplish RNA-seq data analysis and visualization through an intuitive graphical interface on the web page. As a comprehensive RNA-seq analysis tool, QRAP can support to analyze publicly available and user-generated data, which include regular RNA-seq data, time-course RNA-seq, data and clinically relevant RNA-seq data, and provide function annotation for approximately 500 species.

<figure>
  <center>1.QRAP workflow</center>
  <img src="inst/shiny/www/images/workflow.jpg" alt="QRAP workflow" width="100%" style="max-width: 100%;" />
</figure>

<figure>
  <center>2.QRAP Features</center>
  <img src="inst/shiny/www/images/features.jpg" alt="QRAP workflow" width="100%" style="max-width: 100%;" />
</figure>

## Usage

#### Depends
- R (>= 3.5.2)
- STRINGdb (>= 2.3.0)
- DOSE (>= 3.16.0)
- enrichplot (>= 1.10.0)
- fgsea (>= 1.16.0)
- clusterProfiler (>= 3.18.0)
> Unpredictable errors may occur if these packages are lower than the specified version

#### Installation

Install the QRAP from github:
```
## install.packages("devtools") ## you may need install devtools first
devtools::install_github("gsx-ucas/QRAP")
```
#### Launch QRAP in R or Rstudio Console
```
library(QRAP)
startQRAP()
```
## Documentation
The documentation is available at <a href="https://gsx-ucas.github.io/QRAP/" target="_blank"> here </a>, the doc include a tutorial and example gallery.
