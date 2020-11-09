## 1. Getting Start

#### 1.1 Launching QRseq

```
library(QRseq)
startQRseq()
```

#### 1.2 Start your anlysis 

As you can see in the home page, there are two action buttons can let you start your data exploring. Click <code>Get Started Local</code> if you want upload your own data, or click <code>Get started GEO</code> to explore the published data sets.

<img src="../shiny/myApp/www/images/get-start-button.jpeg" width="100%" style="clear: both;display: block;margin: auto;"/>

## 2. Data Input

### 2.1 upload data from local

- browse your files to choices the expression matrix file.
- set the header and rownames parameters in the check box.
- don't worry the delemeters, it will be detected automaticaly.
- click the upload button to upload your data, or you can click the example button to load the example datasets.
- after you upload the data, a data table will be showed in the right panel, like this:

<div style="float:left;border:solid 1px 000;margin:2px;"><img src="../shiny/myApp/www/figures/data-upload-card.jpeg"  width="43%" ></div>

<div style="float:left;border:solid 1px 000;margin:2px;"><img src="../shiny/myApp/www/figures/preview-uploaded-data.jpeg" width="55%" ></div>

### 2.2 download data from GEO

- searching datasets you are interseted in from geo in our app.
- or you can skip this step if you known exactly which datasets you are interested.
- using searching geo functions, a table will be returned:


<img src="../shiny/myApp/www/figures/searching-geo-card.jpeg" width="44%"/>
<img src="../shiny/myApp/www/figures/searching-results.jpeg" width="54%"/>


- after detemind which datasets you want to analyze, input this Series_Accession number to the input box and click the fetch data button:

<img src="../shiny/myApp/www/figures/geo-downloaded-card.jpg" width="30%" /> 
<img src="../shiny/myApp/www/figures/upload-geo-datasets-preview.jpeg" width="69%"/>

### 2.3 pre-filtering data

- genes with low expression level dose no means for downstream analyis, for speed up the analysis, we firstly filtered out genes have total counts lower than an user defined threshold (default: 1 reads).
- For the downstream analysis, users need select the species and gene types of their datasets.
- after click the filter data button, a density plot will be generated and red line means the cutsite.
- Some words will be printed in the bottom of the density plot, it will tell you how many genes were filtered and how many genes were keeped for downstream analysis.

<img src="../shiny/myApp/www/figures/data-pre-filtering.jpeg" width="40%" /> 
<img src="../shiny/myApp/www/figures/density_plot_filtered_data.jpeg" width="58%"/>

## 3. Data Normalizing and transforming (by DESeq2)

### 3.1 Design the experiment condition table

- If your sample names were named as **sample1-1, sample1-2, sample2-1, sample2-2...**, the condition will be extracted as **sample1, sample1, sample2, sample2...** automaticlly.
- If the condition table was not that one you wanted, you can download it to your computer and modify it to the corrected condition groups, and then upload it to the app to do the next step analysis.

<img src="../shiny/myApp/www/figures/unmodified-design-table.jpeg" width="51%" /> `>>`
<img src="../shiny/myApp/www/figures/modified-design-table.jpeg" width="41%"/>

- If your data have a batch effector (can be detected in the data quality assessment part), a column can be added to the design table and this coumn can be treated as batch effector, and will be corrected.
- after finished the experimental condition table, you can run the DESeq normalize data and choices one method (rlog or vst) to transform the normalized expression levels.
- More Parameters for running this step, including correct batch effects, can be accessed by click the Additional Parameters button.

<img src="../shiny/myApp/www/figures/DESeq2_running_card.jpeg" width="80%" />
> Additional parametes for running 

<img src="../shiny/myApp/www/figures/additional-parameters-of-deseq.jpeg" width="80%"/>

## 4. Data Quality Assessment

### 4.1 Principal Component Analysis (PCA)

> A glob view of analysis page modules, the left panel is parameters which control the runing or visualizing, the middle panel is the region where figures or datatables will be produced and showed, the right panel is the slider for control the figure width and height.

<img src="../shiny/myApp/www/figures/whole-view-of-analysis-page.jpeg" width="100%"/>

- in the addtional parameters, we provide an directly input of ggplot2 codes, which allow users with programming skills to modify the figures.

> this example below showed that change text family to "Times", text face to "bold" and fontsize to 20 through one line ggplot2 codes.

<img src="../shiny/myApp/www/figures/ggplot-code-inputs.jpeg" width="46%"/>
<img src="../shiny/myApp/www/figures/pca-modified-by-ggplot-codes.jpeg" width="53%"/>

### 4.2 Hierarchical Clustering Heatmap

- We use the top n (default 500) highest variance genes between samples to prudce the hierarchical clustered heatmap.

<img src="../shiny/myApp/www/figures/Hierarchical-clustering-heatmap.jpeg" width="100%"/>

- you can easily change the color and cut the tree to by the clusters.

<img src="../shiny/myApp/www/figures/Hierarchical-clustering-heatmap-modified.jpeg" width="100%"/>

### 4.3 Sample to Sample Distance

<img src="../shiny/myApp/www/figures/Sample-to-sample-distance.jpeg" width="100%"/>

### 4.4 Sample correlation coefficients

- Pairwise Scatter Plot

<img src="../shiny/myApp/www/figures/Sample-correlation-pairwise-scatter.jpeg" width="100%"/>

- multiple groups compare heatmap

<img src="../shiny/myApp/www/figures/Sample-correlation-multiple-heatmap.jpeg" width="100%"/>

- multiple groups compare scatter plot

<img src="../shiny/myApp/www/figures/Sample-correlation-multiple-scatterplot.jpeg" width="100%"/>

## 5. Differential Expression Analysis

### 5.1 Volcano plot of DEGs

<img src="../shiny/myApp/www/figures/Volcano_plot.jpeg" width="100%"/>

### 5.2 DEG heatmap

<img src="../shiny/myApp/www/figures/DEGs-heatmap.jpeg" width="100%"/>

### 5.3 DEG VennDiagram

<img src="../shiny/myApp/www/figures/DEGs-venn.jpeg" width="100%"/>

### 5.3 DEG VennDiagram

<img src="../shiny/myApp/www/figures/DEGs-number-barplot.jpeg" width="100%"/>

### 5.3 DEG Detail Table

<img src="../shiny/myApp/www/figures/DEGs-tables.jpeg" width="100%"/>

## 6. DEG Patterns Detection

<img src="../shiny/myApp/www/figures/DEGs-patterns.jpeg" width="100%"/>

## 7. Gene Expression Visualization

### 7.1 Expression BarPlot

<img src="../shiny/myApp/www/figures/gene-expression-barplot.jpeg" width="100%"/>
<img src="../shiny/myApp/www/figures/gene-expression-barplot-splited.jpeg" width="100%"/>

### 7.2 Expression BoxPlot

<img src="../shiny/myApp/www/figures/gene-expression-boxplot.jpeg" width="100%"/>
<img src="../shiny/myApp/www/figures/gene-expression-boxplot-splited.jpeg" width="100%"/>

### 7.3 Expression HeatMap

<img src="../shiny/myApp/www/figures/gene-expression-heatmap.jpeg" width="100%"/>

### 7.4 Log2FoldChage BarPlot

<img src="../shiny/myApp/www/figures/gene-log2foldchange-barplot.jpeg" width="100%"/>
<img src="../shiny/myApp/www/figures/gene-log2foldchange-barplot-splited.jpeg" width="100%"/>

### 7.5 Log2FoldChage HeatMap

<img src="../shiny/myApp/www/figures/gene-log2foldchange-heatmap.jpeg" width="100%"/>

### 7.6 Log2FoldChage DotPlot

<img src="../shiny/myApp/www/figures/gene-log2foldchange-dotplot.jpeg" width="100%"/>

## 8. WGCNA analysis

### 8.1 Data Preparation

<img src="../shiny/myApp/www/figures/wgcna-data-preparation.jpeg" width="100%"/>

### 8.2 Modules Detection

<img src="../shiny/myApp/www/figures/wgcna-modules-detection.jpeg" width="100%"/>

### 8.3 Data Preparation

<img src="../shiny/myApp/www/figures/wgcna-modules-traits-heatmap.jpeg" width="100%"/>

## 9. ClusterProfiler ORA Enrichment Analysis

### 9.1 Running Parameters

<img src="../shiny/myApp/www/figures/clusterprofiler-run-card.jpeg" width="100%"/>

### 9.2 DotPlot

<img src="../shiny/myApp/www/figures/clusterprofiler-dot-plot.jpeg" width="100%"/>

### 9.3 BarPlot

<img src="../shiny/myApp/www/figures/clusterprofiler-bar-plot.jpeg" width="100%"/>

### 9.4 Ggtable

<img src="../shiny/myApp/www/figures/clusterprofiler-ggtable-plot.jpeg" width="100%"/>

### 9.5 Cnetplot

<img src="../shiny/myApp/www/figures/clusterprofiler-cnetplot-plot.jpeg" width="100%"/>

### 9.6 Emapplot

<img src="../shiny/myApp/www/figures/clusterprofiler-emapplot.jpeg" width="100%"/>

### 9.7 Pathway Heatmap

<img src="../shiny/myApp/www/figures/clusterprofiler-pathway-heatmap.jpeg" width="100%"/>

## 10. ClusterProfiler GSEA Enrichment Analysis

### 10.1 Running Parameters

<img src="../shiny/myApp/www/figures/clusterprofiler-gsea-card.jpeg" width="100%"/>

### 10.2 gseaplot2

<img src="../shiny/myApp/www/figures/clusterprofiler-gsea-gseaplot2.jpeg" width="100%"/>

### 10.3 ridgeplot

<img src="../shiny/myApp/www/figures/clusterprofiler-gsea-ridgeplot.jpeg" width="100%"/>

### 10.4 heatmap

<img src="../shiny/myApp/www/figures/clusterprofiler-gsea-heatmap.jpeg" width="100%"/>

## 11. gProfiler2 Enrichment Analysis

### 11.1 Running Parameters

<img src="../shiny/myApp/www/figures/gprofiler2-run-card.jpeg" width="100%"/>

### 11.2 gostplot

<img src="../shiny/myApp/www/figures/gprofiler2-gostplot.jpeg" width="100%"/>

### 11.3 gosttable

<img src="../shiny/myApp/www/figures/gprofiler2-gosttable.jpeg" width="100%"/>

### 11.4 dotplot

<img src="../shiny/myApp/www/figures/gprofiler2-dotplot.jpeg" width="100%"/>

### 11.5 heatmap

<img src="../shiny/myApp/www/figures/gprofiler2-heatmap.jpeg" width="100%"/>

## 12. Protein to protein network

<img src="../shiny/myApp/www/figures/ppi-network.jpeg" width="100%"/>

## 13. GENIE3 network

### 13.1 edgebundle Plot

<img src="../shiny/myApp/www/figures/GENIE3-network-edgebundle.jpeg" width="100%"/>

### 13.2 visNetwork Plot

<img src="../shiny/myApp/www/figures/GENIE3-network-visNetwork.jpeg" width="100%"/>

