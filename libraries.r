library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)
library(fs)

library(BiocParallel)

library(plotly)
library(DT)

library(ggplot2)
library(dplyr)
library(reshape2)

library(corrplot) # for plotting correlation between samples
library(psych) # for pairs.panels for plotting correlation between smaples

library(ComplexHeatmap) 
library(car) # scatterplot with marginal plots

library(Seurat)
# library(DoubletFinder) # for predicting doublets

library(scDblFinder)
library(scds) # for predicting doublets

library(SingleR)
library(celldex)

library(AUCell) # cell type annotatino

library(CellChat) # infer cell-cell communication # fail to install in win
# library(liana) # infer cell-cell interaction # fail to install in win

library(infercnv)  # infercnv
library(slingshot) # for trajectory
library(monocle) # for trajectory
library(copykat) # infer cnv
library(CaSpER) # identifies and visualizes CNV events
# library(SCEVAN)
library(SCENIC)

library(ROGUE) # evaluate cluster purity

library(SCpubr)

library(clusterProfiler) # for functiontal enrichment
library(msigdbr)