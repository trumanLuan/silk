library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)
library(fs)

library(plotly)
library(DT)

library(SCpubr)
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
library(CellChat) # infer cell-cell communication
library(liana) # infer cell-cell interaction

# library(infercnv)  # infercnv
library(slingshot) # for trajectory
library(monocle) # for trajectory
# library(copykat) # infer cnv
library(CaSpER)
library(SCEVAN)
library(SCENIC)

library(ROGUE) # evaluate cluster purity

# library(SCPubr)

library(clusterProfiler) # for functiontal enrichment