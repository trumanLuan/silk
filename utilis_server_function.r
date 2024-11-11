

##* **********
##* summarize meta.data of Seurat object (list)
##* 

report_summary_sce_metadata <- function(sce){
  ngene.mean = sapply(sce, function(x)  mean(x@meta.data$nFeature_RNA) )
  ngene.sd = sapply(sce, function(x)  sd(x@meta.data$nFeature_RNA) )
  numi.mean = sapply(sce, function(x)  mean(x@meta.data$nCount_RNA) )
  numi.sd = sapply(sce, function(x)  sd(x@meta.data$nFeature_RNA) )
  mito.mean = sapply(sce, function(x)  mean(x@meta.data$mito_percent) )
  mito.sd = sapply(sce, function(x)  sd(x@meta.data$mito_percent) )
  n.doublet = sapply(sce, function(x)  table(x@meta.data$scDblFinder.class)['doublet'] )
  percent.doublet <- sapply(sce, function(x)  table(x@meta.data$scDblFinder.class)['doublet']/nrow(x@meta.data) )
  percent.g1 <- sapply(sce, function(x)  table(x@meta.data$Phase)['G1']/nrow(x@meta.data) )
  percent.g2m <- sapply(sce, function(x)  table(x@meta.data$Phase)['G2M']/nrow(x@meta.data) )
  percent.s <- sapply(sce, function(x)  table(x@meta.data$Phase)['S']/nrow(x@meta.data) )
  
  summary.df <- data.frame(
    sample=names(sce), 
    description = rep('', length(sce)), 
    group = rep("", length(sce)), 
    ncell = sapply(sce, ncol), 
    # ngene.summary = paste0(round(ngene.mean, 2), " ± ", round(ngene.sd, 2)),
    # numi.summary = paste0(round(numi.mean, 2), " ± ", round(numi.sd, 2)),
    # mito.summary = paste0(round(mito.mean, 2), " ± ", round(mito.sd, 2)),
    ngene.mean = ngene.mean,
    numi.mean = numi.mean, 
    mito.mean = mito.mean,
    n.doublet = n.doublet, percent.doublet = percent.doublet,
    percent.g1 = percent.g1, 
    percent.g2m = percent.g2m,
    percent.s = percent.s,
    stringsAsFactors = F
  )
  
  summary.df
}


##* **********
##* filter single cells according to the input in filter module
##* 

sce_filter <- function(sce, fix = TRUE, customized = FALSE){
    
}


