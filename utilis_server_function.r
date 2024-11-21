

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
## function for gsa of input gene list using clusterProfiler package
enrichr_gsa_all_subcat <- function(genes, species, all_subcat = TRUE){
  enrichr <- NULL
  
  m.df <- msigdbr(species = species)
  gs.subcat <- unique(m.df$gs_subcat)
  gs.subcat <- gs.subcat[gs.subcat != ""]
  
  if(all_subcat){
    for(subcat.i in gs.subcat){
      m.t2g <- subset(m.df, gs_subcat %in% subcat.i, select= c("gs_name", "gene_symbol"))
      em <- clusterProfiler::enricher(gene = genes, TERM2GENE= m.t2g)
      em.df.new <- cbind(data.frame(gs_database = rep(subcat.i, nrow(em))), em)
      enrichr <- rbind(enrichr, em.df.new)
    }
  }
  
  enrichr
  
}



##* **********
##* filter single cells according to the input in filter module
##* 

sce_filter <- function(sce, fix = TRUE, customized = FALSE){
    
}


##* **********
##* https://github.com/IanevskiAleksandr/sc-type/blob/master/R/auto_detect_tissue_type.R
##* 
##* 

# GNU General Public License v3.0 (https://github.com/IanevskiAleksandr/sc-type/blob/master/LICENSE)
# Written by Aleksandr Ianevski <aleksandr.ianevski@helsinki.fi>, June 2021
#
# Functions on this page:
# auto_detect_tissue_type: automatically detect a tissue type of the dataset
#
# @params: path_to_db_file - DB file with cell types
# @params: seuratObject - The Seurat Object from wich to extract the input scRNA-seq matrix (rownames - genes, column names - cells), 
# @params: scale - indicates whether the matrix is scaled (TRUE by default)
# @params: assay - e.g. RNA, SCT, integrated
auto_detect_tissue_type <- function(path_to_db_file, seuratObject, scaled, assay = "RNA", ...){
  
  # get all tissue types in DB
  db_read = openxlsx::read.xlsx(path_to_db_file); tissues_ = unique(db_read$tissueType); result_ = c()
  
  for(tissue in tissues_){ print(paste0("Checking...", tissue));
    
    # prepare gene sets
    gs_list = gene_sets_prepare(path_to_db_file, tissue);
    
    # check Seurat version
    package_type <- substr(packageVersion("Seurat"), 1, 1)
    data_type <- if (scaled) "scale.data" else "counts"
    obj <- if (package_type == "5") {
      as.matrix(seuratObject[[assay]]$data_type)
    } else {
      as.matrix(seuratObject[[assay]]@data_type)
    }
    
    es.max = sctype_score(scRNAseqData = obj, scaled = scaled, gs = gs_list$gs_positive, gs2 = gs_list$gs_negative, 
                          marker_sensitivity = gs_list$marker_sensitivity, verbose=!0);
    
    cL_resutls = do.call("rbind", lapply(unique(seuratObject@meta.data$seurat_clusters), function(cl){
      
      es.max.cl = sort(rowSums(es.max[ ,rownames(seuratObject@meta.data[seuratObject@meta.data$seurat_clusters==cl, ])]), decreasing = !0)
      head(data.frame(cluster = cl, type = names(es.max.cl), scores = es.max.cl), 10)
    }))
    
    dt_out = cL_resutls %>% group_by(cluster) %>% top_n(n = 1)
    
    # return mean score for tissue
    result_ = rbind(result_, data.frame(tissue = tissue, score = mean(dt_out$scores)))
  }
  
  # order by mean score
  result_ = result_[order(-result_$score),]
  
  # plot 
  barplot(height=result_$score, names=result_$tissue, col=rgb(0.8,0.1,0.1,0.6),
          xlab="Tissue", ylab="Summary score",  main="The higher summary score, the more likely tissue type is")
  
  result_
}

##* **********
##* https://github.com/IanevskiAleksandr/sc-type/blob/master/R/gene_sets_prepare.R
##* 
##* 

##* **********
##* https://github.com/IanevskiAleksandr/sc-type/blob/master/R/gene_sets_prepare.R
##* 

gene_sets_prepare <- function(path_to_db_file, cell_type){
  
  cell_markers = openxlsx::read.xlsx(path_to_db_file)
  cell_markers = cell_markers[cell_markers$tissueType == cell_type,] 
  cell_markers$geneSymbolmore1 = gsub(" ","",cell_markers$geneSymbolmore1); cell_markers$geneSymbolmore2 = gsub(" ","",cell_markers$geneSymbolmore2)
  
  # correct gene symbols from the given DB (up-genes)
  cell_markers$geneSymbolmore1 = sapply(1:nrow(cell_markers), function(i){
    
    markers_all = gsub(" ", "", unlist(strsplit(cell_markers$geneSymbolmore1[i],",")))
    markers_all = toupper(markers_all[markers_all != "NA" & markers_all != ""])
    markers_all = sort(markers_all)
    
    if(length(markers_all) > 0){
      suppressMessages({markers_all = unique(na.omit(checkGeneSymbols(markers_all)$Suggested.Symbol))})
      paste0(markers_all, collapse=",")
    } else {
      ""
    }
  })
  
  # correct gene symbols from the given DB (down-genes)
  cell_markers$geneSymbolmore2 = sapply(1:nrow(cell_markers), function(i){
    
    markers_all = gsub(" ", "", unlist(strsplit(cell_markers$geneSymbolmore2[i],",")))
    markers_all = toupper(markers_all[markers_all != "NA" & markers_all != ""])
    markers_all = sort(markers_all)
    
    if(length(markers_all) > 0){
      suppressMessages({markers_all = unique(na.omit(checkGeneSymbols(markers_all)$Suggested.Symbol))})
      paste0(markers_all, collapse=",")
    } else {
      ""
    }
  })
  
  cell_markers$geneSymbolmore1 = gsub("///",",",cell_markers$geneSymbolmore1);cell_markers$geneSymbolmore1 = gsub(" ","",cell_markers$geneSymbolmore1)
  cell_markers$geneSymbolmore2 = gsub("///",",",cell_markers$geneSymbolmore2);cell_markers$geneSymbolmore2 = gsub(" ","",cell_markers$geneSymbolmore2)
  
  gs = lapply(1:nrow(cell_markers), function(j) gsub(" ","",unlist(strsplit(toString(cell_markers$geneSymbolmore1[j]),",")))); names(gs) = cell_markers$cellName
  gs2 = lapply(1:nrow(cell_markers), function(j) gsub(" ","",unlist(strsplit(toString(cell_markers$geneSymbolmore2[j]),",")))); names(gs2) = cell_markers$cellName
  
  list(gs_positive = gs, gs_negative = gs2)
}


##* **********
##*
##* 


sctype_score <- function(scRNAseqData, scaled = T, gs, gs2 = NULL, gene_names_to_uppercase = T, ...){
  
  # check input matrix
  if(!is.matrix(scRNAseqData)){
    warning("scRNAseqData doesn't seem to be a matrix")
  } else {
    if(sum(dim(scRNAseqData))==0){
      warning("The dimension of input scRNAseqData matrix equals to 0, is it an empty matrix?")
    }
  }
  
  # marker sensitivity
  marker_stat = sort(table(unlist(gs)), decreasing = T)
  marker_sensitivity = data.frame(score_marker_sensitivity = scales::rescale(as.numeric(marker_stat), to = c(0,1), from = c(length(gs),1)),
                                  gene_ = names(marker_stat), stringsAsFactors = !1)
  
  # convert gene names to Uppercase
  if(gene_names_to_uppercase){
    rownames(scRNAseqData) = toupper(rownames(scRNAseqData));
  }
  
  # subselect genes only found in data
  names_gs_cp = names(gs); names_gs_2_cp = names(gs2);
  gs = lapply(1:length(gs), function(d_){ 
    GeneIndToKeep = rownames(scRNAseqData) %in% as.character(gs[[d_]]); rownames(scRNAseqData)[GeneIndToKeep]})
  gs2 = lapply(1:length(gs2), function(d_){ 
    GeneIndToKeep = rownames(scRNAseqData) %in% as.character(gs2[[d_]]); rownames(scRNAseqData)[GeneIndToKeep]})
  names(gs) = names_gs_cp; names(gs2) = names_gs_2_cp;
  cell_markers_genes_score = marker_sensitivity[marker_sensitivity$gene_ %in% unique(unlist(gs)),]
  
  # z-scale if not
  if(!scaled) Z <- t(scale(t(scRNAseqData))) else Z <- scRNAseqData
  
  # multiple by marker sensitivity
  for(jj in 1:nrow(cell_markers_genes_score)){
    Z[cell_markers_genes_score[jj,"gene_"], ] = Z[cell_markers_genes_score[jj,"gene_"], ] * cell_markers_genes_score[jj, "score_marker_sensitivity"]
  }
  
  # subselect only with marker genes
  Z = Z[unique(c(unlist(gs),unlist(gs2))), ]
  
  # combine scores
  es = do.call("rbind", lapply(names(gs), function(gss_){ 
    sapply(1:ncol(Z), function(j) {
      gs_z = Z[gs[[gss_]], j]; gz_2 = Z[gs2[[gss_]], j] * -1
      sum_t1 = (sum(gs_z) / sqrt(length(gs_z))); 
      sum_t2 = sum(gz_2) / sqrt(length(gz_2));
      if(is.na(sum_t2)){
        sum_t2 = 0;
      }
      sum_t1 + sum_t2
    })
  })) 
  
  dimnames(es) = list(names(gs), colnames(Z))
  es.max <- es[!apply(is.na(es) | es == "", 1, all),] # remove na rows
  
  es.max
}

##* **********
##*
##* 

#' @title sctype source files
#' @name sctype_source
#' @description loads sctype functions needed for an automated cell type annotation . 
#' @details none
#' @param none 
#' @return original ScType database
#' @export
#' @examples
#' db_=sctype_source()
#' 
sctype_source <- function(){
  # load tissue auto detect
  source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/auto_detect_tissue_type.R")
  # load gene set preparation function
  source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/gene_sets_prepare.R")
  # load cell type annotation function
  source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/sctype_score_.R")
  # load ScType database
  db_ = "https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/ScTypeDB_full.xlsx"
  return(db_)
}

#' @title Run sctype analysis on Seurat object
#' @name run_scType
#' @description run an automated cell type annotation 
#' @details This function is compatible with seurat versions 4 and 5 -- different methods(@,$) to retrieve counts,data,scale.data
#' @param seurat_object A Seurat object
#' @param known_tissue_type The tissue type of the input data (optional)
#' @param custom_marker_file Path to the custom marker file (optional)
#' @param plot Whether to plot the results (default is FALSE)
#' @param name The name of the metadata column to store the scType results (default is "sctype_classification")
#' @return A modified copy of the input Seurat object with a new metadata column
#' 
#' @import sctype source code
#' @import Seurat DimPlot
#' 
#' @examples
#' seurat_object=run_scType(seurat_object,"Immune system)
#' 
#' @export
#' 


run_sctype <- function(seurat_object, known_tissue_type = NULL, assay = "RNA", scaled = TRUE, custom_marker_file = NULL, plot = FALSE, name = "sctype_classification") {
  db_=sctype_source()
  # Check for missing arguments
  if (is.null(seurat_object)) {
    stop("Argument 'seurat_object' is missing")
  }
  if (!inherits(seurat_object, "Seurat")) {
    stop("Argument 'seurat_object' must be a Seurat object")
  }
  # Set default custom marker file
  if (is.null(custom_marker_file)) {
    custom_marker_file = db_
  }
  # Auto-detect tissue type if not provided
  if (is.null(known_tissue_type)) {
    print("Guessing tissue type: \n");
    tissue_type = auto_detect_tissue_type(path_to_db_file = custom_marker_file, 
                                          seuratObject = seurat_object, 
                                          scaled = scaled, assay = assay)
    rownames(tissue_type)=NULL
    tissue_type=tissue_type$tissue[1]
  } else {
    tissue_type = known_tissue_type
  }
  
  # Prepare gene sets
  gs_list = gene_sets_prepare(custom_marker_file, tissue_type)
  
  data_type <- if (scaled) "scale.data" else "counts"  
  package_type <- data_type %in% names(attributes(seurat_object[[assay]]))
  
  # Calculate scType scores
  if(package_type){
    
    print("Using Seurat v4 object")
    es.max = sctype_score(scRNAseqData = slot(seurat_object[[assay]], data_type),
                          scaled = TRUE,gs = gs_list$gs_positive, 
                          gs2 = gs_list$gs_negative)   
    
  } else{
    
    print("Using Seurat v5 object")
    
    if (data_type == "scale.data") {
      scRNAseqData <- seurat_object[[assay]]$scale.data
    } else {
      scRNAseqData <- seurat_object[[assay]]$counts
    }
    
    es.max = sctype_score(scRNAseqData = as.matrix(scRNAseqData),
                          scaled = TRUE,gs = gs_list$gs_positive, 
                          gs2 = gs_list$gs_negative)       
  }
  
  # Extract top cell types for each cluster
  cL_resutls = do.call("rbind", lapply(unique(seurat_object@meta.data$seurat_clusters), function(cl){
    es.max.cl = sort(rowSums(es.max[ ,rownames(seurat_object@meta.data[seurat_object@meta.data$seurat_clusters==cl, ])]), decreasing = !0)
    head(data.frame(cluster = cl, type = names(es.max.cl), scores = es.max.cl, ncells = sum(seurat_object@meta.data$seurat_clusters==cl)), 10)
  }))
  sctype_scores = cL_resutls %>% group_by(cluster) %>% top_n(n = 1, wt = scores)  
  # set low-confident (low ScType score) clusters to "unknown"
  sctype_scores$type[as.numeric(as.character(sctype_scores$scores)) < sctype_scores$ncells/4] = "Unknown"
  seurat_object_res=seurat_object
  seurat_object_res@meta.data[name] = ""
  for(j in unique(sctype_scores$cluster)){
    cl_type = sctype_scores[sctype_scores$cluster==j,]; 
    seurat_object_res@meta.data[seurat_object_res@meta.data$seurat_clusters == j,name] = as.character(cl_type$type[1])
  }
  if(plot){
    plot_ = DimPlot(seurat_object_res, reduction = "umap", group.by = name)   
    print(plot_)
  }
  text_=paste("New metadata added: ",name)
  print(text_)
  return(seurat_object_res)
}

