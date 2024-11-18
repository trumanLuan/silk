library(clusterProfiler) # for functiontal enrichment
library(msigdbr)

msigdbr_show_species()

## function
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

