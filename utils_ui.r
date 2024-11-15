
library(outliers)

rm_outliers_from_df <- function(df, cols){
  for(col in cols){
    outliers_idx <- which( outlier(df[, col]) )
  }
}

# ##* *************************
# library(DoubletFinder)
# 
# my_paramSweep <- function (seu, PCs = 1:10, sct = FALSE, num.cores = 1) 
# {
#   require(Seurat)
#   require(fields)
#   require(parallel)
#   pK <- c(5e-04, 0.001, 0.005, seq(0.01, 0.3, by = 0.01))
#   pN <- seq(0.05, 0.3, by = 0.05)
#   min.cells <- round(nrow(seu@meta.data)/(1 - 0.05) - nrow(seu@meta.data))
#   pK.test <- round(pK * min.cells)
#   pK <- pK[which(pK.test >= 1)]
#   orig.commands <- seu@commands
#   if (nrow(seu@meta.data) > 10000) {
#     real.cells <- rownames(seu@meta.data)[sample(1:nrow(seu@meta.data), 
#                                                  10000, replace = FALSE)]
#     data <- seu@assays$RNA@counts[, real.cells]
#     n.real.cells <- ncol(data)
#   }
#   if (nrow(seu@meta.data) <= 10000) {
#     real.cells <- rownames(seu@meta.data)
#     data <- seu@assays$RNA$counts
#     n.real.cells <- ncol(data)
#   }
#   if (num.cores > 1) {
#     require(parallel)
#     cl <- makeCluster(num.cores)
#     output2 <- mclapply(as.list(1:length(pN)), FUN = parallel_paramSweep, 
#                         n.real.cells, real.cells, pK, pN, data, orig.commands, 
#                         PCs, sct, mc.cores = num.cores)
#     stopCluster(cl)
#   }
#   else {
#     output2 <- lapply(as.list(1:length(pN)), FUN = parallel_paramSweep, 
#                       n.real.cells, real.cells, pK, pN, data, orig.commands, 
#                       PCs, sct)
#   }
#   sweep.res.list <- list()
#   list.ind <- 0
#   for (i in 1:length(output2)) {
#     for (j in 1:length(output2[[i]])) {
#       list.ind <- list.ind + 1
#       sweep.res.list[[list.ind]] <- output2[[i]][[j]]
#     }
#   }
#   name.vec <- NULL
#   for (j in 1:length(pN)) {
#     name.vec <- c(name.vec, paste("pN", pN[j], "pK", pK, 
#                                   sep = "_"))
#   }
#   names(sweep.res.list) <- name.vec
#   return(sweep.res.list)
# }


##* *************************
##* UI Tabs
##* *************************


##* **********
tab_quickStart <- tabItem(
  tabName = "quickStart",
  h2("Quick Start"),
  p("This is the user guide for the scRNA-seq Data Analysis platform."),
  p("Instructions on how to use the application:"),
  tags$ol(
    tags$li("Select the dimensionality reduction method from the dropdown."),
    tags$li("Select the cluster to view from the dropdown."),
    tags$li("Click the 'Update View' button to refresh the plots and tables.")
  ),
  p("For further details, please refer to the documentation.")
)



##* **********
tab_dashboard <- tabItem(
  tabName = "dashboard", fluidRow(
    box(title = "Controls",selectInput("reduction", "Select Dimensionality Reduction Method:",
                                       choices = c("PCA", "UMAP", "tSNE")), actionButton("update", "Update View") ),
    
    box(title = "Cluster Data", dataTableOutput("clusterTable") )
  )
)



##* **********
tab_load <- tabItem(
  tabName = "load",
  h2("Data Loading"),
  p("Different formats of input are supported."),
  br(),
  
  # 添加具体的UI元素，如图表和表格
  tabBox(
    width = 12,
    tabPanel("10X", 
           
             fluidRow(
               column(6, 
                      tags$div(
                        tags$p(style = "font-size: 18px; font-weight: bold; text-decoration: underline;", "Select Files"),
                      ),
                      br(),
                      
                      h4("1.Select data files:"),
                      shinyDirButton(id = "load_10x_select_folder", label = "Select Folder", "Select Folder:"),
                      verbatimTextOutput("load_10x_selected_folder"),
                      # verbatimTextOutput("load_10x_selected_folder_content"),
                      
                      h4("2.Sample metatable (optional):"),
                      shinyFilesButton(id = 'load_10x_select_samplemeta', label = "Select File", "Select File:", multiple = FALSE),
                      verbatimTextOutput("load_10x_selected_samplemeta"),
                      
                      h4("3.Define project directory:"),
                      shinyDirButton(id = 'load_10x_define_output_dir', label = "Select Folder", "Select Folder:", multiple = FALSE),
                      verbatimTextOutput("load_10x_selected_output_dir"),
                      
                      br(),
                      shinyjs::useShinyjs(),
                      actionButton("load_10x_submit", "Submit", class = "btn-danger")
                      
               ), # left-column end
               
               column(6, style = "height: 550px;",
                      tags$div(
                        tags$p(style = "font-size: 16px; font-weight: bold; text-decoration: underline;", "Processing Log"),
                      ),  # tags$div end. 
                      
                      
                      div(
                        id = 'load_10_submit_log-container',
                        style = "height: 500px; overflow-y: scroll; border: 1px solid #ccc;",
                        textOutput("load_10x_submit_log")  
                        )
                      
               ) # right-column end.
             ) # fluidRow end. 
             
    ), # end of tabPanel: 10X
    
    tabPanel("GEX", 
             tags$div(
               tags$p(style = "font-size: 18px; font-weight: bold;text-decoration: underline;", "Hard-cutoff method")
             ),
             br(),
             checkboxGroupInput("checkboxes2", "Choose options:",
                                choices = c("Option A", "Option B", "Option C")),
             dateRangeInput("dates2", "Select date range:"),
             plotOutput("plot2")
    ), # end of tabPanel: GEX
    
    tabPanel("SeuratObject", 
             tags$div(
               tags$p(style = "font-size: 16px; font-weight: bold;text-decoration: underline;", 
                      "Start from SeuratObject to save time.")
             ),
             br(),
             checkboxGroupInput("checkboxes2", "Choose options:",
                                choices = c("Option A", "Option B", "Option C")),
             dateRangeInput("dates2", "Select date range:"),
             plotOutput("plot2")
    ), # end of tabPanel: SeuratObject
    
    tabPanel("Interoperability", 
             tags$div(
               tags$p(style = "font-size: 16px; font-weight: bold;text-decoration: underline;", 
                      "Conversion between Seurat objects, SingleCellExperiment objects, and anndata objects.")
             ),
             br(),
             
             checkboxGroupInput("checkboxes2", "Choose options:",
                                choices = c("Option A", "Option B", "Option C")),
             dateRangeInput("dates2", "Select date range:"),
             plotOutput("plot2")
    ) # end of tabPanel: Interoperability
  ) # end of tabBox
) # end of tabItem



##* *****
## tab_qc page

tab_qc <- tabItem(
  tabName = "qc",
  h2("Quality Controls"),
  p("This section provides tools for A series of quality assessment indicators for the scRNA-seq data."),
  br(),
  
  # 添加具体的UI元素，如图表和表格
  tabBox(
    width = 12,
    tabPanel("Overview", 
                 fluidRow(
                      column(3, 
                             actionButton("run_qc_overview", "Show Me !", class = "btn-danger"),
                             actionButton("download_qc_overview", "Download", class = "btn-success")
                            )
                 ),
                 br(),
                 uiOutput("sce_meta_overview")
             
            ), # end of tabPanel
    
    tabPanel("Cells, Genes & UMIs", 
             fluidRow(
                  column(3, 
                     actionButton("run_qc_cell_gene_umi", "Show Me !", class = "btn-danger"),
                     actionButton("download_qc_cell_gene_umi", "Download", class = "btn-success")
                     ),
                  column(4, 
                     uiOutput("sample_selector_genes_vs_umis")
                    )
                    ),
             br(),
             
             # uiOutput("sample_selector_summary_plot"),
             
             fluidRow(
                   column(width = 6, 
                          box(title = "# of Genes vs. # of UMIs", status = 'info', solidHeader = TRUE, width = 12, 
                              plotOutput("selected_plot_genes_vs_umis")
                          )), # summaryBySample-column-2
                   
                   column(width = 6, 
                          box(title = "Mito Genes and UMIs", status = 'info', solidHeader = TRUE, width = 12, 
                              plotOutput("selected_plot_mito_vs_umis")
                          ))
                      )
             ), # end of tabPanel: 
    
    tabPanel("Between-sample correlation", 
             fluidRow(
               column(3, 
                  actionButton("run_qc_sample_corr", "Show Me !", class = "btn-danger"),
                   actionButton("download_qc_sample_corr", "Download", class = "btn-success")
               )
             ),
             br(),
             
             fluidRow(
                     column(width = 6, 
                            box(title = "Between-sample correlation", status = 'info', solidHeader = TRUE, width = 12, 
                                plotOutput("plt_merged_sample_correlation")
                            ))
                      )
    ), # end of tabPanel
    
    tabPanel("Doublet", 
             fluidRow(
               column(3, 
                   actionButton("run_qc_doublet", "Show Me !", class = "btn-danger"),
                   actionButton("download_qc_doublet", "Download", class = "btn-success")
                   ),
               column(4, 
                    uiOutput("sample_selector_doublet")
                  )
             ),
             br(),
             
             # uiOutput("sample_selector_pred_doublet"),
             fluidRow(
                     column(width = 6,
                            box(title = "Doublets in Clusters", status = 'info', solidHeader = TRUE, width = 12,
                                plotOutput("plt_doublet")
                            )) #
                   )
    ), # end of tabPanel
    
    tabPanel("Cell cycle", 
             fluidRow(
               column(3, 
                   actionButton("run_qc_cellcycle", "Show Me !", class = "btn-danger"),
                   actionButton("download_qc_cellcycle", "Download", class = "btn-success")
                   ),
               column(4, 
                   uiOutput("sample_selector_cellcycle")
               )
             ),
             br(),
             
             # uiOutput("sample_selector_pred_cellcycle"),
             fluidRow(
                     column(width = 6,
                            box(title = "Cell Cycle in Clusters", status = 'info', solidHeader = TRUE, width = 12,
                                plotOutput("plt_cellcycle")
                            ))
                     
                     # column(width = 6, 
                     #        box(title = "Cyclin expression", status = 'info', solidHeader = TRUE, width = 12, 
                     #            plotOutput("plt_cyclin_heatmap")
                     #        ))
                   )
    ) # end of tabPanel
  ) # end of tabBox
) # end of tabItem


##* ********************
tab_filter <- tabItem(
  tabName = "filter",
  h2("Pre-processing and Filtration (Customizable)"),
  p("Combination of Filtering Criteria and Customizable Filtering for Screening High-Quality Single-Cell Transcriptomes."),
  
  # 添加具体的UI元素，如图表和表格
  tabBox(
    title = "", width = 12,
    tabPanel("Fixed", 
             
             fluidRow(
               column(6,
                      tags$div(
                        tags$p(style = "font-size: 16px; font-weight: bold; text-decoration: underline;", "Filters"),
                        
                      ),
                      numericInput("filter_fix_min_umis", 
                                   "Min number of UMIs for cells:", 
                                   value = 0, 
                                   min = 0, 
                                   max = 100),
                      
                      numericInput("filter_fix_min_features", 
                                   "Min number of features for cells:", 
                                   value = 0, 
                                   min = 0, 
                                   max = 100),
                      
                      sliderInput("filter_fix_mitoratio", 
                                  "Cutoff of mitochondrial percentage:", 
                                  min = 0, 
                                  max = 100,
                                  value = 20),
                      
                      numericInput("filter_fix_max_features", 
                                   "Max number of features for cells:", 
                                   value = 6000, 
                                   min = 0, 
                                   max = 100),
                      
                      selectInput("filter_fix_rm_doublet", 
                                  "Whether to remove doublets:",
                                  choices = c("Yes", "No"), 
                                  selected = "Yes"
                      ),
                      
                      selectInput("filter_fix_integrate", 
                                  "Whether to integrate individual libraries:",
                                  choices = c("Yes", "No"), 
                                  selected = "Yes"
                      ),
                      
                      
                      actionButton("filter_fix_submit", "Submit", class = "btn-danger")
                      
                      ), # left-column end

               column(6, style = "height: 550px;",
                      tags$div(
                        tags$p(style = "font-size: 16px; font-weight: bold; text-decoration: underline;", "Filter Log"),
                      ),  # tags$div end. 
                      
                      div(
                        id = 'filter_fix_submit_log-container',
                        style = "height: 500px; overflow-y: scroll; border: 1px solid #ccc;",
                        textOutput("filter_fix_submit_log")  
                      )
                      
               ) # right-column end.
               
             ) # fluidRow end. 
             
             ), # end of tabPanel
    
    tabPanel("Customized", 
             
             checkboxGroupInput("checkboxes2", "Choose options:",
                                choices = c("Option A", "Option B", "Option C")),
             dateRangeInput("dates2", "Select date range:"),
             plotOutput("plot2")
             )
  ) # tabBox end
  
)


##* ~~~~~~~~~~~~~~~~

tab_find_marker <- tabItem(
  tabName = "find_marker",
  h2("Differential expression and functional enrichment"),
  p("This section provides tools for analyzing clusters identified in the scRNA-seq data."),
  
  # 添加具体的UI元素，如图表和表格
  tabBox(title = "", width = 12,
    
    tabPanel("visCluster",
             fluidRow(
                 box(
                   title = "Clustering",
                   status = "primary", solidHeader = TRUE, width = 12, #height = '200px',
                   style = "height: 550px; overflow-y: auto;",
                   div(style = "position: absolute; left: 10px;",
                       selectInput("find_marker_cell_cluster_plot_method", "",
                                   width = "140px",
                                   choices = c("UMAP", "PCA"), 
                                   selected = "UMAP"
                       )
                   ),
                   plotOutput("find_marker_cell_cluster_plot")
                 )
             )
               
               
    ), 
    
    tabPanel("Markers", 
             fluidRow(
                box(
                  title = "Cluster Markers",
                  status = "primary", solidHeader = TRUE, width = 12, 
                  style = "height: 550px; overflow-y: auto;",
                  actionButton("find_marker_submit_allpairs", "Find-All", class = "btn-danger"),
                  dataTableOutput("find_marker_table_genes")
                )
             )
               
    ),
    
    tabPanel("Biological Functions", 
             fluidRow(
                box(
                  title = "Cluster-enriched Function",
                  status = "primary", solidHeader = TRUE, width = 12, 
                  style = "height: 550px; overflow-y: auto;",
                  # div(style = "position: absolute; top: 30px; left: 10px;",
                  #     uiOutput("find_marker_functermdb_selector")
                  # ),
                  dataTableOutput("find_marker_table_functions")
                )
                
             )
               
    )
  ) # tabBox end.
  
)


##* ~~~~~~~~~~~~~~~~

tab_annotatecell <- tabItem(
  tabName = "annotatecell",
  h2("Cell Type Annotation"),
  p("This section provides tools for analyzing clusters identified in the scRNA-seq data.")
  # 添加具体的UI元素，如图表和表格
  # plotlyOutput("clusterAnalysisPlot")
)




##* ~~~~~~~~~~~~~~~~

tab_infercnv <- tabItem(
  tabName = "infercnv",
  h2("Differential Expression Analysis"),
  p("This section provides tools for analyzing clusters identified in the scRNA-seq data.")
  # 添加具体的UI元素，如图表和表格
  # plotlyOutput("clusterAnalysisPlot")
)




##* ~~~~~~~~~~~~~~~~

tab_ccc <- tabItem(
  tabName = "ccc",
  h2("Cell-Cell Communication"),
  p("This section provides tools for performing differential expression analysis between different clusters or conditions.")
  # 添加具体的UI元素，如图表和表格
  # plotlyOutput("diffExpPlot")
)




##* ~~~~~~~~~~~~~~~~

tab_trajectory <- tabItem(
  tabName = "trajectory",
  h2("Trajectory Analysis"),
  p("This section provides tools for analyzing clusters identified in the scRNA-seq data.")
  # 添加具体的UI元素，如图表和表格
  # plotlyOutput("clusterAnalysisPlot")
)




##* ~~~~~~~~~~~~~~~~

tab_predGRN <- tabItem(
  tabName = "iger",
  h2("Infer Gene Expression Regulation Network"),
  p("This section provides tools for analyzing clusters identified in the scRNA-seq data.")
  # 添加具体的UI元素，如图表和表格
  # plotlyOutput("clusterAnalysisPlot")
  
)



##* ~~~~~~~~~~~~~~~~

tab_userguide <- tabItem(
  tabName = "userGuide",
  h2("User Guide"),
  p("This is the user guide for the scRNA-seq Data Analysis platform."),
  p("Instructions on how to use the application:"),
  tags$ol(
    tags$li("Select the dimensionality reduction method from the dropdown."),
    tags$li("Select the cluster to view from the dropdown."),
    tags$li("Click the 'Update View' button to refresh the plots and tables.")
  ),
  p("For further details, please refer to the documentation.")
)



##* ~~~~~~~~~~~~~~~~

menu_dashboard <- menuItem("Dashboard", tabName = 'dashboard', icon = icon("dashboard"))

##* ~~~~~~~~~~~~~~~~

menu_quickStart <- menuItem("Quick Start", tabName = 'quickStart', icon = icon("rocket"))

##* ~~~~~~~~~~~~~~~~

menu_pipe <- menuItem("Pipeline", icon = icon("sitemap"), startExpanded = TRUE,
                      menuSubItem("Data Loading", tabName = 'load', icon = icon("arrow-up")),
                      menuSubItem("Quality Control", tabName = "qc", icon = icon('gear')),
                      menuSubItem("Pre-process & Filter", tabName = "filter", icon = icon("filter")),
                      
                      menuSubItem("Differential Analysis", tabName = 'find_marker', icon = icon("sort")),
                      menuSubItem("Annotate Cell Type", tabName = "annotatecell", icon = icon("arrow-up-a-z")),
                      menuSubItem("Identify CNV", tabName = "infercnv", icon = icon("shuffle")),
                      
                      menuSubItem("Cell-cell Communication", tabName = 'ccc', icon = icon("recycle")),
                      menuSubItem("Trajectory Analysis", tabName = 'trajectory', icon = icon("up-down-left-right")),
                      menuSubItem("Predict Gene regulation network", tabName = 'predGRN', icon = icon("arrows-turn-to-dots"))
)

##* ~~~~~~~~~~~~~~~~

menu_userguide <- menuItem("User Guide", tabName = 'userGuide', icon = icon("book"))



##* ~~~~~~~~~~~~~~~~

sidebar_logo <- div(
  style = "position: absolute; bottom: 0; width: 100%; padding: 10px; text-align: center;",
  tags$img(src = "silk-logo.png", height = "150px"),  # 替换为实际的Logo路径
  br(),
  tags$p("Version: 1.0.0"),
  tags$p("Contact: luanyz_leo@outlook.com")
)


##* ~~~~~~~~~~~~~~~~
##* 
header_dropdown <- dropdownMenu(type = "notifications", icon = icon("question-circle"),
                                headerText = "Help", badgeStatus = NULL,
                                notificationItem(text = "User Guide", 
                                                 icon = icon('book'), 
                                                 href="#quickStart"),
                                
                                notificationItem(
                                  text = "12 items delivered",
                                  icon = icon("truck"),
                                  status = "success"
                                       )
)


##* ~~~~~~~~~~~~~~~~
##* 
header_search <- tags$li(class = "dropdown",
                         tags$form(class = "navbar-form navbar-left",
                                   tags$div(class = "input-group",
                                            tags$input(type = "text", class = "form-control", placeholder = "Search..."),
                                            tags$span(class = "input-group-btn",
                                                      tags$button(class = "btn btn-default", type = "button", icon("search"))
                                            )
                                   )
                         )
)
