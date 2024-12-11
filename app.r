 # app.R 

source("libraries.r")
source("utils_ui.r")
source("utilis_server_function.r")

## ------------------------
## UI
##------------------------ 

# Set up UI components
## header
header <- dashboardHeader(title = "SILK: scRNA-seq Interactive anaLysis toolKit", 
                          titleWidth = 400, 
                          disable = FALSE,
                          header_search,
                         header_dropdown
                          )

## sidebar region,  for navigation
sidebar <- dashboardSidebar(
  tags$head(
    tags$style(HTML("
        /* 固定sidebar的CSS */
        .main-sidebar { 
          position: fixed; 
          max-height: 100%;
          overflow: auto;
        }
      "))
  ),
  
  width = 300,
  sidebarMenu(
    
    menu_quickStart,
    menu_dashboard, 
    menu_pipe,
    menu_userguide
    
  ),

  sidebar_logo
  
)  

## main body for content
body <- dashboardBody(
  tags$head(
    tags$style(HTML("
        .row {
          margin-left: -10px;
          margin-right:-10px;
        }
        .col-sm-4 {
          padding-left: 1px;
          padding-right: 1px;
        }
        .box {
          margin-bottom: 5px;
        }
        
        /* 定制sidebar menu主题和格式，主要是针对menuSubItem */
       .sidebar-menu .treeview-menu {
        margin-left: 5px !important; /* Adjust this value to change the indent */
       }
      
       .sidebar-menu .treeview-menu > li > a {
          padding-left: 30px;
        }
        .sidebar-menu .treeview-menu > li > a::before {
          content: '';
          position: absolute;
          left: 20px;
          top: 0;
          bottom: 0;
          width: 2px;
          background: #3c8dbc;
        }
        .sidebar-menu .treeview-menu > li > a::after {
          content: '';
          position: absolute;
          left: 20px;
          top: 50%;
          width: 8px;
          height: 2px;
          background: #3c8dbc;
        }
        
        /* 去除 verbatimTextOutput 背景颜色 */
        pre {
          background-color: transparent;
          border: none;
          box-shadow: none;
          color: inherit; /* 保持文本颜色 */
        }
        
        /* 固定header */
        .main-header {
          position: fixed;
          width: 100%;
          top: 0;
          z-index: 1030;
        }
        .content-wrapper {
          margin-top: 50px; /* 调整这个值以匹配您的header高度 */
        }
      "))
  ), #css,自定义box等组件的layout
  
  tabItems(
    
    tab_quickStart,
    tab_dashboard, 
    tab_load,
    tab_qc,
    tab_filter,
    tab_find_marker,
    tab_annotatecell,
    # tab_infercnv,
    tab_ccc,
    tab_trajectory,
    # tab_predGRN,
    tab_userguide
    
  )
)  

ui <- dashboardPage(
  header, 
  sidebar, 
  body)

## ------------------------
## Server Function
##------------------------ 

server <- function(input, output, session){
  
##* **********
  ## ---- for tab_load page
  # 指定常见的 macOS 目录
  volumes <- c(Home = fs::path_home(), Root = "/")
  
  ## define path root in win OS
  # volumes <- getVolumes()
  
  # ## initialize shinyjs
  # shinyjs::useShinyjs()
  
  # create reactiveValues to save shared R data objects
  rv <- reactiveValues(data = list(), 
                       data_dir=NULL, 
                       output_dir = NULL, 
                       log = NULL,
                       sce_meta_summary = NULL,
                       data.now = NULL,
                       data.combined = NULL )
  
  # initialize shinyFiles
  shinyDirChoose(input, "load_10x_select_folder", roots = volumes, session = session)
  shinyFileChoose(input, "load_10x_select_samplemeta", roots = volumes, session = session)
  shinyDirChoose(input, "load_10x_define_output_dir", roots = volumes, session = session)

  # select folders and files
  output$load_10x_selected_folder <- renderText({
    req(input$load_10x_select_folder)
    selected_folder <- parseDirPath(volumes, input$load_10x_select_folder)
    rv$data_dir <- selected_folder
    paste("Data Under:", selected_folder )
    
  })
  
  output$load_10x_selected_output_dir <- renderText({
    req(input$load_10x_define_output_dir)
    selected_folder <- parseDirPath(volumes, input$load_10x_define_output_dir)
    rv$output_dir <- selected_folder
    paste("Output Root:", selected_folder )
    
  })
  
  output$load_10x_selected_samplemeta <- renderText({
    req(input$load_10x_select_samplemeta)
    selected_file <- parseDirPath(volumes, input$load_10x_select_samplemeta)
    paste("Output Root:", selected_file )
    
  })
  
  ## //
  
  
  ## actions for submit button, SeuratObject submit 
  observeEvent(input$load_seurat_submit, {
      # 打印表单输入的基本信息
      output$load_seurat_form_data <- renderPrint({
        req(input$load_seurat_file) # 如果 fileInput 为空则阻止运行
        input$load_seurat_file
      })
      
      # 清空加载结果区域
      output$load_seurat_submit_result <- renderPrint({})
      
      # 检查是否有文件上传
      if (is.null(input$load_seurat_file)) {
        output$load_seurat_submit_result <- renderPrint({
          "错误: 未选择任何文件，请上传一个 .rds 文件后再提交！"
        })
      } else {
        # 获取文件路径
        file_path <- input$load_seurat_file$datapath
        
        # 尝试读取文件
        tryCatch({
          rv$data <- readRDS(file_path)
          
          # 检查对象是否为 SeuratObject
          if (class(rv$data.combined)[1] == "Seurat") {
            output$load_seurat_submit_result <- renderPrint({
              "数据加载成功！文件包含一个有效的 Seurat 对象。"
            })
          } else {
            output$load_seurat_submit_result <- renderPrint({
              "错误: 读取的文件不包含有效的 Seurat 对象！"
            })
          }
        }, error = function(e) {
          output$load_seurat_submit_result <- renderPrint({
            paste("错误: 加载文件失败 -", e$message)
          })
        })
      }
  })
    
  ### define actions for button: 10X submit
  observeEvent(input$load_10x_submit, {
    
    withCallingHandlers({
        shinyjs::html("load_10x_submit_log", "") ## 
        
        ## check folders and files
        if(is.null(rv$data_dir) || rv$data_dir == ""){
            message("Error: No data folder selected.")
        }else{
            subfolders <- list.dirs(rv$data_dir, full.names = TRUE, recursive = FALSE)
            message("Number of samples in the selected folder: ", length(subfolders))
        }
        
        if(is.null(rv$output_dir) || rv$output_dir == ""){
            message("Error: No output root folder selected.\n")
        }else{
            dir.create(file.path(rv$output_dir, "1_Rds"))
            dir.create(file.path(rv$output_dir, "2_Reports"))
            dir.create(file.path(rv$output_dir, "3_DEA"))
            dir.create(file.path(rv$output_dir, "4_CNV"))
            dir.create(file.path(rv$output_dir, "5_CCC"))
            dir.create(file.path(rv$output_dir, "6_Trajectory"))
            dir.create(file.path(rv$output_dir, "7_GRN"))
            message(Sys.time(), " SUCCESS: output subfolders prepared.")
        }
      
      ## start load data
        message(Sys.time(), ' Start to load data.')
        for(i in seq_along(subfolders) ){
            rv$data[[i]] <- Seurat::CreateSeuratObject( Seurat::Read10X(subfolders[i])  )
            message("......", basename(subfolders[i]), ' loaded')
        }
        message('...All samples loaded. ')
        names(rv$data) <- basename(subfolders)
        
      ## start to pre-process data
        message(Sys.time(), ' Start to pre-process data.')
        rv$data <- lapply(X = rv$data, FUN = function(x){
            x <- NormalizeData(x)
            x <- FindVariableFeatures(x, selection.method = "vst", nfeatures = 2000)
            x <- ScaleData(x)
            x <- RunPCA(x)
            x <- RunUMAP(x, dims = 1:30)
            
            x <- FindNeighbors(x, dims = 1:30, reduction = "pca")
            x <- FindClusters(x, cluster.name = 'seurat_cluster')
        })
        
        message(Sys.time(), '...Pre-processing finished.')
        
        ## count mito-genes and renew meta.data
        message(Sys.time(), ' Count mito-genes and renew meta.data.')
        rv$data = lapply(rv$data, FUN = function(x){
            AddMetaData(x, metadata = PercentageFeatureSet(x, pattern = "^MT-"), col.name = 'mito_percent')
        })
        
        ## find doublets and renew meta.data
        message(Sys.time(), ' Find doublets and renew meta.data.')
        rv$data <- lapply(X = rv$data, FUN = function(x){
            x <- as.SingleCellExperiment(x)
            x <- scDblFinder(x)
            x <- as.Seurat(x)
          
        })
        
        ## Predict cell cycle phase and renew meta.data
        message(Sys.time(), ' Predict cell cycle phase and renew meta.data.')
        rv$data <- lapply(X = rv$data, FUN = function(x){
            x <- Seurat::CellCycleScoring(x, s.features = cc.genes$s.genes, g2m.features = cc.genes$g2m.genes, set.ident = TRUE)
          
        })
        
        saveRDS(rv$data, file = file.path(rv$output_dir,  '1_Rds/seurat.raw.processed.rds'))
        
        message(Sys.time(), ' SUCCESS: All preprocessing steps finished.')
      
    },
    message = function(m) {
      shinyjs::html(id = "load_10x_submit_log", html = paste0(m$message, "<br>"), add = TRUE)
      shinyjs::runjs("var objDiv = document.getElementById('load_10x_submit_log-container'); objDiv.scrollTop = objDiv.scrollHeight;")
    }
    ) # end of withCallingHandlers
  })

  ## ---- for tabItem-qc
  ##* ******************************
  ##* 
  observeEvent(input$run_qc_overview, {
      if(length(rv$data) == 0){
          output$sce_meta_overview <- renderUI({
              tags$p("Error: Data not found. Please upload the data before proceeding.",
                     style = "color: red; font-weight: bold;")
          })
      }else{
          rv$sce_meta_summary <- report_summary_sce_metadata(rv$data)
      }
  })
  
  output$sce_meta_overview <- renderUI({
      if(length(rv$data) ==0 || is.null(rv$sce_meta_summary)){
          tags$p("No single-cell transcriptome datasets were detected.",
                 style = "color: red; font-weight: bold;")
      } else {
         DT::datatable(
              rv$sce_meta_summary, 
              options = list(dom = 't', scrollX = TRUE, scrollY = '150px'),
              style = 'bootstrap', 
              class = "cell-border stripe",
              rownames = FALSE 
        )
      }
      
  })
  
  ## downlaod
  output$download_qc_overview <- downloadHandler(
      filename = function() { paste("sce-meta-summary-", Sys.Date(), ".tsv", sep="") },
      content = function(file) { write.table(rv$sce_meta_summary, file, row.names = FALSE, quote=F,sep='\t') }
  )
  
  
##* ******************************
  ## region: cells, genes, and umis by sample
  ## sample selector
  output$sample_selector_genes_vs_umis <- renderUI({
      req(rv$data)
      div(
        style = "display: flex; align-items: center;", # 在同一行显示，并垂直居中对齐
        tags$label("", style = "margin-right: 10px;"), # 添加一些右边距以与下拉框分隔开
        selectInput(inputId = "selected_sample_genes_vs_umis", 
                    label = NULL, 
                    choices = c("please select one sample" = ""),
                    selected = ""
                      )
      )
  })
  
  observeEvent(input$run_qc_cell_gene_umi,{
      # activate the sample selector.
      updateSelectInput(
          session, 
          'selected_sample_genes_vs_umis',
          choices = names(rv$data),
          selected = names(rv$data)[1]
      )
  })
  
  ## id = selected_plot_genes_vs_umis
  output$selected_plot_genes_vs_umis <- renderPlot({
    if(length(rv$data) == 0){
        plot.new()
        text(0.5, 0.5, "No scRNA-seq datasets\n were detected.", col = "red", font = 2, cex = 1.5)
    }else{
        data.now <- rv$data[[input$selected_sample_genes_vs_umis]]@meta.data
        car::scatterplot(x = data.now$nCount_RNA, xlab = 'nCount_RNA',
                         y = data.now$nFeature_RNA, ylab = "nFeature_RNA",
                         pch= 19, col = 1, smooth = FALSE, regLine = FALSE)
    }
  })
  
  
  ## id = selected_plot_mito_vs_umis
  output$selected_plot_mito_vs_umis <- renderPlot({
    if(length(rv$data)==0){
        plot.new()
        text(0.5, 0.5, "No scRNA-seq datasets\n were detected.", col = "red", font = 2, cex = 1.5)
    }else{
        data.now <- rv$data[[input$selected_sample_genes_vs_umis]]@meta.data
        
        car::scatterplot(x = data.now$nCount_RNA, xlab = 'nCount_RNA',
                         y = data.now$mito_percent, ylab = "Mito_percent",
                         pch= 19, col = 1, smooth = FALSE, regLine = FALSE)
    }    
  })
  

  ##* ******************************
  ## tabItem region:  between-sample correlation
  observeEvent(input$run_qc_sample_corr, {
      req(rv$data)
      
      output$plt_merged_sample_correlation <- renderPlot({
        if(length(rv$data) == 0 ){
            plot.new()
            text(0.5, 0.5, "No scRNA-seq datasets\n were detected.", col = "red", font = 2, cex = 1.5)
            
        }else{
            data.now <- rv$data
            data.now <- lapply(data.now, function(x) Seurat::AverageExpression(x,group.by = "orig.ident")$RNA)
            data.now <- do.call(cbind, data.now)
            colnames(data.now) <- names(rv$data)
            data.now <- data.now[rowSums(data.now) > 0,]
            data.now <- log2(data.now+1)
            
            psych::pairs.panels(data.now, scale = TRUE, gap = 0, pch = 19, lm = FALSE, method = 'spearman')
        }
      })
  })
  

    ##* ******************************
  ## tabItem region:  doublet
  
  output$sample_selector_doublet <- renderUI({
      div(
          style = "display: flex; align-items: center;", # 在同一行显示，并垂直居中对齐
          tags$label("", style = "margin-right: 10px;"), # 添加一些右边距以与下拉框分隔开
          selectInput(inputId = "selected_sample_doublet", 
                      label = NULL, 
                      choices = c("please select one sample" = ""),
                      selected = ""
        )
    )
  })
  
  observeEvent(input$run_qc_doublet,{
    # activate the sample selector.
      updateSelectInput(
        session, 
        'selected_sample_doublet',
        choices = names(rv$data),
        selected = names(rv$data)[1]
    )
  })
  
  ## id = plt_doublet
  output$plt_doublet <- renderPlot({
    if(length(rv$data) == 0){
        plot.new()
        text(0.5, 0.5, "No scRNA-seq datasets\n were detected.", col = "red", font = 2, cex = 1.5)
    }else{
        data.now <- rv$data[[input$selected_sample_doublet]]
        Seurat::DimPlot(data.now,  reduction = 'UMAP',group.by = "scDblFinder.class")
    }
  })

  
    ##* ******************************
  ## tabItem region:  cellcycle
  
  ## sample selector
  output$sample_selector_cellcycle <- renderUI({
    req(rv$data)
    div(
      style = "display: flex; align-items: center;", # 在同一行显示，并垂直居中对齐
      tags$label("", style = "margin-right: 10px;"), # 添加一些右边距以与下拉框分隔开
      selectInput(inputId = "selected_sample_cellcycle", 
                  label = NULL, 
                  choices = c("please select one sample" = ""),
                  selected = ""
      )
    )
  })
  
  observeEvent(input$run_qc_cellcycle,{
    # activate the sample selector.
    updateSelectInput(
        session, 
        'selected_sample_cellcycle',
        choices = names(rv$data),
        selected = names(rv$data)[1]
    )
  })
  
  output$plt_cellcycle <- renderPlot({
    if(length(rv$data) == 0){
        plot.new()
        text(0.5, 0.5, "No scRNA-seq datasets\n were detected.", col = "red", font = 2, cex = 1.5)
    }else{
        data.now <- rv$data[[input$selected_sample_cellcycle]]
        DimPlot(data.now,  reduction = 'UMAP', group.by = "Phase")
    }
  })
  
  
  # output$plt_cyclin_heatmap <- renderPlot({
  #   if(length(rv$data)==0){
  #       plot.new()
  #       text(0.5, 0.5, "No scRNA-seq datasets\n were detected.", col = "red", font = 2, cex = 1.5)
  #   }else{
  #       data.now <- rv$data[[ input$selected_sample_cellcycle ]]
  #       a =as.SingleCellExperiment( data.now )
  #       cc.features <- c(cc.genes$s.genes, cc.genes$g2m.genes)
  #       in.features <- cc.features[cc.features %in% rownames(a)]
  #       scater::plotHeatmap(a, order_columns_by = 'Phase', features = in.features, scale = TRUE, exprs_values = "logcounts")
  #     
  #   }    
  # })
  
  
  
  ##* ******************************
  ##* ******************************
  ##*  tab_filter
  ##*  
  
  ### define actions for button: filter_fix_submit
  observeEvent(input$filter_fix_submit, {
    
    withCallingHandlers({
      shinyjs::html("filter_fix_submit_log", "") ## 
      
      ##* ***** check scRNAseq data input
      if(length(rv$data) == 0){
          message("no data were detected.")
      }else{
          # Log the filtering parameters
          message(Sys.time(), " 过滤参数如下:")
          message(Sys.time(), " Min number of UMIs for cells: ", input$filter_fix_min_umis)
          message(Sys.time(), " Min number of features for cells: ", input$filter_fix_min_features)
          message(Sys.time(), " Cutoff of mitochondrial percentage: ", input$filter_fix_mitoratio, "%")
          message(Sys.time(), " Max number of features for cells: ", input$filter_fix_max_features)
          message(Sys.time(), " Remove doublets: ", input$filter_fix_rm_doublet)
          message(Sys.time(), " Integrate individual libraries: ", input$filter_fix_integrate)
          
          message(Sys.time(), "Start filtering ...")
          # whether to remove doublets
          if(input$filter_fix_rm_doublet == 'Yes'){
            
              rv$data <- lapply(rv$data, FUN = function(x){
                subset(x, subset = nFeature_RNA > input$filter_fix_min_features &
                         nCount_RNA > input$filter_fix_min_umis &
                         mito_percent < input$filter_fix_mitoratio &
                         nFeature_RNA < input$filter_fix_max_features &
                         scDblFinder.class == "singlet"
                )
              })
          }else{
              rv$data <- lapply(rv$data, FUN = function(x){
                subset(x, subset = nFeature_RNA > input$filter_fix_min_features &
                         nCount_RNA > input$filter_fix_min_umis &
                         mito_percent < input$filter_fix_mitoratio &
                         nFeature_RNA < input$filter_fix_max_features
                )
              })
          }
          
          message(Sys.time(), " Filtering is finished.")
          
          ## whether to integrate
          if( input$filter_fix_integrate == "Yes"){
              message(Sys.time(), " Start integration...")
              
              rv$data <- lapply(X = rv$data, FUN = function(x){
                  x = NormalizeData(x)
                  x <- FindVariableFeatures(x, selection.method = "vst", nfeatures = 2000)
              })
              features <-  SelectIntegrationFeatures(object.list = rv$data)
              curr.anchors <- FindIntegrationAnchors(object.list = rv$data, anchor.features = features)
              rv$data.combined <- IntegrateData(anchorset = curr.anchors)
          }
          
          message(Sys.time(), " Integration is finished.")
          message(Sys.time(), " Start to process integrated data.")
          
          DefaultAssay(rv$data.combined) <- "integrated"
          # Run the standard workflow for visualization and clustering
          rv$data.combined <- ScaleData(rv$data.combined, verbose = FALSE)
          rv$data.combined <- RunPCA(rv$data.combined, npcs = 30, verbose = FALSE)
          rv$data.combined <- RunUMAP(rv$data.combined, reduction = "pca", dims = 1:30)
          rv$data.combined <- FindNeighbors(rv$data.combined, reduction = "pca", dims = 1:30)
          rv$data.combined <- FindClusters(rv$data.combined, resolution = 0.5)
          
          saveRDS(rv$data.combined, file = file.path(rv$output_dir,  '1_Rds/seurat.filtered.integrated.rds'))
          
          message(Sys.time(), " Processing is finished.")
          
          
      }
    },
    message = function(m) {
      shinyjs::html(id = "filter_fix_submit_log", html = paste0(m$message, "<br>"), add = TRUE)
      shinyjs::runjs("var objDiv = document.getElementById('filter_fix_submit_log-container'); objDiv.scrollTop = objDiv.scrollHeight;")
    }
    ) # end of withCallingHandlers
  })
  
  
  
  ##* ******************************
  ##* ******************************
  ##*  tab_find_marker
  ##* 
          
output$find_marker_cell_cluster_plot <- renderPlot({
    if(is.null(rv$data.combined) || length(rv$data.combined) == 0){
        plot.new()
        text(0.4, 0.5, "No combined dataset\n was detected.", col = "red", font = 2, cex = 1.5)
    }else{
        plt.reduction = tolower(input$find_marker_cell_cluster_plot_method)
        SCpubr::do_DimPlot(sample = rv$data.combined, reduction = plt.reduction)
    }
})
       

##* ******************************
## tab_find_marker: tabPanel Markers

observeEvent(input$find_marker_submit_allpairs,{
  if(is.null(rv$data.combined) || length(rv$data.combined) == 0){
    plot.new()
    text(0.4, 0.5, "No combined dataset\n was detected.", col = "red", font = 2, cex = 1.5)
  }else{
    output$find_marker_table_genes <- renderDataTable({
        Idents(rv$data.combined) <- "seurat_clusters"
        data.now <- FindAllMarkers(rv$data.combined, only.pos = TRUE, min.pct = 0.25)
        write.table(data.now, file.path(rv$output_dir, '3_DEA/sce_integrated_seurat.cluster_findAllMarkers.tsv'), quote = FALSE, sep = '\t', row.names = TRUE, col.names = TRUE)
        datatable(data.now)
        
    })
  }
})


##* ******************************
## tab_find_marker: tabPanel of 'Biological Functions'

observeEvent(input$show_function_table,{
  if(is.null(rv$data.combined) || length(rv$data.combined) == 0){
    plot.new()
    text(0.4, 0.5, "No combined dataset\n was detected.", col = "red", font = 2, cex = 1.5)
  }else{
    output$find_marker_table_functions <- renderDataTable({
      
      data.now <- read.table(file.path(rv$output_dir, '3_DEA/sce_integrated_seurat.cluster_findAllMarkers.tsv'), header=T, sep='\t', as.is=T)
      
      #output_dir<-"/Users/yizhaoluan/project/test_output"
      #data.now <- read.table(file.path(output_dir, '3_DEA/sce_integrated_seurat.cluster_findAllMarkers.tsv'), header=T, sep='\t', as.is=T)
      uniq.cluster <- unique(data.now$cluster)
      
      data.now.results <- NULL
      for(cluster.i in uniq.cluster){
        cat(cluster.i,'\n')
        cluster.genes <- subset(data.now, cluster == cluster.i)
        cluster.enrich <- enrichr_gsa_all_subcat(genes=cluster.genes$gene, species= "Homo sapiens", all_subcat = TRUE)
        data.now.results <- rbind(data.now.results, 
                                  cbind(data.frame(cluster=rep(cluster.i, nrow(cluster.enrich))), cluster.enrich) 
                                  )
      }
      
      write.table(data.now.results, file.path(rv$output_dir, '3_DEA/sce_integrated_seurat.cluster_markerFunctions.tsv'), quote = FALSE, sep = '\t', row.names = TRUE, col.names = TRUE)
      
      datatable(data.now.results)
    })
  }
})


##* ******************************
## tab_annotatecell: tabPanel of 'Input'
    
# 动态生成表单内容
output$annotcell_dynamic_form <- renderUI({
  # 根据用户选择动态生成内容
  switch(input$form_choice,
         "form_singler" = {
           tagList(
             # textInput("input_a1", "输入字段 A1："),
             selectInput("form_singler_selectref",
                         "Reference expression dataset:",
                         choices = c("BlueprintEncodeData",
                                     "DatabaseImmuneCellExpressionData",
                                     "HumanPrimaryCellAtlasData",
                                     "ImmGenData",
                                     "MonacoImmuneData",
                                     "MouseRNAseqData",
                                     "NovershternHematopoieticData"
                                     )
                         ),
             selectInput("form_singler_selectlabel",
                         "Label type:",
                         choices = c("label.main", "label.fine")
                        ),
             numericInput("form_singler_ncore", "N of Cores:", value = 4)
           )
         },

         "form_sctype" = {
           tagList(
             # dateInput("input_c1", "日期字段 C1："),
             selectInput("form_sctype_selectref",
                         "Reference marker database:",
                         choices = c("ScTypeDB_short",
                                     "ScTypeDB_full"
                         )
             ),
             selectInput("form_sctype_tissue",
                         "Tissue type:",
                         choices = c("Immune system", "Liver", "Pancreas", "Kidney", "Eye", "Brain", "Lung", "Adrenal",
                                     "Heart", "Intestine", "Muscle", "Placenta","Spleen", "Stomach", "Thymus"
                         )
             ),
             selectInput("form_sctype_scaleassay",
                         "If scale:",
                         choices = c("True", "False" )
                    )
             
           )
         }
  )
})


observeEvent(input$annotcell_input_submit,{
  form_data <- switch(input$form_choice,
                      "form_singler" = list(form_type = "SingleR", 
                                            form_singler_ref = input$form_singler_selectref, 
                                            form_singler_labeltype = input$form_singler_selectlabel, 
                                            form_singler_ncore = input$form_singler_ncore),
                      "form_sctype" = list(form_type = "ScType", 
                                           form_sctype_ref = input$form_sctype_selectref, 
                                           form_sctype_tissue = input$form_sctype_tissue,
                                           form_sctype_scaleassay = input$form_sctype_scaleassay ) )
  
  if(is.null(rv$data.combined) || length(rv$data.combined) == 0){
    output$annotcell_form_data <- renderPrint({ "No combined datasets was detected." })
  } else {
    output$annotcell_form_data <- renderPrint(form_data)
  }
  
  if(form_data$form_type == "SingleR"){
    
        if(form_data$form_singler_ref == "BlueprintEncodeData") {
          cell.ref <- BlueprintEncodeData(ensembl=F, cell.ont="nonna")
        }else if(form_data$form_singler_ref == "DatabaseImmuneCellExpressionData"){
          cell.ref <- DatabaseImmuneCellExpressionData(ensembl=F, cell.ont="nonna")
        }else if(form_data$form_singler_ref == "HumanPrimaryCellAtlasData"){
          cell.ref <- HumanPrimaryCellAtlasData(ensembl=F, cell.ont="nonna")
        }else if(form_data$form_singler_ref == "ImmGenData"){
          cell.ref <- ImmGenData(ensembl=F, cell.ont="nonna")
        }else if(form_data$form_singler_ref == "MonacoImmuneData"){ 
          cell.ref <- MonacoImmuneData(ensembl=F, cell.ont="nonna")
        }else if(form_data$form_singler_ref == "MouseRNAseqData"){ 
          cell.ref <- MouseRNAseqData(ensembl=F, cell.ont="nonna")
        }else if(form_data$form_singler_ref == "NovershternHematopoieticData"){
          cell.ref <- NovershternHematopoieticData(ensembl=F, cell.ont="nonna")
        }
        
        if(form_data$form_singler_labeltype == "label.main") label.type = "label.main"
        if(form_data$form_singler_labeltype == "label.fine") label.type = "label.fine"
        
        
        multicorePara <- BiocParallel::MulticoreParam(workers = as.integer(form_data$form_singler_ncore) )
        celltype.predict <- SingleR(test = GetAssayData(rv$data.combined), ref = cell.ref, labels = colData(cell.ref)[,label.type],
                                    clusters = rv$data.combined@meta.data$seurat_clusters, assay.type.test=1, BPPARAM=multicorePara)
        
        celltype.predict <- as.data.frame(celltype.predict)
        celltype.predict$cluster <- rownames(celltype.predict)
        rv$data.combined[["identity_singler"]] <- sapply(rv$data.combined@meta.data$seurat_clusters, function(x) subset(celltype.predict, cluster==x)$labels)
        
        saveRDS(rv$data.combined, file = file.path(rv$output_dir,  '1_Rds/seurat.filtered.integrated.singler_annot.rds'))
        
        output$annotcell_vis_clustering <- renderPlot({
          Idents(rv$data.combined) <- "identity_singler"
          SCpubr::do_DimPlot(sample = rv$data.combined, reduction = 'umap', label=T )
        })
        
  }else if(form_data$form_type == "ScType"){
      output$annotcell_vis_clustering <- renderPlot({
        "This method has not been defined."
      })
  }
  
})



##* ******************************
##** tab_annotatecell: tabPanel of 'Viewer'

observeEvent(input$annotcell_viewer_gene_submit,{
  if(is.null(rv$data.combined) || length(rv$data.combined) == 0){
    output$annotcell_vis_clustering_selectgene <- renderPrint({
      "No combined datasets was detected."
    })
  }else{
    
    input.gene <- input$annotcell_viewer_gene
    
    ## check whether input gene was included in current dataset
    if( toupper(input.gene) %in% (rownames(rv$data.combined@assays$RNA))){
        output$annotcell_vis_clustering_selectgene <- renderPlot({
            Seurat::FeaturePlot(rv$data.combined, features = toupper(input.gene))
        })
    }else if( tolower(input.gene) %in% (rownames(rv$data.combined@assays$RNA)) ){
        output$annotcell_vis_clustering_selectgene <- renderPlot({
          Seurat::FeaturePlot(rv$data.combined, features = tolower(input.gene))
        })
    }else{
        output$annotcell_vis_clustering_selectgene <- renderPlot({
          "The Gene not identified in your data."
        })
    }
  }
})


##* ******************************
##** tab_annotatecell: tabPanel of 'Report'
##* undetermined @2024/11/29
##* 




##* ******************************
##* tab_ccc module, tabpanel of 'Input'
##* 

output$ccc_dynamic_form <- renderUI({
  # 根据用户选择动态生成内容
  switch(input$ccc_form_choice,
         "form_cellchat" = {
           tagList(
             # textInput("input_a1", "输入字段 A1："),
             selectInput("form_cellchat_idents",
                         "Set the idents:",
                         choices = c("Cell clusters", "Cell type identity", 'Customized')
             ),
             selectInput("form_cellchat_refdb",
                         "Set the ligand-receptor interaction database:",
                         choices = c("CellChatDB.human", "CellChatDB.mouse", "Customized")
             ),
             numericInput("form_cellchat_ncore", "N of Cores:", value = 4)
           )
         },
         
         "form_scsignalr" = {
           tagList(
             # dateInput("input_c1", "日期字段 C1："),
             selectInput("form_scsignalr_idents",
                         "Set the idents:",
                         choices = c("Cell clusters", "Cell type identity", 'Customized' )
             ),
             selectInput("form_scsignalr_refdb",
                         "Set the ligand-receptor interaction database:",
                         choices = c("LRdb", "Customized")
             ),
             h4("Set a working directory for SingleCellSignalR:"),
             shinyDirButton(id = "form_scsignalr_workdir",
                         label = "SingleCellSignalR Working Dir",
                         title = "Set SingleCellSignalR Working Dir"
             )
             
           )
         }
  )
})


##* actionButton submit events
##* 

observeEvent(input$ccc_input_submit,{
  form_data <- switch(input$ccc_form_choice,
                      "form_cellchat" = list(form_type = "CellChat", 
                                             form_cellchat_idents = input$form_cellchat_idents, 
                                             form_cellchat_refdb = input$form_cellchat_refdb, 
                                             form_cellchat_ncore = input$form_cellchat_ncore),
                      "form_scsignalr" = list(form_type = "SingleCellSignalR", 
                                              form_scsignalr_idents = input$form_scsignalr_idents, 
                                              form_scsignalr_refdb = input$form_scsignalr_refdb,
                                              form_scsignalr_workdir = input$form_scsignalr_workdir ) )
  
  if(is.null(rv$data.combined) || length(rv$data.combined) == 0){
    output$ccc_form_data <- renderPrint({ "No combined datasets was detected." })
  } else {
    output$ccc_form_data <- renderPrint(form_data)
  }
  
  if(form_data$form_type == "CellChat"){
      
    ## form_cellchat_idents, choice of idents of Seurat object
      if(form_data$form_cellchat_idents == "Cell clusters") {
        Idents(rv$data.combined ) = 'seurat_clusters'
      }else if(form_data$form_cellchat_idents == "Cell type identity"){
        Idents(rv$data.combined)  = 'identity_singler'
      }
      
    ## form_cellchat_refdb, choice of database of ligand-receptor interaction
      if(form_data$form_cellchat_refdb == "CellChatDB.human"){ 
            lrdb = CellChatDB.human
        }else if(form_data$form_cellchat_refdb == "CellChatDB.mouse"){
            lrdb = CellChatDB.mouse
        }
      
    ## parallel computation setting
      future::plan("multicore", workers = form_data$form_cellchat_ncore )
      options(future.globals.maxSize = 100 * 1024^3)
      options(future.rng.onMisuse="ignore")
      
    ## CellChat pipeline  
      data.input <- GetAssayData(rv$data.combined, assay = "RNA", slot = "data") # normalized data matrix
      labels <- Idents(rv$data.combined)
      meta <- data.frame(labels = (labels), row.names = names(labels))
      meta$labels <- paste0('C_', meta$labels)
      
      cellchat.obj <- createCellChat(object = data.input, meta = meta, group.by = 'labels')
      
      cellchat.obj@DB <- lrdb ## set DB
      
      cellchat.obj <- subsetData(cellchat.obj) # essential
      
      cellchat.obj <- identifyOverExpressedGenes(cellchat.obj)
      cellchat.obj <- identifyOverExpressedInteractions(cellchat.obj)
      cellchat.obj <- computeCommunProb(cellchat.obj)  # Inference of cell-cell communication network
      cellchat.obj <- filterCommunication(cellchat.obj, min.cells = 20) # Filter out the cell-cell communication if there are only few number of cells in certain cell groups
      cellchat.obj <- computeCommunProbPathway(cellchat.obj) # Infer the cell-cell communication at a signaling pathway level
      cellchat.obj <- aggregateNet(cellchat.obj)
      
      ## save cellchat resulting objects
      saveRDS(cellchat.obj, file = file.path(rv$output_dir,  '5_CCC/CellChat_obj.rds'))
      
      ## save LR-pairs probability table
      df.net <- subsetCommunication(cellchat.obj, thresh=1)
      write.table(df.net, file.path(rv$output_dir, "5_CCC/CellChat_results_LRPairs.tsv" ), quote=F, sep='\t', row.names=F, col.names=T)
      output$ccc_table_lrPair <- renderDataTable({
        datatable(df.net)
      })
      
      ## save signaling pathway probability table
      df.netp <- subsetCommunication(cellchat.obj, slot.name = "netP", thresh=1)
      write.table(df.netp, file.path(rv$output_dir, "5_CCC/CellChat_results_signalPathway.tsv"), quote=F, sep='\t', row.names=F, col.names=T)
      output$ccc_table_signalPathway <- renderDataTable({
        datatable(df.netp)
      })
      
      ## render cell-cell interaction network between cell clusters by counts
      output$ccc_vis_netCount <- renderPlot({
         netVisual_circle(cellchat.obj@net$count, vertex.weight = as.numeric(table(cellchat.obj@idents)), weight.scale = T, label.edge= F, title.name = "Number of interactions")
      })
      
      ## render cell-cell interaction network between cell clusters by weights
      output$ccc_vis_netWeight <- renderPlot({
        netVisual_circle(cellchat.obj@net$weight, vertex.weight = as.numeric(table(cellchat.obj@idents)), weight.scale = T, label.edge= F, title.name = "Interaction weights/strength")
      })
      
    
  }else if(form_data$form_type == "SingleCellSignalR"){
    output$ccc_vis_clustering <- renderPlot({
      "This method has not been defined."
    })
  }
  
})

##* ******************************
##* tab_ccc module, tabpanel of 'Viewer'
##* Viewer functions are done in the above function.
##* 


##* ******************************
##* tab_ccc module, tabpanel of 'Report'
##* 


##* ******************************
##* tab_trajectory module, tabpanel of 'Input'
##* 

output$trajectory_dynamic_form <- renderUI({
  # 根据用户选择动态生成内容
  switch(input$trajectory_form_choice,
         "form_monocle" = {
           tagList(
             # textInput("input_a1", "输入字段 A1："),
             selectInput("form_monocle_cellcluster",
                         "Select cell cluster used to build trajectory:",
                         choices = form_monocle_cellcluster_dynamic,
                         multiple = TRUE
             ), # selectInput end
             selectInput("form_monocle_reduction_method",
                         "Select reduction method in monocle clustering:",
                         choices = c("UMAP", "tSNE", "PCA", "LSI", "Aligned"),
                         selected = c("UMAP")
             ), #selectInput end
             textInput("form_monocle_key", "prefix of output") # textInput end
           )
         }, # form_monocle end.
         
         "form_slingshot" = {
           tagList(
             # dateInput("input_c1", "日期字段 C1："),
             selectInput("form_scsignalr_idents",
                         "Set the idents:",
                         choices = c("Cell clusters", "Cell type identity", 'Customized' )
             ),
             selectInput("form_scsignalr_refdb",
                         "Set the ligand-receptor interaction database:",
                         choices = c("LRdb", "Customized")
             ),
             h4("Set a working directory for SingleCellSignalR:"),
             shinyDirButton(id = "form_scsignalr_workdir",
                            label = "SingleCellSignalR Working Dir",
                            title = "Set SingleCellSignalR Working Dir"
             )

           )
         } # form_slingshot end
  ) # end of switch function
})

observeEvent(input$trajectory_form_choice, {
    if(input$trajectory_form_choice =="form_monocle"){
      form_monocle_cellcluster_dynamic <<- unique(rv@meta.data$identity_singler)
      
      # update selectInput choices
      updateSelectInput(session, "form_monocle_cellcluster", 
                        choices = form_monocle_cellcluster_dynamic)
    } # end of if function
})

##* actionButton submit events
##* 

observeEvent(input$trajectory_input_submit,{
    form_data <- switch(input$trajectory_form_choice,
                        "form_monocle" = list(form_type = "Monocle3", 
                                              form_monocle_cellcluster = input$form_monocle_cellcluster, 
                                              form_monocle_reduction_method = input$form_monocle_reduction_method, 
                                              form_monocle_key = input$form_monocle_key),
                        "form_slingshot" = list(form_type = "Slingshot", 
                                                form_scsignalr_idents = input$form_scsignalr_idents, 
                                                form_scsignalr_refdb = input$form_scsignalr_refdb,
                                                form_scsignalr_workdir = input$form_scsignalr_workdir ) )
    
    if(is.null(rv$data.combined) || length(rv$data.combined) == 0){
      output$ccc_form_data <- renderPrint({ "No combined datasets was detected." })
    } else {
      output$ccc_form_data <- renderPrint(form_data)
    }
    
    if(form_data$form_type == "CellChat"){
      
      ## form_cellchat_idents, choice of idents of Seurat object
      if(form_data$form_cellchat_idents == "Cell clusters") {
        Idents(rv$data.combined ) = 'seurat_clusters'
      }else if(form_data$form_cellchat_idents == "Cell type identity"){
        Idents(rv$data.combined)  = 'identity_singler'
      }
      
      ## form_cellchat_refdb, choice of database of ligand-receptor interaction
      if(form_data$form_cellchat_refdb == "CellChatDB.human"){ 
        lrdb = CellChatDB.human
      }else if(form_data$form_cellchat_refdb == "CellChatDB.mouse"){
        lrdb = CellChatDB.mouse
      }
      
      ## parallel computation setting
      future::plan("multicore", workers = form_data$form_cellchat_ncore )
      options(future.globals.maxSize = 100 * 1024^3)
      options(future.rng.onMisuse="ignore")
      
      ## CellChat pipeline  
      data.input <- GetAssayData(rv$data.combined, assay = "RNA", slot = "data") # normalized data matrix
      labels <- Idents(rv$data.combined)
      meta <- data.frame(labels = (labels), row.names = names(labels))
      meta$labels <- paste0('C_', meta$labels)
      
      cellchat.obj <- createCellChat(object = data.input, meta = meta, group.by = 'labels')
      
      cellchat.obj@DB <- lrdb ## set DB
      
      cellchat.obj <- subsetData(cellchat.obj) # essential
      
      cellchat.obj <- identifyOverExpressedGenes(cellchat.obj)
      cellchat.obj <- identifyOverExpressedInteractions(cellchat.obj)
      cellchat.obj <- computeCommunProb(cellchat.obj)  # Inference of cell-cell communication network
      cellchat.obj <- filterCommunication(cellchat.obj, min.cells = 20) # Filter out the cell-cell communication if there are only few number of cells in certain cell groups
      cellchat.obj <- computeCommunProbPathway(cellchat.obj) # Infer the cell-cell communication at a signaling pathway level
      cellchat.obj <- aggregateNet(cellchat.obj)
      
      ## save cellchat resulting objects
      saveRDS(cellchat.obj, file = file.path(rv$output_dir,  '5_CCC/CellChat_obj.rds'))
      
      ## save LR-pairs probability table
      df.net <- subsetCommunication(cellchat.obj, thresh=1)
      write.table(df.net, file.path(rv$output_dir, "5_CCC/CellChat_results_LRPairs.tsv" ), quote=F, sep='\t', row.names=F, col.names=T)
      output$ccc_table_lrPair <- renderDataTable({
        datatable(df.net)
      })
      
      ## save signaling pathway probability table
      df.netp <- subsetCommunication(cellchat.obj, slot.name = "netP", thresh=1)
      write.table(df.netp, file.path(rv$output_dir, "5_CCC/CellChat_results_signalPathway.tsv"), quote=F, sep='\t', row.names=F, col.names=T)
      output$ccc_table_signalPathway <- renderDataTable({
        datatable(df.netp)
      })
      
      ## render cell-cell interaction network between cell clusters by counts
      output$ccc_vis_netCount <- renderPlot({
        netVisual_circle(cellchat.obj@net$count, vertex.weight = as.numeric(table(cellchat.obj@idents)), weight.scale = T, label.edge= F, title.name = "Number of interactions")
      })
      
      ## render cell-cell interaction network between cell clusters by weights
      output$ccc_vis_netWeight <- renderPlot({
        netVisual_circle(cellchat.obj@net$weight, vertex.weight = as.numeric(table(cellchat.obj@idents)), weight.scale = T, label.edge= F, title.name = "Interaction weights/strength")
      })
      
      
    }else if(form_data$form_type == "SingleCellSignalR"){
      output$ccc_vis_clustering <- renderPlot({
        "This method has not been defined."
      })
    }
  
}) # end of observeEvent


} ## server end


## ------------------------
## Launch
##------------------------

shinyApp(ui, server, options = list(
  launch.browser = .rs.invokeShinyWindowExternal,
  launch.browser.args = c("--window-size=1200,800")
    )
)

