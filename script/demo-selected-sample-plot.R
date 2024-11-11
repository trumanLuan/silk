library(shiny)
library(shinydashboard)
library(DT)  # 用于展示数据表
library(ggplot2)  # 用于绘制图形

# UI部分
ui <- dashboardPage(
  dashboardHeader(title = "数据传递示例"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("数据加载", tabName = "load_data", icon = icon("file-upload")),
      menuItem("数据分析", tabName = "analyze_data", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "load_data",
        h2("加载数据"),
        fileInput("file1", "选择CSV文件", multiple = TRUE, accept = ".csv"),
        DTOutput("data_table")
      ),
      tabItem(
        tabName = "analyze_data",
        h2("数据分析"),
        verbatimTextOutput("data_summary"),
        uiOutput("sample_selector"),
        plotOutput("selected_plot"),
        downloadButton("download_summary", "下载汇总数据")
      )
    )
  )
)

# Server部分
server <- function(input, output, session) {
  # 存储数据的reactiveValues对象
  rv <- reactiveValues(data_list = list(), summary_data = NULL)
  
  # 读取数据并存储到rv$data_list
  observeEvent(input$file1, {
    req(input$file1)
    rv$data_list <- lapply(input$file1$datapath, read.csv)
    names(rv$data_list) <- input$file1$name  # 给数据列表中的每个元素命名
  })
  
  # 显示加载的数据
  output$data_table <- renderDT({
    req(rv$data_list)
    datatable(rv$data_list[[1]])  # 显示第一个数据文件的内容
  })
  
  # 数据分析摘要
  output$data_summary <- renderPrint({
    req(rv$data_list)
    summary_list <- lapply(rv$data_list, summary)
    summary_list  # 输出所有数据文件的摘要信息
  })
  
  # 创建样本选择器
  output$sample_selector <- renderUI({
    req(rv$data_list)
    selectInput("selected_sample", "选择样本", choices = names(rv$data_list))
  })
  
  # 绘制选定样本的分布图
  output$selected_plot <- renderPlot({
    req(input$selected_sample)
    data <- rv$data_list[[input$selected_sample]]
    ggplot(data, aes(x = data[[1]])) +  # 使用第一列绘制分布图
      geom_histogram(binwidth = 1) +
      theme_minimal() +
      labs(title = paste("分布图 -", input$selected_sample), x = "Value", y = "Count")
  })
  
  # 下载汇总数据
  output$download_summary <- downloadHandler(
    filename = function() {
      paste("summary_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      summary_data <- do.call(rbind, lapply(names(rv$data_list), function(name) {
        data <- rv$data_list[[name]]
        data.frame(
          File = name,
          Rows = nrow(data),
          Columns = ncol(data)
        )
      }))
      write.csv(summary_data, file, row.names = FALSE)
    }
  )
}

# 启动应用
shinyApp(ui, server)

