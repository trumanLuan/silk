library(shiny)

# UI 部分
ui <- fluidPage(
  titlePanel("文件选择框示例"),
  
  sidebarLayout(
    sidebarPanel(
      # 文件选择框
      fileInput("file", 
                label = "选择一个文件", 
                multiple = FALSE, # 是否允许多文件上传
                accept = c(".csv", ".txt") # 限制文件类型
      ),
      helpText("支持 .csv 和 .txt 文件。"),
      helpText("支持 .csv 和 .txt 文件2。")
    ),
    
    mainPanel(
      # 显示上传文件的内容
      tableOutput("fileContents")
    )
  )
)

# Server 部分
server <- function(input, output) {
  # 监控文件输入并读取内容
  output$fileContents <- renderTable({
    req(input$file)  # 确保文件已上传
    file <- input$file$datapath
    read.csv(file)   # 假设文件为 CSV 格式
  })
}

# 启动应用
shinyApp(ui = ui, server = server)
