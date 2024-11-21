library(shiny)

# UI 部分
ui <- fluidPage(
  titlePanel("单选控件动态表单示例"),
  
  sidebarLayout(
    sidebarPanel(
      # 单选控件
      radioButtons("form_choice", "选择表单类型：",
                   choices = c("表单 A" = "form_a",
                               "表单 B" = "form_b",
                               "表单 C" = "form_c"),
                   selected = "form_a"),
      
      # 动态表单区域
      uiOutput("dynamic_form")
    ),
    
    mainPanel(
      # 显示结果
      verbatimTextOutput("form_data")
    )
  )
)

# Server 部分
server <- function(input, output, session) {
  # 动态生成表单内容
  output$dynamic_form <- renderUI({
    # 根据用户选择动态生成内容
    switch(input$form_choice,
           "form_a" = {
             tagList(
               textInput("input_a1", "输入字段 A1："),
               numericInput("input_a2", "数值字段 A2：", value = 0)
             )
           },
           "form_b" = {
             tagList(
               selectInput("input_b1", "选择字段 B1：", 
                           choices = c("选项 1", "选项 2", "选项 3")),
               checkboxInput("input_b2", "勾选字段 B2", value = FALSE)
             )
           },
           "form_c" = {
             tagList(
               dateInput("input_c1", "日期字段 C1："),
               sliderInput("input_c2", "滑动字段 C2：", min = 0, max = 100, value = 50)
             )
           }
    )
  })
  
  # 收集动态表单数据
  output$form_data <- renderPrint({
    selected_form <- input$form_choice
    data <- switch(selected_form,
                   "form_a" = list(A1 = input$input_a1, A2 = input$input_a2),
                   "form_b" = list(B1 = input$input_b1, B2 = input$input_b2),
                   "form_c" = list(C1 = input$input_c1, C2 = input$input_c2))
    data
  })
}

# 运行 Shiny 应用
shinyApp(ui, server)