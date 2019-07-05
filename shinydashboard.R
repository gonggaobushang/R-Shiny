# dropdownMenuy 右上角的小下拉菜单
#shiny支持2个库的icon，Font 
# https://fontawesome.com/
# https://www.glyphicons.com/
library(shiny)
library(shinydashboard)
ui<-dashboardPage( 
  dashboardHeader(
    title = "标题",
    dropdownMenu(type = "notifications",
                 notificationItem(text = "1",icon("users"),status="primary" #蓝色
                                  ),
                 notificationItem(text = "2",icon("truck"),status = "success" #绿色
                                  ),
                 notificationItem(text = "3",icon = icon("exclamation-triangle"),
                                  status = "info" #蓝色
                                  ),
                 notificationItem(text = "4",icon = icon("wifi"),
                                  status = "danger" #红色
                 )
                 ),
    dropdownMenu(type = "messages",badgeStatus = "danger",
                 messageItem(from = "老板", 
                             message = "来我办公室一趟", 
                             icon = icon(name = "handshake")
                             ),
                 messageItem(
                   from = "老大",
                   message = "几点到",
                   icon = icon("plane", lib = "glyphicon"),#默认Font Awesome库
                   time = "13:45"
                   ),
                 messageItem(
                   from = "上司",
                   message = "几号？",
                   icon = icon("female", lib = "font-awesome"),
                   time = "2014-12-01" 
                   )
    ) ,
    dropdownMenu(type = "tasks", badgeStatus = "warning", #同上面的status颜色
                 taskItem(value = 90, color = "green",text = "1"),
                 taskItem(value = 17, color = "aqua",text = "2"),
                 taskItem(value = 75, color = "yellow",text = "3"),
                 taskItem(value = 80, color = "red",text = "4")
    )
  ),
  dashboardSidebar(),   
  dashboardBody() 
)
Server<-function(input, output){}
shinyApp(ui,Server)
#还有另外一种方便维护的方式
ui<-dashboardPage(
  dashboardHeader(title="",dropdownMenuOutput("messageMenu") 
                  ),
  dashboardSidebar(),
  dashboardBody()
)
messageData <- data.frame(
  from = c("老板", "老大", "上司"),
  message = c(
    "来我办公室一趟",
    "几点到？？",
    "几号？"
  ),
  iconname = c("handshake", "plane", "female"),
  iconlib = c("font-awesome", "glyphicon", "font-awesome"),
  time = c(NA, "13:45", "2014-12-01"),
  stringsAsFactors = FALSE
)
serve<-function(input, output) {
  output$messageMenu <- renderMenu({ 
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], #相当于 .$from
                  message = row[["message"]],
                  icon = icon(name = row[["iconname"]], lib = row[["iconlib"]]),
                  time = row[["time"]])
    })
    dropdownMenu(type = "messages",badgeStatus = "success", .list = msgs) #其余以列表形式呈现
  })
}
shinyApp(ui,serve)



#sliderInput
ui<-dashboardPage(
  dashboardHeader(title = "滑动条传入参数"),
  dashboardSidebar(
    sliderInput("integer", "整数：", 
                min=0, max=1000, value=500), 
    sliderInput("decimal", "小数：", 
                min = 0, max = 1, value = 0.5, step= 0.1), 
    sliderInput("range", "范围：",
                min = 1, max = 1000, value = c(200,500)), 
    sliderInput("format", "货币格式:", 
                min = 0, max = 10000, value = 0, step = 2500, 
                format="$#,##0", locale="us",  #格式为千分位数字，locale美元
                animate=TRUE), #设定动画
    sliderInput("animation", "循环动画", 
                1, 2000, 1, step = 10, 
                animate=animationOptions(interval=300, loop=T)) #设定动画选项
  ),
  dashboardBody( 
    fluidRow(
      box(
        tableOutput("values"))) #以HTML表格形式输出变量values
  )
)
server<-function(input, output) {
  # 反应表达式：创建一个数据框，用来存放所有输入值。  
  sliderValues <- reactive({
    data.frame(
      Name = c("整数", "小数","范围", "货币格式","动画"),
      Value = as.character(c(input$integer, input$decimal,
                             paste(input$range, collapse=' '),
                             input$format, input$animation)), 
      )
  }) 
  # 输出组件，新增变量values
  output$values <- renderTable({ # 以表格的形式输出
    sliderValues() # 调用反应表达式需要加括号()
  })
  
  sliderValues <- reactive({
    data.frame(
      Name = c("整数", "小数","范围", "货币格式","动画"),
      Value = as.character(c(input$integer, input$decimal,
                             paste(input$range, collapse=' '),input$format, input$animation)), 
      stringsAsFactors=FALSE)
  }) 
  output$values <- renderTable({ 
    sliderValues() #调用反应表达式需要加括号()
  })
}
shinyApp(ui,server)



#selectInput
ui<-dashboardPage(
  dashboardHeader(title = "写入参数"),
  dashboardSidebar(
    selectInput("variable",  
                "选择变量：", 
                list("气缸数" = "cyl",  
                     "变速箱类型" = "am", 
                     "档位数" = "gear")),
    checkboxInput("outliers",  
                  "显示离群值", 
                  FALSE) , #默认输出FALSE
    textInput("text",
              "自定义标题：") 
  ),
  dashboardBody(
    fluidRow(
      box(
        plotOutput("mpgPlot"), # 以图片形式输出mpgPlot变量
        title = h3(textOutput("caption"))),
      box(
        title = h3(textOutput("text")))) 
  )
)
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))
server<-function(input, output) {
  formulaText <- reactive({
    paste("mpg ~", input$variable)
  })
  output$text <- renderText({
    input$text
  })
  output$caption <- renderText({
    formulaText()
  })
  output$mpgPlot <- renderPlot({
    boxplot(as.formula(formulaText()), #变成公式
            data = mpgData,
            outline = input$outliers) #判断：TRUE或者FALSE
  })
}
shinyApp(ui,server)



#fileInput
library(DT)
ui<-dashboardPage(
  dashboardHeader(title = "上传文件"),
  dashboardSidebar(
    fileInput('file1', '选择CSV文件', multiple = FALSE, 
              accept=c('text/csv', 'text/comma-separated-values,text/plain')), 
    tags$hr(), # 水平线条
    checkboxInput('header', '第1行为变量名', TRUE),
    radioButtons('sep', '选择分隔符：',
                 c("逗号"=',', "分号"=';', "制表符"='\t'), 
                 selected = ','),
    radioButtons('quote', '指定引号：',
                 c("空格"='', "双引号"='"', "单引号"="'"), # 选择范围
                 selected = ';') #没有匹配的就不选择
  ),
  dashboardBody(
    h2("表格内容："), 
    fluidRow(width = 8,
             box(DT::DTOutput("contents"))) # 以DT控件输出
  )
)
server<-function(input, output) {
  output$contents <- renderDT({ 
    inFile <- input$file1 # file属性组成的数据框，包括name, size , type, datapath
    if (is.null(inFile)) # 初始值应该为NULL
      return(NULL)  # 空则返回NULL
    # 非空则作为csv文件进行读取
    read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  })
}
shinyApp(ui,server)




#downloadButton
ui<-dashboardPage(
  dashboardHeader(title = "下载数据"),
  dashboardSidebar(
    selectInput("dataset", "选择要下载的数据集:", 
                choices = c("rock", "pressure", "cars")),
    downloadButton('downloadData', '下载')
  ),
  dashboardBody(
    h2("表格内容："), 
    fluidRow(width = 8,
             box(DTOutput("table"))) 
  )
)
server<-function(input, output) {
  datasetInput <- reactive({
    switch(input$dataset, #判断，相当于if
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  output$table <- renderDT({
    datasetInput()
  })
  output$downloadData <- downloadHandler( 
    filename = function() {paste(input$dataset, '.csv', sep='') },
    content = function(file) {write.csv(datasetInput(), file)}
  )
}
shinyApp(ui,server)





#submitButton
ui<-dashboardPage(
  dashboardHeader(title = "补充上面"),
  dashboardSidebar(
    selectInput("dataset", "选择一个数据集:", 
                choices = c("rock", "pressure", "cars")),
    numericInput("obs", "输入观测值数量:", 10),
    helpText("注：表格内只显示指定观测值数量的数据，而概况中包括所有数据"), 
    #换行符无效，若需要多段文本则增加多个文本部件
    submitButton("提交")
  ),
  dashboardBody(
    h2("表格内容："), 
    fluidRow(
      h4("概况"), 
      box(width = 11, 
          verbatimTextOutput("summary"))),# 以文本形式打印summary变量
    fluidRow(
      h4("观测值"),
      box(
        tableOutput("view"))) # 以表格形式输出view变量 
  )
)
server<-function(input, output) {
  datasetInput <- reactive({
    switch(input$dataset, 
           "rock" = rock, 
           "pressure" = pressure,
           "cars" = cars)
  })
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
}
shinyApp(ui,server)
