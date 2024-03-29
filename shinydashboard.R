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
#icon也可以加到sidebar侧边栏中
ui<-dashboardPage(
  dashboardHeader(title = "动态侧边栏"),
  dashboardSidebar(dropdownMenuOutput("myMenu")), # 以下拉菜单形式输出myMenu变量),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard", 
              h2("1")), 
      tabItem(tabName = "widgets", 
              h2("2"))
    )
  )
)
menudata <- data.frame(
  text = c("图表页", "小部件页"),
  tabnames = c("dashboard", "widgets"),
  iconname = c("dashboard", "th"),
  stringsAsFactors = FALSE
)
server<-function(input, output) {
  output$myMenu <- renderMenu({ #以menu形式输出
    mymenu_list <- apply(menudata, 1, function(row){
      menuItem(text = row[["text"]], 
               tabName = row[["tabnames"]], 
               icon = icon(row[["iconname"]]))
    })
    sidebarMenu(.list = mymenu_list) 
  })
}
shinyApp(ui,server)



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




# box,renderPlot
ui<-dashboardPage(
  dashboardHeader(title = "对象框"),
  dashboardSidebar(disable = FALSE), #不需要侧边栏
  dashboardBody(
    fluidRow(
      box(plotOutput("gplot_1"), title = textOutput("text_1"), 
          width = 8, status = "primary", solidHeader = TRUE, 
          collapsible = TRUE, background = "fuchsia"), # 最小化按钮，洋红色背景
      box(width = 4, background = "lime", # 黄绿色背景
          "随便打的文本", # 直接插入文本
          br(), 
          sliderInput("slider", "请输入观测值数量：", 50, 500, 200), 
          textInput("text_1", "请输入标题：", value = "我是标题"),
          textInput("text_2", "输入横轴名称：", value = "我是x轴"), 
          textInput("text_3", "输入纵轴名称：", value = "我是y轴"), 
          submitButton("提交")) #ggplot2运算复杂，需增加提交按钮
    )
  )
)
library(ggplot2)
library(RColorBrewer)
#library(showtext)
server<-function(input, output) {
  datainput <- reactive({
    data.frame(abc = sample(LETTERS[1:7], size = input$slider, replace = TRUE), 
               stringsAsFactors = F)
  })
  output$gplot_1 <- renderPlot({ 
    #showtext_auto()
    ggplot(data = datainput()) + # 注意datainput()括号不能少
      geom_bar(aes(abc, fill = abc)) +
      scale_fill_brewer(palette = "Set2") + 
      labs(title = input$text_1, x = input$text_2, y = input$text_3) + 
      theme_void() + #ggplot2的内置主题之一
      theme(
        plot.title = element_text(colour = "magenta", hjust = 0.5, size = 30),
        axis.title.x = element_text(colour = "blue", hjust = 0.5, size = 20),
        axis.title.y = element_text(colour = "blue", hjust = 0.5, angle = 90, size = 20),
        axis.text = element_text(colour = "black", size = 10)
      )
  })
  
}
shinyApp(ui,server)




# tabBox,tagList
ui<-dashboardPage(
  dashboardHeader(title = "tabBox"),
  dashboardSidebar(disable = FALSE), 
  dashboardBody(
    fluidRow(
      tabBox(
        title = "绘图区域",id = "tabbox1", selected = "Tab1", # 默认显示Tab1
        width = 8, side = "right", #side表示tablePanel的顺序，right表示反向
        tabPanel(title = "图1", value = "Tab1", #value与tabBox内的selected匹配
                 "第1个图的内容", br(), plotOutput("plot1")), # 内容
        tabPanel(title = "图2", value = "Tab2",
                 "第2个图的内容", br(), plotOutput("plot2"))
      ),
      tabBox(
        title = "表格区域", id = "tabbox2", selected = "Tab3", 
        width = 4, side = "left",  # 默认显示左起第3个图表
        tabPanel(title = "表1", value = "Tab1", 
                 "第1个表的内容", br(), tableOutput("table1")), 
        tabPanel(title = "表2", value = "Tab2", 
                 "第2个表的内容", br(), tableOutput("table2")),
        tabPanel(title = "表3", value = "Tab3", 
                 "第3个表的内容", br(), tableOutput("table3"))
      )
    ),
    fluidRow(
      tabBox(
        title = tagList(shiny::icon("gear"), "状态区域"),#标题也可以包含icon
        id = "tabbox3", selected = "Tab1",
        tabPanel(title = "状态1", value = "Tab1", 
                 "随便码一行文字:",br(), 
                 verbatimTextOutput("summary")), # 文本形式输出变量tabset1Selected
        tabPanel(title = "状态2", value = "Tab2", 
                 "状态2的内容", br(), verbatimTextOutput("str"))
      ))
  )
)

set.seed(123)
mydata <- data.frame(abc = sample(letters[1:7], size = 100, replace = TRUE),
                     ABC = sample(LETTERS[1:7], size = 100, replace = TRUE),
                     numb1 = rnorm(100),
                     numb2 = 1:100)
server<-function(input, output) {
  output$plot1 <- renderPlot({
    ggplot(mydata) + 
      geom_bar(aes(abc, fill = abc)) + 
      scale_fill_brewer(palette = "Set2") + 
      theme_classic()
  })
  output$plot2 <- renderPlot({
    ggplot(mydata) + 
      geom_point(aes(x = numb2, y = numb1), color = "magenta") + 
      theme_bw()
  })
  output$table1 <- renderTable({
    head(mydata, 6L)
  })
  output$table2 <- renderTable({
    head(mydata[7:12,])
  })
  output$table3 <- renderTable({
    head(mydata[13:18,])
  })
  output$summary <- renderPrint({
    summary(mydata)
  })
  output$str <- renderPrint({
    str(mydata)
  })
}
shinyApp(ui,server)





# infoBox,valueBox,actionButton
ui<- dashboardPage(
  dashboardHeader(title = "infoBox"),
  dashboardSidebar(disable = FALSE), 
  dashboardBody(
    fluidRow(
      valueBox(value = 10 * 2, subtitle = "新增用户", icon = icon("credit-card")),
      #valueBox与infoBox相似只是外表不同
      infoBox(title = "定单", value = 10 * 2, icon = icon("credit-card")),
      infoBoxOutput("progressBox"),
      infoBoxOutput("approvalBox")
    ),
    fluidRow(
      infoBox(title = "定单", value = 10 * 2, 
              icon = icon("credit-card"), fill = TRUE),
      infoBoxOutput("progressBox2"),
      infoBoxOutput("approvalBox2")
    ),
    fluidRow(
      box(width = 4, actionButton("addtion", label = "增加赞", icon = icon("plus"))),
      box(width = 4, actionButton("minus", label = "减少赞", icon = icon("minus")))
    )
  )
)
server<-function(input, output) {
  count_thumbs <- reactive({
    comprehensive <- input$addtion - input$minus
    if(comprehensive > 0) {
      positive <- comprehensive 
      negative <- 0
    } else {
      positive <- 0
      negative <- comprehensive
    }
    thumbs_bind <- c(positive, negative)
  })
  output$progressBox <- renderInfoBox({
    infoBox(
      title = "变化", value = paste0(25, "%"), 
      icon = icon("list"), color = "purple")
  })
  output$approvalBox <- renderInfoBox({
    infoBox(
      title = "赞同", value = 25 + count_thumbs()[1], 
      icon = icon("thumbs-up"), color = "yellow")
  })
  output$progressBox2 <- renderInfoBox({
    infoBox(
      title = "变化", value = paste0(25, "%"), 
      icon = icon("list"),color = "purple", fill = TRUE)
  })
  output$approvalBox2 <- renderInfoBox({
    infoBox(
      title = "不赞同", value = 25 - count_thumbs()[2], 
      icon = icon("thumbs-down"), color = "yellow", fill = TRUE)
  })
}
shinyApp(ui,server)



# fluidRow,body
body <- dashboardBody(
  fluidRow(
    box(title = "第1行第1个", "随便码几个字"),
    box(title = "第1行第2个", status = "warning", "warning状态")),
  fluidRow(
    box(         #一行共12
      title = "第2行第1个", width = 3, solidHeader = TRUE, #自动高
      status = "primary", "primary状态", br(), "宽3"),
    box(
      title = "第2行第2个", width = 4, solidHeader = TRUE,
      "随便码几个字", br(), "宽4"),
    box(
      title = "第2行第3个", width = 5, solidHeader = TRUE, 
      status = "warning", "warning状态", br(), "宽5")),
  fluidRow(
    box(
      title = "第3行第1个", width = 5, height = 400, 
      background = "black","黑色背景", br(), "宽5"),
    box(
      title = "第3行第2个", width = 4, height = 400, 
      background = "light-blue","浅蓝色背景", br(), "宽4"),
    box(
      title = "第3行第3个", width = 3, height = 400, 
      background = "maroon", "栗色背景", br(), "宽3"))
)
ui<-dashboardPage(
  dashboardHeader(title = "多行布局"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(body)
)
server<-function(input, output) {}
shinyApp(ui,server)



# column
body <- dashboardBody(
  fluidRow(
    column(width = 3,
           box(
             title = "第1列第1个", width = NULL, height = 200, 
             #必须width = NULL, 因为默认为6但是column已经定义了3
             status = "primary","primary状态", br(), "高200"),
           box(
             title = "第1列第2个", width = NULL, height = 100, solidHeader = TRUE, 
             status = "primary", "primary状态", br(), "高100"),
           box(
             title = "第1列第3个",width = NULL, height = 300, 
             background = "black", "背景颜色：black", br(), "高300")
    ),
    column(width = 4, offset = 2, # offset = 2，跟前一列之间增加间隙2个宽度
           box(
             title = "第2列第1个", status = "warning", width = NULL, height = 100, 
             "warning状态", br(), "高100"),
           box(
             title = "第2列第2个", width = NULL, height = 200, solidHeader = TRUE, 
             status = "warning","warning状态", br(), "高200"),
           box(
             title = "第2列第3个", width = NULL, height = 300, 
             background = "light-blue", "背景颜色：light-blue", br(), "高300")
    ),
    column(width = 3,
           box(
             title = "第3列第1个", width = NULL, height = 300, solidHeader = TRUE,
             "随便码几个字", br(), "高300"),
           box(
             title = "第3列第2个", width = NULL, height = 200, 
             background = "maroon", "背景颜色：maroon", br(), "高200")
    )
  )
)
ui<-dashboardPage(
  dashboardHeader(title = "多列布局"),
  dashboardSidebar(disable = TRUE), 
  dashboardBody(body)
)
server<-function(input, output) {}
shinyApp(ui,server)
#行列布局
body <- dashboardBody(
  fluidRow(
    box(
      title = "第1行第1个", width = 6, status = "primary",
      "primary状态", br(), "宽6"),
    box(
      title = "第1行第2个", width = 6, status = "warning", 
      "warning状态", br(), "宽6")),
  fluidRow(
    column(width = 3, 
           box(
             title = "第2行第1列第1个", width = NULL, height = 100, 
             solidHeader = TRUE, status = "primary",
             "primary状态", br(), "高100"),
           box(
             title = "第2行第1列第2个", width = NULL, heigth = 150, 
             background = "black",
             "背景颜色：black", br(), "高150"),
           box(
             title = "第2行第1列第3个", width = NULL, heigth = 100, 
             background = "fuchsia",
             "背景颜色：fuchsia", br(), "高100")),
    column(width = 4, 
           box(
             title = "第2行第2列第1个", width = NULL, height = 150,
             solidHeader = TRUE, status = "warning",
             "warning状态", br(), "高150"),
           box(
             title = "第2行第2列第2个", width = NULL, height = 200, 
             background = "light-blue",
             "背景颜色：light-blue", br(), "高200")),
    column(width = 5,
           box(
             title = "第2行第3列第1个", width = NULL, height = 300, 
             solidHeader = TRUE,
             "高度300"),
           box(
             title = "第2行第3列第2个", width = NULL, height = 200,
             background = "maroon",
             "背景颜色：maroon", br(), "高200")),
    fluidRow(
      box(
        title = "第3行第1个", width = 4, status = "primary",
        "primary状态", br(), "宽4"),
      box(
        title = "第3行第2个", width = 8, status = "warning", 
        "warning状态", br(), "宽8"))
  )
)

ui<-dashboardPage(
  dashboardHeader(title = "混合布局"),
  dashboardSidebar(disable = TRUE), # 
  dashboardBody(body)
)
server<-function(input, output) {}
shinyApp(ui,server)




#menuItem
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "绘图",  
             tabName = "someplots", 
             icon = icon("images")),
    menuItem(text = "箱子", 
             tabName = "someboxes", 
             icon = icon("boxes"), 
             badgeLabel = "新!", badgeColor = "green"),  
    menuItem(text = "百度一下", icon = icon("search"), href = "https://www.baidu.com/")
  )
)

tab1 <- fluidRow(
  box(plotOutput("gplot_1"), width = 8),
  box(width = 4,
      "随便打的文本", 
      br(), 
      "随便码的文字", 
      sliderInput("slider", "请输入观测值数量：", 50, 500, 200),
      textInput("text_1", "请输入标题：", value = "我是标题"),
      textInput("text_2", "输入横轴名称：", value = "我是x轴"), 
      textInput("text_3", "输入纵轴名称：", value = "我是y轴"),
      submitButton("提交")) 
)
tab2_1 <-  fluidRow(
  infoBox(title = "定单", value = 10 * 2, icon = icon("credit-card")),
  infoBoxOutput("progressBox"),
  infoBoxOutput("approvalBox")
)
tab2_2 <- fluidRow( 
  infoBox(title = "定单", value = 10 * 2, 
          icon = icon("credit-card"), fill = TRUE),
  infoBoxOutput("progressBox2"),
  infoBoxOutput("approvalBox2")
)
tab2_3 <- fluidRow( 
  box(width = 4, actionButton("addtion", label = "增加赞", icon = icon("plus"))),
  box(width = 4, actionButton("minus", label = "减少赞", icon = icon("minus")))
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "someplots", 
            tab1), 
    tabItem(tabName = "someboxes", 
            h2("随便码几个字"), br(), tab2_1, tab2_2, tab2_3) 
  )
)

ui<-dashboardPage(
  dashboardHeader(title = "选项卡"),
  sidebar,
  body
)
library(ggplot2)
library(RColorBrewer)
server<-function(input, output) {
  datainput <- reactive({
    data.frame(abc = sample(LETTERS[1:7], size = input$slider, replace = TRUE), 
               stringsAsFactors = F)
  })
  count_thumbs <- reactive({
    comprehensive <- input$addtion - input$minus
    if(comprehensive > 0) {
      positive <- comprehensive 
      negative <- 0
    } else {
      positive <- 0
      negative <- comprehensive
    }
    thumbs_bind <- c(positive, negative)
  })
  output$gplot_1 <- renderPlot({ 
    ggplot(data = datainput()) + 
      geom_bar(aes(abc, fill = abc)) +
      scale_fill_brewer(palette = "Set2") + 
      labs(title = input$text_1, x = input$text_2, y = input$text_3) + 
      theme_void() + 
      theme(
        plot.title = element_text(colour = "magenta", hjust = 0.5, size = 30),
        axis.title.x = element_text(colour = "blue", hjust = 0.5, size = 20),
        axis.title.y = element_text(colour = "blue", hjust = 0.5, angle = 90, size = 20),
        axis.text = element_text(colour = "black", size = 10)
      )
  })
  output$progressBox <- renderInfoBox({
    infoBox(
      title = "变化", value = paste0(25, "%"), 
      icon = icon("list"), color = "purple")
  })
  output$approvalBox <- renderInfoBox({
    infoBox(
      title = "赞同", value = 25 + count_thumbs()[1], 
      icon = icon("thumbs-up"), color = "yellow")
  })
  output$progressBox2 <- renderInfoBox({
    infoBox(
      title = "变化", value = paste0(25, "%"), 
      icon = icon("list"),color = "purple", fill = TRUE)
  })
  output$approvalBox2 <- renderInfoBox({
    infoBox(
      title = "不赞同", value = 25 - count_thumbs()[2], 
      icon = icon("thumbs-down"), color = "yellow", fill = TRUE)
  })
}
shinyApp(ui,server)



# dashboardPage
ui<- dashboardPage(skin = "green",
                   dashboardHeader(title = "我是一个long-long-long-long-long-long-long标题", 
                                   titleWidth = 500),
                   dashboardSidebar(disable = FALSE), 
                   dashboardBody(
                     fluidRow(
                       infoBox(title = "定单", value = 10 * 2, icon = icon("credit-card")),
                       infoBoxOutput("progressBox"),
                       infoBoxOutput("approvalBox")
                     ),
                     fluidRow(
                       infoBox(title = "定单", value = 10 * 2, 
                               icon = icon("credit-card"), fill = TRUE),
                       infoBoxOutput("progressBox2"),
                       infoBoxOutput("approvalBox2")
                     ),
                     
                     fluidRow(
                       box(width = 4, actionButton("addtion", label = "增加赞", icon = icon("plus"))),
                       box(width = 4, actionButton("minus", label = "减少赞", icon = icon("minus")))
                     )
                   )
)
shinyApp(ui,server) 



# 文字
ui<-fluidPage(
  mainPanel(
    h1("一级标题, 水平居中", align = "center"), 
    h2("二级标题, 左对齐", align = "left"),
    h3("三级标题，右对齐", align = "right"),
    h4("四级标题，水平居中，因为我指定了参数align = center,center需要加双引号", 
       align = "center"),
    h5("五级标题"),
    h6("六级标题"),
    p("使用函数p插入一段文字"),
    strong("使用stong函数插入粗体文字"),
    em("使用em函数插入斜体文字"),
    hr(style = "color:red"),
    code("this is a code box, created by code function"),
    div(paste("div函数插入一段风格统一的文字，这段文字是蓝色的",
              "因为我在div内设定了参数`style = color:blue`", sep = ""),
        style = "color:blue"),
    hr(style = "color:black"),# 插入一条红色水平线
    br(),
    p("span函数用于p内，可以插入风格不一样的文字，通过指定style参数", 
      span("我在span函数内，我的风格不一样,我是红色的", style = "color:red"), 
      "我在p内，在span后面，我的颜色与span前面一样"), 
    br(),
    pre("我用  pre函数  执行,  没有  空格压缩,  我指定了  宽度参数  width=67", 
        width = 40),
    p("我用  p函数  执行,  存在  空格压缩, "),
    br(),
    a(href = "https://www.baidu.com/", # 前面需要加https://,否在为打开子网页
      target = "_blank", #target参数表示点击后，超链接的相应方式，_blank表示默认打开新标签页
      "我是超链接，a创建，百度一下"),
    br(), 
    HTML(paste("<p><font color = '#FF00FF' font size = '5' font face = 'Comic sans MS'>",
               "我是紫色的5号Comic sans MS字体，用HTML函数插入",
               "</font>",
               "<br />", # 插入换行符
               "<font color = 'red'>",
               "我是红色的，用HTML插入",
               "</font><p>"), sep = ""),
    br(),
    span("我是蓝色粗体字", style = "color:green;font-weight:bold") # 分号隔开
  )
)
server<-function(input, output) {
}
shinyApp(ui,server) 




# tags
ui<-fluidPage(
  mainPanel(
    tags$ul(
      tags$li("第1个项目"),
      tags$li("第2个项目"),
      tags$li("第3个项目")
    )
  )
)
server<-function(input, output) {
}
shinyApp(ui,server) 
ui<-fluidPage(
  mainPanel(
    tags$blockquote("没想到你个浓眉大眼的，也叛变了革命", cite = "鲁迅")
  )
)
shinyApp(ui,server)




# 图像,音乐,视频
ui<-fluidPage(
  mainPanel(
    img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1b/R_logo.svg/2000px-R_logo.svg.png", 
        height = "200px", width = "200px", align = "center"),
    br(),
    img(src = "https://i.stack.imgur.com/eddZp.png", 
        height = "400px", width = "600px", align = "right", 
        alt = "图像显示不出来就显示该文本")
  )
)
shinyApp(ui,server)#只能在浏览器中打开
ui<-fluidPage(
  mainPanel(
    strong("一期一会，网易云音乐外链"),
    br(),
    tags$iframe(src="https://music.163.com/outchain/player?type=2&id=479850552&auto=0&height=66",
                height = 80, width = 400, scrolling = "no", #框架内是否显示滚动条(yes,no,auto)
                seamless = FALSE)#是否需要框架看起来像是网页的一部分
  )
)
shinyApp(ui,server)#只能在浏览器中打开
ui<-fluidPage(
  mainPanel(
    strong("Because of you，临时外链，可能失效"),
    br(),
    # 后缀为音频后缀格式,使用audio函数
    tags$audio(src = "http://other.web.nc01.sycdn.kuwo.cn/resource/n3/95/92/2685144667.mp3", 
               type = "audio/mp3", autoplay = FALSE, controls = TRUE),
    br(),
    strong("一期一会----周深， 网盘永久外链"),
    br(),
    # 后缀不是音频格式，使用
    tags$iframe(src = "https://www.opendrive.com/player/NDVfOTY5NTY5Ml9BN1ZCbg", 
                height = 25, width = 297, scrolling = "no", seamless = FALSE),
    br(),
    strong("一期一会，网易云音乐外链"),
    br(),
    tags$iframe(src="https://music.163.com/outchain/player?type=2&id=479850552&auto=0&height=66",
                height = 80, width = 400, scrolling = "no", seamless = FALSE)
  )
)
shinyApp(ui,server)#只能在浏览器中打开
ui<-fluidPage(
  mainPanel(
    strong("Katy Perry MV，框架插入，临时外链，可能失效"),
    br(),
    tags$iframe(src = "https://vd.yinyuetai.com/he.yinyuetai.com/uploads/videos/common/EFC90168F02CAF3035BF1A310EBBAD7C.mp4",
                height = 400, width = 710, scrolling = "no", seamless = FALSE),
    br(),
    strong("Katy Perry MV，video函数插入，临时外链，可能失效"),
    br(),
    tags$video(src = "https://vd.yinyuetai.com/he.yinyuetai.com/uploads/videos/common/EFC90168F02CAF3035BF1A310EBBAD7C.mp4",
               type = "video/mp4", autoplay = FALSE, controls = TRUE),
    br(),
    strong("Katy Perry MV，框架插入，网盘外链，永久有效"),
    br(),
    tags$iframe(src = "https://www.opendrive.com/player/NDVfOTcwMjEwNl9oeFdlbQ",
                height = 400, width = 710, scrolling = "no", seamless = FALSE)    
  )
)
server<-function(input, output) {
}
shinyApp(ui,server) 
