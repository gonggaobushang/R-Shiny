setwd("")
rm(list=ls())
library(shiny)
library(shinydashboard)
library(networkD3)
library(plotly)
library(ggplot2)
library(dplyr)

#
float=data.frame(x=sin(pi/180*seq(0,360,360/25)),y=cos(pi/180*seq(0,360,360/25)),
                 name=as.character(paste0("name",0:25)),col=rainbow(26),size=rep(10,26),
                 text=c(NA,"许捷","徐必聪","陈定凯","张丹松","张恒",
                        "纪烨磊","王逸岑","杨星宇","俞淇匀","章志成",
                        "孙诗婷","洪张杰","陆文丹","赖肖颖","孙静",
                        "吕旸萌","钱程","潘颖颖","邵超霞","包雨欣",
                        "何俐倩","王钰","张娜","王奕璐","金晶"))
float$text<-float$text%>%as.character

#
ui<-dashboardPage(
  dashboardHeader(title = "宁波大学",
                  dropdownMenu(type = "notifications",
                               notificationItem(text = "还有同学资料未完善",icon("users")
                               ),
                               notificationItem(text = "服务器已加载",
                                                icon = icon("exclamation-triangle"),status = "warning"
                               )
                  ),
                  dropdownMenu(type = "messages",
                               messageItem(from = "小助手", 
                                           message = "请记得及时更新",icon = icon(name = "handshake") # 默认Font Awesome库
                               )
                  ),
                  dropdownMenu(type = "tasks", badgeStatus = "success",
                               taskItem(value = 90, color = "green",text = "项目"),
                               taskItem(value = 75, color = "yellow",text = "服务器部署"),
                               taskItem(value = 80, color = "red",text = "总进程")
                  )
                  
  ), #显示不全
  dashboardSidebar(
    sidebarMenu(
      menuItem("宁波大学",tabName = "sidebar401"),
      menuItem("自我介绍",tabName = "sidebar101"),
      menuItem("组织关系",tabName = "sidebar301"),
      menuItem("留言板",tabName = "sidebar501"),
      menuItem("照片墙",tabName = "sidebar601"),
      menuItem("网站说明",tabName = "siderbar201")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "sidebar101",
              fluidRow(
                h1("数学与统计学院", align = "center"),
                h2("2019届金融工程", align = "center"),
                br(),
                mainPanel(
                  strong("建议搭配音乐食用"),
                  br(),
                  tags$iframe(src="https://music.163.com/outchain/player?type=2&id=541960310&auto=0")
                  ),
                box(checkboxGroupInput("Item101",h3("小可爱"),
                                       choices = list("所有"="name0",
                                                      "许捷" = "name1",
                                                      "徐必聪" = "name2", 
                                                      "陈定凯" = "name3",
                                                      "张丹松"="name4",
                                                      "张恒"="name5",
                                                      "纪烨磊" = "name6",
                                                      "王逸岑" = "name7", 
                                                      "杨星宇" = "name8",
                                                      "俞淇匀"="name9",
                                                      "章志成"="name10",
                                                      "孙诗婷" = "name11",
                                                      "洪张杰" = "name12", 
                                                      "陆文丹" = "name13",
                                                      "赖肖颖"="name14",
                                                      "孙静"="name15",
                                                      "吕旸萌" = "name16",
                                                      "钱程" = "name17", 
                                                      "潘颖颖" = "name18",
                                                      "邵超霞"="name19",
                                                      "包雨欣"="name20",
                                                      "何俐倩" = "name21",
                                                      "王钰" = "name22", 
                                                      "张娜" = "name23",
                                                      "王奕璐"="name24",
                                                      "金晶"="name25"),
                                       selected =as.character(paste0("name",0:25)))),
                box(submitButton("Check"),h4("随便画的图呦~"),
                  plotlyOutput("myOutput101A"),
                  verbatimTextOutput("selection"),
                  width=12),
                box(width=12,
                    h4("请允许我最后再做一次自我介绍"),
                    h5(textOutput("text001")),
                    h6("个人照片暂未开放"))
              )),
      tabItem(tabName = "siderbar201",
              fluidRow(
                h4("更新:2019.6.19   1.0"),
                h3("该网站将会一直更新与维护"),
                h5("联系方式:1945089262@qq.com,Jie Xu"),
                h5("由R部署，核心代码将上传GitHub：gonggaobushang")
              )),
      tabItem(tabName = "sidebar501",
              textInput("text0","是不是想说点什么"),
              fluidRow(
                box(title = h3(textOutput("text1"))),
                br(),
                h6("你随便说，反正该版本也没有保存等功能-。-")
              )),
      tabItem(tabName = "sidebar301",
               fluidRow(
                 h3("图谱"),
                 box(width=12,simpleNetworkOutput("myOutput301A"))
               )),
      tabItem(tabName = "sidebar601",
              fluidRow(
                h3("集体照片"),
                box(imageOutput("myOutput601A",  width = "auto", height = "auto",
                                         click = "image_click",
                                         hover = hoverOpts(
                                           id = "image_hover",
                                           delay = 10,
                                           delayType = "throttle"
                                         ),
                                         brush = brushOpts(id = "image_brush") ))
              )),
      tabItem(tabName = "sidebar401",
              fluidRow(
                a(href = "https://www.nbu.edu.cn/",target = "_blank", 
                  "宁波大学官网"),
                strong("视频宁大"),
                br(),
                box(tags$video(src = "https://www.nbu.edu.cn/__local/F/00/A4/61CC3D9EA5F86FE1361CABB55B6_5934FCFE_859578E.mp4?e=.mp4",
                               type = "video/mp4", autoplay =FALSE, controls = TRUE))
              ))
      
    )
  )
)
  


server<-function(input,output){
  
  output$myOutput601A<-renderImage({
    
    list(src="1.jpg")
  })
  
  
  output$text1 <- renderText({
    print("功能错误")
  })
  output$example <- renderText({
    h4("举个例子")
  })
  networkInput <- reactive({
    read.table("network01.txt",header = FALSE, sep = "")
  })
  
  output$myOutput301A <- renderSimpleNetwork({
    simpleNetwork(networkInput(),
                  fontFamily="华文行楷",
                  linkColour="#B23AEE",
                  nodeColour="#00CD66",       
                  charge = -950,
                  opacity=1,
                  zoom=TRUE,
                  fontSize = 20)})
  
  nameInput <- reactive({
    pp=numeric();
    for (i in 1:length(input$Item101)){
      q=which(float$name==input$Item101[i])%>%as.numeric 
      pl=length(pp)
      for (j in 1:length(q)){
        pp[pl+j]=q[j]
      }
    }
    sort(pp)
  })
  
  dataInput <- reactive({
    float[nameInput(),]  
  })
  
  
  output$myOutput101A <- renderPlotly ({
    plot_ly(type = 'scatter', mode = 'lines')%>%
      add_trace(
        x = dataInput()$x, 
        y = dataInput()$y,
        text = dataInput()$text,
        hoverinfo = 'text',
        marker = list(color=dataInput()$col,size=dataInput()$size),
        showlegend = F)
  })
  
  output$selection <- renderPrint({
    s <- event_data("plotly_click")
    if (length(s) == 0) {
      "请点击上图"
    } else {
      cat("小可爱: \n",as.character(float$text[which.min(abs(float$x-s$x)+abs(float$y-s$y))]))
    }
  })
  
  pointInput <- reactive({
    s <- event_data("plotly_click")
    s$x
  })
  
  pipeiInput<- reactive({
    s <- event_data("plotly_click")
    if (length(s) == 0) {
      "请点击上图"
    } else{
      if(float$text[which.min(abs(float$x-s$x)+abs(float$y-s$y))] == "许捷"){
        c("许捷\n",
          "..........\n暂时就这么多")
      }else if(float$text[which.min(abs(float$x-s$x)+abs(float$y-s$y))] == "徐必聪"){
        c("徐必聪\n",
          "4.骑手).-.(")
      }else("资料暂缺")
    }
    
  })
  
  output$text001 <- renderText({
    pipeiInput()%>%as.character
  })
  
  
} 


shinyApp(ui,server)



      
