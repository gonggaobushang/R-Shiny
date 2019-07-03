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
