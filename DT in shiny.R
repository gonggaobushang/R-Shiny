library(shiny)
library(DT)
#语言设为中文
options(DT.options = list(
  searchHighlight = TRUE,          
  language = list(
    info = '显示第_START_ 至 _END_ 项结果，共 _TOTAL_ 项',
    search = '搜索:',
    paginate = list(previous = '上页', `next` = '下页'),
    lengthMenu = '显示 _MENU_ 项结果')))

#增添多个功能按钮
shinyApp(
  server = function(input, output) {
    
    output$ListingAlarmes <- DT::renderDataTable({
      
      
      DT::datatable( iris, 
                     rownames = FALSE,
                     extensions = c("Buttons"),
                     options=list(
                       lengthMenu = list(c(10, 25, 50, -1), c('10', '25','50', 'All')), 
                       pageLength = 10,
                       orderClasses = TRUE,
                       dom = 'Blfrtip',
                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print')   
                     )              
      )
    })
  },
  ui = fluidPage(
    DT::dataTableOutput('ListingAlarmes')
  )
)
