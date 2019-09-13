# 可拖动的输出 jqui_draggable
library(shinyjqui)
library(shiny)
library(plotly)

shinyApp(
  server = function(input, output) {
    output$foo <- renderPlotly({
      plot_ly()%>%add_lines(data=mtcars,x = mtcars$cyl, y = mtcars$mpg)
    })
   
    output$position <- renderPrint({
      print(input$foo_position)
    })
  },
  ui = fluidPage(
    verbatimTextOutput('position'),
    jqui_draggable(plotlyOutput('foo', width = '500px', height = '500px'), operation ="save")
  )
)

#jqui_draggable(ui, operation = c("enable", "disable", "destroy", "save", "load"), options = NULL, selector = NULL, switch = NULL)


