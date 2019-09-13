# withSpinner 缓冲动画
library(shinycssloaders)
library(shiny)
library(DT)



options(spinner.color.background="#F5F5F5")

ui <- fluidPage(
  wellPanel(
    tags$b(style="color:red"," hides the spinner when the data-table is not shown."), 
    br(),
   
    br(),
    br(),
    tags$ul(
      tags$li("You can use it to wrap any kind of output."),
      tags$li("To see what happens on recalculation, click the recalculate button")
    ),
    checkboxInput("show_plot","Show tables",value=TRUE),
    actionButton("redraw_plot","Re-draw tables"),
    br(),
    br(),
    fluidRow(
      column(
        sliderInput("n_rows","Number of rows in table",min=1,max=nrow(mtcars),value=10,step = 1),
        width=3)
    )
  ),
  do.call(tabsetPanel,lapply(1:8,function(.type) {
    tabPanel(paste0("Type ",.type),
             fluidRow(
               column(width=6,
                      wellPanel(
                        tags$b("With spinner:"),
                        withSpinner(DT::dataTableOutput(paste0("table",.type)),type=.type) 
                      )
               ),
               column(width=6,
                      wellPanel(
                        tags$b("Without spinner (default):"),
                        DT::dataTableOutput(paste0("nospin_table",.type))
                      )
               )
             )
    )
  }))
)

server <- function(input, output,session) {
  for (i in 1:8) {
    output[[paste0("nospin_table",i)]] <- output[[paste0("table",i)]] <- DT::renderDataTable({
      validate(need(input$show_plot,"Show table is unchecked. Check to see table."))
      input$redraw_plot
      Sys.sleep(1) # just for demo so you can enjoy the animation
      mtcars[,1:4] %>% datatable(options=list(pageLength=input$n_rows))
    })
  }
}

shinyApp(ui = ui, server = server)