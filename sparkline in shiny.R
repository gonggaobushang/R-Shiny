library(dplyr)
library(htmltools)
library(formattable)
library(shinydashboard)
library(sparkline)
library(shiny)

# 小交互式图 sparkline spk_add_deps
mtcars %>%
  group_by(cyl) %>%
  summarise(
    hp = spk_chr(
      hp, type="box",
      chartRangeMin=0, chartRangeMax=max(mtcars$hp)
    ),
    mpg = spk_chr(
      mpg, type="box",
      chartRangeMin=0, chartRangeMax=max(mtcars$mpg)
    )
  ) %>%
  formattable() %>%
  formattable::as.htmlwidget() %>%
  spk_add_deps()


sl1 <- sparkline(
  c(5,4,5,-2,0,3),
  type='bar',
  barColor="#aaf",
  chartRangeMin=-10,
  chartRangeMax=2,
  elementId="sparkline-for-composite"
)
sl2 <- sparkline(
  c(4,1,5,7,9,9,8,7,6,6,4,7,8,4,3,2,2,5,6,7),
  type="line",
  fillColor = FALSE,
  lineColor ='red',
  chartRangeMin = -5,
  chartRangeMax = 10
)
spk_composite(sl1, sl2)
spk_composite(
  sl1,
  values=c(4,1,5,7,9,9,8,7,6,6,4,7,8,4,3,2,2,5,6,7),
  options = list(
    type="line",
    fillColor = FALSE,
    lineColor ='red',
    chartRangeMin = -5,
    chartRangeMax = 10
  )
)

spk_composite(
  sl1,
  sl2,
  options = list(
    type="box"
  )
)



#将sparklineq嵌进shiny

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "widgets",
            tabBox(width = 12,
                   tabPanel(title ="Boom", uiOutput("Table")),
                   tabPanel(title="However",h1("whatevs"))
            ))
  )
)
ui <-  dashboardPage(
  dashboardHeader(title = "Simple tabs"),
  sidebar,
  body
)
server <- function(input, output) {

  output$Table <- renderUI({
    res <- iris %>% 
      group_by(Species) %>% 
      summarise(Petal.Length=spk_chr(c(Petal.Length), width = 120, height = 40)) %>%
      format_table() %>%
      htmltools::HTML() %>%
      div() %>%
      spk_add_deps() %>%
      {column(width=6, .)} %>%
      fluidRow()
    
    res
  })
}

server2 <- function(input, output) {
  
  output$Table <- renderUI({
    fw <- as.htmlwidget(
      formattable(
        data.frame(
          id = c("a", "b"),
          sparkline = c(
            spk_chr(runif(10,0,10), type="bar"),
            spk_chr(runif(10,0,5), type="bar")
          ),
          stringsAsFactors = FALSE
        )
      )
    )
    spk_add_deps(fw)->a
    fluidRow(a)
  })

}

server3 <- function(input, output) {
  
  output$Table <- renderUI({
    sl1 <- sparkline(
      c(5,4,5,-2,0,3),
      type='line',
      lineColor="red",
      chartRangeMin=-4,
      chartRangeMax=6,
      width = 360, height = 120,
      elementId="sparkline-for-composite"
    )
    column(sl1,width=12, offset = 4)
    #box(sl1)
    #fluidRow(sl1)
    
  })
  
}



shinyApp(ui = ui, server = server)
shinyApp(ui = ui, server = server2)
shinyApp(ui = ui, server = server3)


