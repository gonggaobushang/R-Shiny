library(shinyBS)
library(shiny)

# 警报bsAlert
shinyApp(
  ui =
    fluidPage(
      sidebarLayout(
        sidebarPanel(textInput("num1", NULL, value = 100),
                     "divided by", textInput("num2", NULL, value = 20),
                     "equals", textOutput("exampleOutput")),
        mainPanel(
          bsAlert("alert")
        )
      )
    ),
  server =
    function(input, output, session) {
      output$exampleOutput <- renderText({
        num1 <- as.numeric(input$num1)
        num2 <- as.numeric(input$num2)
        
        if(is.na(num1) | is.na(num2)) {
          createAlert(session, "alert", "exampleAlert", title = "Oops",
                      content = "Both inputs should be numeric.", append = FALSE)
        } else if(num2 == 0) {
          createAlert(session, "alert", "exampleAlert", title = "Oops",
                      content = "You cannot divide by 0.", append = FALSE)
        } else {
          closeAlert(session, "exampleAlert")
          return(num1/num2)
        }
        
      })
    }
)



# 提示按钮bsButton
shinyApp(
  ui =
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          sliderInput("bins",
                      "Move the slider to see its effect on the button below:",
                      min = 1,
                      max = 50,
                      value = 1),
          bsButton("actTwo", label = "Click me if you dare!", icon = icon("ban")),
          tags$p("Clicking the first button below changes the disabled state of the second button.")
        ),
        mainPanel(
          textOutput("exampleText")
        )
      )
    ),
  server =
    function(input, output, session) {
      
      observeEvent(input$bins, ({
        
        b <- input$bins
        disabled = NULL
        style = "default"
        icon = ""
        
        if(b < 5) {
          disabled = TRUE
          icon <- icon("ban")
        } else {
          disabled = FALSE
        }
        
        if(b < 15 | b > 35) {
          style = "danger"
        } else if(b < 20 | b > 30) {
          style = "warning"
        } else {
          style = "default"
          icon = icon("check")
        }
        
        updateButton(session, "actTwo", disabled = disabled, style = style, icon = icon)
        
      }))
      
      output$exampleText <- renderText({
        input$actTwo
        b <- isolate(input$bins)
        txt = ""
        if((b > 5 & b < 15) | b > 35) {
          txt = "That was dangerous."
        } else if((b > 5 & b < 20) | b > 30) {
          txt = "I warned you about that."
        } else if(b >= 20 &  b <= 30) {
          txt = "You have choosen... wisely."
        }
        return(txt)
      })
    }
)


# 展开面板bsCollapsePanel
shinyApp(
  ui =
    fluidPage(
      sidebarLayout(
        sidebarPanel(HTML("This button will open Panel 1 using updateCollapse."),
                     actionButton("p1Button", "Push Me!"),
                     selectInput("styleSelect", "Select style for Panel 1",
                                 c("default", "primary", "danger", "warning", "info", "success"))
        ),
        mainPanel(
          bsCollapse(id = "collapseExample", open = "Panel 2",
                     bsCollapsePanel("Panel 1", "This is a panel with just text ",
                                     "and has the default style. You can change the style in ",
                                     "the sidebar.", style = "info"),
                     bsCollapsePanel("Panel 2", "This panel has a generic plot. ",
                                     "and a 'success' style.", plotOutput("genericPlot"), style = "success")
          )
        )
      )
    ),
  server =
    function(input, output, session) {
      output$genericPlot <- renderPlot(plot(rnorm(100)))
      observeEvent(input$p1Button, ({
        updateCollapse(session, "collapseExample", open = "Panel 1")
      }))
      observeEvent(input$styleSelect, ({
        updateCollapse(session, "collapseExample", style = list("Panel 1" = input$styleSelect))
      }))
    }
)

# 提示框bsTooltip addPopover
shinyApp(
  ui =
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          sliderInput("bins",
                      "Number of bins:",
                      min = 1,
                      max = 50,
                      value = 30),
          bsTooltip("bins", "The wait times will be broken into this many equally spaced bins",
                    "right", options = list(container = "body"))
        ),
        mainPanel(
          plotOutput("distPlot")
        )
      )
    ),
  server =
    function(input, output, session) {
      output$distPlot <- renderPlot({
        
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
      })
      addPopover(session, "distPlot", "Data", content = paste0("
Waiting time between ",
"eruptions and the duration of the eruption for the Old Faithful geyser ",
"in Yellowstone National Park, Wyoming, USA.
Azzalini, A. and ","Bowman, A. W. (1990). A look at some data on the Old Faithful geyser. ",
"Applied Statistics 39, 357-365.

"), trigger = 'click')
    }
)

# 创建窗口bsModal
shinyApp(
  
  ui =
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          sliderInput("bins",
                      "Number of bins:",
                      min = 1,
                      max = 50,
                      value = 30),
          actionButton("tabBut", "View Table")
        ),
        
        mainPanel(
          plotOutput("distPlot"),
          bsModal("modalExample", "Data Table", "tabBut", size = "large",
                  dataTableOutput("distTable"))
        )
      )
    ),
  server =
    function(input, output, session) {
      
      output$distPlot <- renderPlot({
        
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
      })
      
      output$distTable <- renderDataTable({
        
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        tab <- hist(x, breaks = bins, plot = FALSE)
        tab$breaks <- sapply(seq(length(tab$breaks) - 1), function(i) {
          paste0(signif(tab$breaks[i], 3), "-", signif(tab$breaks[i+1], 3))
        })
        tab <- as.data.frame(do.call(cbind, tab))
        colnames(tab) <- c("Bins", "Counts", "Density")
        return(tab[, 1:3])
        
      }, options = list(pageLength=10))
      
    }
)
