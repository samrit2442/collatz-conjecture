library(shiny)
library(tibble)
library(tidyverse)
library(dplyr)
library(plotly)
library(ggplot2)
library(lazyeval)
library(shinyFeedback)
library(shinydashboard)

ui <- fluidPage(
  titlePanel("Collatz Conjecture"),
  sidebarLayout(
    sidebarPanel(
      shinyFeedback::useShinyFeedback(),
      numericInput("n",
                   "Your Favourite Positive Integer (আপনার পছন্দের ধনাত্মক পূর্ণ সংখ্যা)",
                   min = 1,
                   max = 999999999999,
                   value = 5),
      h2("About the Shiny App"),
      h5("One of the most famous unsolved problems in Mathematics is Collatz Conjecture, is now in R Shiny Apps.
         Basically, it is a process or algorithm that will eventually reach to 1 whatever the positive integer is chosen initially.
         Considering any positive integer of your choice, this shiny application is showcasing an animated display of the integers it takes in a line diagram before descending to 1.")
    ),
    mainPanel(
      # tableOutput("table"),
      plotlyOutput("plot"),
      valueBoxOutput("value1", width = 6)
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  stopping_time = NULL
  collatz <- function(s){
    while(s >= 1){
      stopping_time = c(stopping_time, s)
      if(s %% 2 == 0)
      {
        s = s / 2
      }
      else
        s = 3 * s + 1
      if(s == 1){
        break
      }
    }
    return(stopping_time = c(stopping_time, 1))
  }
  
  collatz_df_react <- reactive({
    req(input$n)
    tibble(y = collatz(input$n),
           x = 1:length(y))
  })
  
  
  # output$table <- renderTable(collatz_df_react(), digits = 0)
  
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    bind_rows(dats)
  }
  
  
  fig_react <- reactive({
    
    req(input$n, cancelOutput = T)
    int <- (is.integer(input$n) & input$n > 0)
    shinyFeedback::feedbackDanger("n", !int, "Please insert positive integer only")
    req(int, cancelOutput = T)
    df <- collatz_df_react() |> accumulate_by(~x)
    
    p <- ggplot(df, aes(x, y, frame = frame)) +
      geom_point() +
      geom_line()
    p <- ggplotly(p) |> 
      animation_opts(
        frame = 50,
        transition = 0,
        redraw = F
      ) |>
      animation_slider(
        currentvalue = list(
          prefix = "Stopping Time: "
        )
      )
    p
  })
  
  output$plot <- renderPlotly({fig_react()})
  output$value1 <- renderValueBox({
    valueBox(nrow(collatz_df_react()), "Total Stopping Time", color = "red")
  })
}

shinyApp(ui = ui, server = server)