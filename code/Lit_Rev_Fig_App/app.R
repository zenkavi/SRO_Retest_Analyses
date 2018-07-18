library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Literature review of retest reliabilities of self-regulation measures"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "task_type",
                  label = "Choose a task or survey:",
                  choices = c("survey", "task"))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "litPlot"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("plotData")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(gridExtra)
  library(GGally)
  library(stringr)
  library(plotly)
  
  fig_data <- read.csv('../../input/lit_review_figure.csv')
  
  fig_data = fig_data %>%
    separate(dv, c("task_group", "var"), sep="\\.",remove=FALSE,extra="merge") %>%
    mutate(task_group = factor(task_group, levels = task_group[order(task)]),
           raw1_fit0 = grepl('raw', raw_fit),
           type = as.character(type),
           dv = factor(dv, levels = dv[order(task)]),
           days_cutoff = ifelse(days < 60, days, 120),
           days = paste(days, '\n reference:', reference))
  
  output$litPlot <- renderPlot({
    
    # x    <- faithful$waiting
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    # hist(x, breaks = bins, col = "#75AADB", border = "orange",
    #      xlab = "Waiting time to next eruption (in mins)",
    #      main = "Histogram of waiting times")
    
    
    point_col = ifelse(input$task_type == "survey", '#F8766D','#00BFC4')
    
    plot_data = fig_data %>%
      filter(task_group == input$task_name)
    
    ggplotly(plot_data %>%
               ggplot(aes(y = var, x = retest_reliability, label=days))+
               geom_point(aes(size=sample_size, shape = type, alpha = days_cutoff), color = point_col)+
               theme(panel.background = element_rect(fill = NA),
                     panel.grid.major = element_line(colour = "grey80"),
                     legend.position = 'none')+
               xlab("")+
               ylab("")+
               scale_x_continuous(limits = c(-0.25,1), breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1))+
               scale_shape_manual(breaks = sort(fig_data$type), values = c(15, 16, 17, 3)),
             width=600)
    
  })
  
  # Show the first "n" observations ----
  output$plotData <- renderTable({
    fig_data %>%
      filter(task_group == input$task_name)
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)