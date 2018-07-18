library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(plotly)

lit_data <- read.csv('../../input/lit_review_figure.csv')

lit_data = lit_data %>%
  separate(dv, c("task_group", "var"), sep="\\.",remove=FALSE,extra="merge") %>%
  mutate(task_group = factor(task_group, levels = task_group[order(task)]),
         raw1_fit0 = grepl('raw', raw_fit),
         type = as.character(type),
         dv = factor(dv, levels = dv[order(task)]),
         days_cutoff = ifelse(days < 60, days, 120),
         days = paste(days, '\n reference:', reference))

ui <- fluidPage(
  
  titlePanel("Retest reliabilities of self-regulation measures"),
  
  # sidebarLayout(
  #   
  #   sidebarPanel(
  #     
      # selectInput(inputId = "task_type",
      #             label = "View a task or survey:",
      #             choices = c("survey", "task")),
  #     
      # selectInput(inputId = "task_name",
      #             label = "Choose task or survey name:",
      #             choices = c('bis_bas_survey', 'attention_network_task'))
  #     
  #   ),
  #   
  #   mainPanel(
  #     
  #     plotOutput(outputId = "litPlot"),
  #     
  #     tableOutput("plotData")
  #     
  #   )
  # )
  
  fluidRow(
    column(12,
           h4("Literature Review"),
           selectInput(inputId = "task_type",
                       label = "View a task or survey:",
                       choices = c("survey", "task")),
           
           
           conditionalPanel(condition = "input.task_type == 'survey'",
                            selectInput(inputId = "survey_name",
                                        label = "Choose task or survey name:",
                                        choices = c("bis_bas_survey", "bis11_survey", "brief_self_control_survey", "dickman_survey", "dospert_rp_survey", "dospert_rt_survey", "eating_survey", "erq_survey", "five_facet_mindfulness_survey", "future_time_perspective_survey", "grit_scale_survey", "impulsive_venture_survey", "leisure_time_activity_survey", "mindful_attention_awareness_survey", "selection_optimization_compensation_survey", "sensation_seeking_survey", "ten_item_personality_survey", "theories_of_willpower_survey", "time_perspective_survey", "upps_impulsivity_survey"))),
           
           conditionalPanel(condition = "input.task_type == 'task'",
                            selectInput(inputId = "task_name",
                                        label = "Choose task or survey name:",
                                        choices = c("adaptive_n_back", "angling_risk_task_always_sunny", "attention_network_task", "bickel_titrator", "choice_reaction_time", "dietary_decision", "digit_span", "dot_pattern_expectancy", "go_nogo", "holt_laury_survey" , "information_sampling_task", "keep_track", "kirby", "local_global_letter", "probabilistic_selection", "psychological_refractory_period_two_choices", "ravens", "recent_probes", "shape_matching", "shift_task", "simon", "simple_reaction_time", "spatial_span", "stop_signal", "stroop", "tower_of_london"))),
           
           plotlyOutput(outputId = "litPlot"),
           dataTableOutput("plotData")
    )
    # ,
    # column(6,
    #        h4("Bootstrapped Data"),
    #        selectInput(inputId = "task_type",
    #                    label = "View a task or survey:",
    #                    choices = c("survey", "task")),
    #        selectInput(inputId = "task_name",
    #                    label = "Choose task or survey name:",
    #                    choices = c('bis_bas_survey', 'attention_network_task')),
    #        plotOutput(outputId = "litPlot")
    # )
  )
  
)


server <- function(input, output) {
  
  datasetInput <- reactive({
    
    filterby = ifelse(input$task_type == "survey", input$survey_name, input$task_name)
    
    lit_data %>%
      filter(task_group == filterby)
  })
  
  output$litPlot <- renderPlotly({
    
    point_col = ifelse(input$task_type == "survey", '#F8766D','#00BFC4')
    
    plot_data = datasetInput()
    
   ggplotly(plot_data %>%
               ggplot(aes(y = var, x = retest_reliability, label=days))+
               geom_point(aes(size=sample_size, shape = type, alpha = days_cutoff), color = point_col)+
               theme(panel.background = element_rect(fill = NA),
                     panel.grid.major = element_line(colour = "grey80"),
                     legend.position = 'none')+
               xlab("")+
               ylab("")+
               scale_x_continuous(limits = c(-0.25,1), breaks=c(-0.25, 0, 0.25, 0.5, 0.75, 1))+
               scale_shape_manual(breaks = sort(lit_data$type), values = c(15, 16, 17, 3)))
    
  })
  
  
  output$plotData <- renderDataTable({
    datasetInput() %>%
      select("var", "retest_reliability", "type", "sample_size")
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)