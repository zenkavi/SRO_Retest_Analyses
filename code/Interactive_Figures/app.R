##################################################################
# LOAD PACKAGES
##################################################################

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(plotly)

##################################################################
# LOAD LIT REVIEW DATA
##################################################################

lit_data <- read.csv('../../input/lit_review_figure.csv')

lit_data = lit_data %>%
  separate(dv, c("task_group", "var"), sep="\\.",remove=FALSE,extra="merge") %>%
  mutate(task_group = factor(task_group, levels = task_group[order(task)]),
         raw1_fit0 = grepl('raw', raw_fit),
         type = as.character(type),
         dv = factor(dv, levels = dv[order(task)]),
         days_cutoff = ifelse(days < 60, days, 120),
         days = paste(days, '\n reference:', reference))

##################################################################
# LOAD BOOT DATA
##################################################################

# retest_data_path = '/Users/zeynepenkavi/Documents/PoldrackLabLocal/Self_Regulation_Ontology/Data/Retest_03-29-2018/'
# 
# process_boot_df = function(df){
#   df = df %>%
#     drop_na() %>%
#     mutate(dv = as.character(dv),
#            icc = as.numeric(as.character(icc)),
#            spearman = as.numeric(as.character(spearman)),
#            pearson = as.numeric(as.character(pearson)),
#            eta_sq = as.numeric(as.character(eta_sq)),
#            sem = as.numeric(as.character(sem)),
#            partial_eta_sq = as.numeric(as.character(partial_eta_sq)),
#            omega_sq = as.numeric(as.character(omega_sq)),
#            var_subs = as.numeric(as.character(var_subs)),
#            var_ind = as.numeric(as.character(var_ind)),
#            var_resid = as.numeric(as.character(var_resid)),
#            F_time = as.numeric(as.character(F_time)),
#            p_time = as.numeric(as.character(p_time)),
#            df_time = as.numeric(as.character(df_time)),
#            df_resid = as.numeric(as.character(df_resid)))
#   return(df)} 
# 
# boot_df <- read.csv(gzfile(paste0(retest_data_path,'bootstrap_merged.csv.gz')))
# 
# boot_df = process_boot_df(boot_df)
# 
# refit_boot_df = read.csv(gzfile(paste0(retest_data_path,'refits_bootstrap_merged.csv.gz')))
# 
# refit_boot_df = process_boot_df(refit_boot_df)
# 
# fullfit_boot_df = boot_df[as.character(boot_df$dv) %in% unique(as.character(refit_boot_df$dv)),]
# 
# boot_df = boot_df[!as.character(boot_df$dv) %in% unique(as.character(refit_boot_df$dv)),]
# 
# boot_df = rbind(boot_df, refit_boot_df)
# 
# rm(refit_boot_df)
# 
# measure_labels <- read.csv('/Users/zeynepenkavi/Dropbox/PoldrackLab/SRO_Retest_Analyses/input/measure_labels.csv')
# measure_labels = measure_labels %>% select(-measure_description)
# 
# tmp = measure_labels %>%
#   mutate(dv = as.character(dv)) %>%
#   left_join(boot_df[,c("dv", "icc", "spearman")], by = 'dv') 
# 
# tmp = tmp %>%
#   separate(dv, c("task_group", "var"), sep="\\.",remove=FALSE,extra="merge") %>%
#   mutate(task_group = factor(task_group, levels = task_group[order(task)])) %>%
#   separate(var, c("var"), sep="\\.",remove=TRUE,extra="drop") %>%
#   mutate(task_group = gsub("_", " ", task_group),
#          var = gsub("_", " ", var)) %>%
#   arrange(task_group, var)
# 
# tmp = tmp %>%
#   left_join(rel_df[,c("dv", "icc")], by = "dv") %>%
#   rename(icc = icc.x, point_est = icc.y)
# 
# #Manual correction
# tmp = tmp %>%
#   mutate(task = ifelse(task_group == 'holt laury survey', "task", as.character(task))) %>%
#   mutate(task_group = ifelse(task_group == "psychological refractory period two choices", "psychological refractory period", ifelse(task_group == "angling risk task always sunny", "angling risk task",task_group))) %>%
#   mutate(task_group = gsub("survey", "", task_group))

##################################################################
# UI
##################################################################

ui <- fluidPage(
  
  titlePanel("Retest reliabilities of self-regulation measures"),
  
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

##################################################################
# SERVER
##################################################################

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

##################################################################
# SHINY APP
##################################################################
shinyApp(ui = ui, server = server)