# Load Libraries
library(shiny)
library(ggplot2)
suppressPackageStartupMessages(library(dplyr))
library(kableExtra)
library(plotly)
library(plotrix)
library(here)
library(googlesheets)
library(janitor)


################## Initial Global 


# import data from google sheet

# 1. Saved as googlesheet, on my google drive.
#Great tips: http://htmlpreview.github.io/?https://raw.githubusercontent.com/jennybc/googlesheets/master/vignettes/basic-usage.html
#my_sheets <- gs_ls()

#class_gs <- gs_key("")
#class_gs
#gs_ws_ls(class_gs)

#class_data <- class_gs %>%
#  gs_read(ws = "Form Responses 1") %>%
#  clean_names()

# import data from r project
class_raw <- readr::read_csv("./CPPS Rating Form (PSYC 532).csv") %>% clean_names()



# Rename variables and create summary scores with .csv
class_processed <- class_raw %>% 
  dplyr::rename(cpps1 = "the_therapist_encourages_the_exploration_of_feelings_regarded_by_the_patient_as_uncomfortable_e_g_anger_envy_excitement_sadness_or_happiness",
                cpps2="the_therapist_gives_explicit_advice_or_direct_suggestions_to_the_patient",
                cpps3 = "the_therapist_actively_initiates_the_topics_of_discussion_and_therapeutic_activities", 
                cpps4 = "the_therapist_links_the_patient_s_current_feelings_or_perceptions_to_experiences_of_the_past", 
                cpps5 = "the_therapist_focuses_attention_on_similarities_among_the_patient_s_relationships_repeated_over_time_settings_or_people", 
                cpps6= "the_therapist_focuses_discussion_on_the_patient_s_irrational_or_illogical_belief_systems", 
                cpps7= "the_therapist_focuses_discussion_on_the_relationship_between_the_therapist_and_patient", 
                cpps8= "the_therapist_encourages_the_patient_to_experience_and_express_feelings_in_the_session", 
                cpps9 = "the_therapist_suggests_specific_activities_or_tasks_homework_for_the_patient_to_attempt_outside_of_session", 
                cpps10= "the_therapist_addresses_the_patient_s_avoidance_of_important_topics_and_shifts_in_mood", 
                cpps11="the_therapist_explains_the_rationale_behind_his_or_her_technique_or_approach_to_treatment", 
                cpps12="the_therapist_focuses_discussion_on_the_patient_s_future_life_situations", 
                cpps13="the_therapist_suggests_alternative_ways_to_understand_experiences_or_events_not_previously_recognized_by_the_patient", 
                cpps14="the_therapist_identifies_recurrent_patterns_in_patient_s_actions_feelings_and_experiences", 
                cpps15="the_therapist_provides_the_patient_with_information_and_facts_about_his_or_her_current_symptoms_disorder_or_treatment", 
                cpps16="the_therapist_allows_the_patient_to_initiate_the_discussion_of_significant_issues_events_and_experiences", 
                cpps17="the_therapist_explicitly_suggests_that_the_patient_practice_behavior_s_learned_in_therapy_between_sessions", 
                cpps18="the_therapist_teaches_the_patient_specific_techniques_for_coping_with_symptoms", 
                cpps19="the_therapist_encourages_discussion_of_patient_s_wishes_fantasies_dreams_or_earlychildhood_memories_positive_or_negative", 
                cpps20="the_therapist_interacts_with_the_patient_in_a_teacher_like_didactic_manner",
                video = "select_the_therapist_being_rated") %>%
  mutate(id = plyr::revalue(email_address,
                      c("student1" = "94",
                        "student2" = "97",
                        "student3" = "12",
                        "student4"= "32",
                        "student5" = "58",
                        "student6" = "82",
                        "student7" = "21",
                        "jbedics" = "1")),
         Video = plyr::revalue(video,
                         c("Steven Hayes" = "Hayes",
                           "Janet Wolfe" = "Wolfe")),
                          # "Marsha Linehan" = "Linehan",
                          # "Aaron Beck" = "Beck",
                          # "Donald Meichenbaum" = "Meichenbaum",
                          # "Albert Ellis" = "Ellis")),
         piavg = (cpps1 + cpps4 + cpps5 + cpps7 + cpps8 + cpps10 + cpps13 + cpps14 + cpps16 + cpps19)/10,
         cbavg = (cpps2 + cpps3 + cpps6 + cpps9 + cpps11 + cpps12 + cpps15 + cpps17 + cpps18 + cpps20)/10) %>%
  dplyr::select(-email_address, -timestamp, -video) %>%
  filter(id %in% c("97","12","94", "32","58","82","21","1"))

# gather
class_gathered <- class_processed %>% 
  tidyr::gather(key=cpps, value= score, cpps1, cpps2, cpps3, cpps4, cpps5, cpps6, cpps7, cpps8, cpps9, cpps10, cpps11, cpps12, cpps13, cpps14, cpps15, cpps16, cpps17, cpps18, cpps19, cpps20, cbavg, piavg) %>%
  mutate(cpps_f = as.factor(cpps),
         cpps_l = plyr::revalue(cpps_f,
                          c("cpps1" = "The therapist encourages the exploration of feelings regarded by the patient as uncomfortable", "cpps2"="The therapist gives explicit advice or direct suggestions to the patient", "cpps3" = "The therapist actively initiates the topics of discussion and therapeutic activities", "cpps4" = "The therapist links the patient's current feelings or perceptions to experiences of the past", "cpps5"= "The therapist focuses attention on similarities among the patient's relationships repeated over time, settings, or people", "cpps6"= "The therapist focuses discussion on the patient's irrational or illogical belief systems", "cpps7"="The therapist focuses discussion on the relationship between the therapist and patient", "cpps8"="The therapist encourages the patient to experience and express feelings in the session", "cpps9" = "The therapist suggests specific activities or tasks (homework) for the patient to attempt outside of session", "cpps10"="The therapist addresses the patient's avoidance of important topics and shifts in mood", "cpps11"="The therapist explains the rationale behind his or her technique or approach to treatment", "cpps12"="The therapist focuses discussion on the patient's future life situations", "cpps13"="The therapist suggests alternative ways to understand experiences or events not previously recognized by the patient", "cpps14"="The therapist identifies recurrent patterns in patient's actions, feelings and experiences", "cpps15"="The therapist provides the patient with information and facts about his or her current symptoms, disorder, or treatment", "cpps16"="The therapist allows the patient to initiate the discussion of significant issues, events, and experiences", "cpps17"="The therapist explicitly suggests that the patient practice behavior(s) learned in therapy between sessions", "cpps18"="The therapist teaches the patient specific techniques for coping with symptoms", "cpps19"="The therapist encourages discussion of patient's wishes, fantasies, dreams, or early childhood memories (positive or negative)", "cpps20"="The therapist interacts with the patient in a teacher-like (didactic) manner")),
         cpps_f = forcats::fct_relevel(cpps_f, "cpps1", "cpps4", "cpps5","cpps7", "cpps8", "cpps10", "cpps13", "cpps14", "cpps16", "cpps19", "cpps2", "cpps3", "cpps6", "cpps9", "cpps11", "cpps12", "cpps15", "cpps17", "cpps18", "cpps20")) %>%
  as_tibble() %>%
  dplyr::select(-cpps)


##############  Wrangling for Kappa

hayes <- class_processed %>%
  filter(Video == "Hayes") %>%
  select(id, Video, everything(), -piavg, -cbavg) %>%
  tidyr::gather(item, score, -id, -Video) %>%
  tidyr::spread(id, score) %>%
  select(-Video, -item) %>%
  as.data.frame()

wolfe <- class_processed %>%
  filter(Video == "Wolfe") %>%
  select(id, Video, everything(), -piavg, -cbavg) %>%
  tidyr::gather(item, score, -id, -Video) %>%
  tidyr::spread(id, score) %>%
  select(-Video, -item) %>%
  as.data.frame()


##################### Calculate Cohen's Kappa for each pair of raters #####################
library(psych)

################# kappa for hayes
kappa_hayes <- data.frame()
for(j in 1:8) { # 10 = Number of Raters
  
  for(i in 1:8)
  {
    x <- hayes[,c(i,j)] # Make datasets for each pair of raters (10 raters by 10 raters)
    k <- cohen.kappa(x)
    w <- broom::tidy(k) %>% tidyr::gather(variable, value, -type) %>%
      tidyr::unite(temp, type, variable) %>%
      tidyr::spread(temp, value)
    kappa_data <- data.frame(first_rater = i, second_rater = j, w_kappa = w$weighted_estimate, w_low = w$weighted_conf.low, w_high=w$weighted_conf.high, kappa = w$unweighted_estimate, low = w$unweighted_conf.low, high=w$unweighted_conf.high) # Buid dataset with Rater ID and Kappa
    kappa_hayes <- rbind(kappa_hayes, kappa_data)
  }
}

# Delete same rater's kappa (e.g., Rater 1 & Rater 1 and keep jamie ratings only 
kappa_h2 <- data.frame(
  kappa_hayes %>% 
    filter(first_rater!= second_rater,
           first_rater < second_rater,
           first_rater ==1)) %>%
  mutate(tape = "Hayes") %>%
  as_tibble()


################# kappa for wolfe
kappa_wolfe <- data.frame()
for(j in 1:8) { # 10 = Number of Raters
  
  for(i in 1:8)
  {
    x <- wolfe[,c(i,j)] # Make datasets for each pair of raters (10 raters by 10 raters)
    k <- cohen.kappa(x)
    w <- broom::tidy(k) %>% tidyr::gather(variable, value, -type) %>%
      tidyr::unite(temp, type, variable) %>%
      tidyr::spread(temp, value)
    kappa_data <- data.frame(first_rater = i, second_rater = j, w_kappa = w$weighted_estimate, w_low = w$weighted_conf.low, w_high=w$weighted_conf.high, kappa = w$unweighted_estimate, low = w$unweighted_conf.low, high=w$unweighted_conf.high) # Buid dataset with Rater ID and Kappa
    kappa_wolfe <- rbind(kappa_wolfe, kappa_data)
  }
}

# Delete same rater's kappa
kappa_w2 <- data.frame(
  kappa_wolfe %>% 
    filter(first_rater!= second_rater,
           first_rater < second_rater,
           first_rater ==1)) %>% # select just jamie
  mutate(tape = "Wolfe")

final_kappa <- bind_rows(kappa_h2, kappa_w2) %>%
  select(-first_rater) %>%
  dplyr::rename(Video = "tape") %>%
  mutate(id = plyr::revalue(as.character(second_rater), c("2"= "12", "3"= "21", "4"= "32", "5"= "58", "6"="82", "7"="94", "8" = "97"))) %>%
  select(-second_rater)


joined <- full_join(class_gathered, final_kappa)  %>% 
  mutate(id = plyr::revalue(id,
                            c("1" = "Jamie")))



######################################################

ui <- fluidPage(
  titlePanel("CPPS Ratings by Video and Rater"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput('rater', "Select Rater (Plot 1)", choices = joined$id %>% unique(), selected = "Jamie", multiple = TRUE),
      br(),
      
      selectizeInput('tape', "Select Video (All Plots)", choices = joined$Video %>% unique(),selected = TRUE,  multiple = FALSE),
      
      br(),
      
      selectInput("num", "CPPS Item (Plot 3)", choices = c("The therapist encourages the exploration of feelings regarded by the patient as uncomfortable","The therapist gives explicit advice or direct suggestions to the patient","The therapist actively initiates the topics of discussion and therapeutic activities","The therapist links the patient's current feelings or perceptions to experiences of the past","The therapist focuses attention on similarities among the patient's relationships repeated over time, settings, or people","The therapist focuses discussion on the patient's irrational or illogical belief systems","The therapist focuses discussion on the relationship between the therapist and patient","The therapist encourages the patient to experience and express feelings in the session","The therapist suggests specific activities or tasks (homework) for the patient to attempt outside of session","The therapist addresses the patient's avoidance of important topics and shifts in mood","The therapist explains the rationale behind his or her technique or approach to treatment","The therapist focuses discussion on the patient's future life situations","The therapist suggests alternative ways to understand experiences or events not previously recognized by the patient","The therapist identifies recurrent patterns in patient's actions, feelings and experiences","The therapist provides the patient with information and facts about his or her current symptoms, disorder, or treatment","The therapist allows the patient to initiate the discussion of significant issues, events, and experiences","The therapist explicitly suggests that the patient practice behavior(s) learned in therapy between sessions","The therapist teaches the patient specific techniques for coping with symptoms","The therapist encourages discussion of patient's wishes, fantasies, dreams, or early childhood memories (positive or negative)","The therapist interacts with the patient in a teacher-like (didactic) manner","cbavg","piavg"),
                  selected = "cbavg", selectize = FALSE),
      
      br(),
      
      helpText("Data from Spring 2019 ABA/CBT Class")
      

    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("1. CPPS Item Profile by Rater", plotlyOutput("pos1")),
        tabPanel("2. Plot of Kappa", plotOutput("kappaplot")),
        tabPanel("3. CPPS Item/Scale Comparison by Rater", plotOutput("pointplot")),
        #tabPanel("Bar Plot", plotOutput("barplot")), 
        tabPanel("Summary", tableOutput("summary")),
        hr(),
        helpText("1. `CPPS Item Profile by Rater` allows you to compare your scores with other raters including the instructor. You can choose the rater and video. The first 10 items are the CB items and second 10 are the PI items"),
        hr(),
        helpText("2. `Plot of Kappa` shows the kappa value which is a measure of agreement between two raters.  In this case, it identifies agreement between the rater and instructor.  The bar surrounding the point is the confidence interval.  You can choose the video for the plot."),
        hr(),
        helpText("3. `CPPS Item/Scale` shows individual CPPS items and scales by rater.  You can choose the item/scale and video.")
      )
    )
  ))

###############################################################

server <- function(input,output, session){
  
  output$pos1 <- renderPlotly({ 
    
    dta <- joined %>% 
      select(-cpps_l) %>% 
      dplyr::filter(id %in% input$rater, Video %in% input$tape) %>%
      group_by(id, cpps_f) %>%
      dplyr::summarise(score = mean(score))
    
    
    plot_ly(data =dta, x = ~cpps_f, y = ~score, type = 'scatter', mode = 'lines+markers', color = ~id) %>%
      layout(title = '', xaxis = list(title = ""),yaxis = list(title = "CPPS Score")) 
    
  })
  
  output$pointplot <- renderPlot({
    
    filtered <-
      joined %>%
      mutate(id = forcats::fct_relevel(id, "Jamie")) %>%
      dplyr::filter(cpps_l == input$num,
                    Video == input$tape
      )
    
    ggplot(filtered, aes(id, score)) +
      geom_point(size=3, color="blue", alpha=1) +
     # geom_smooth(aes(group=1), se=FALSE, color="black", alpha=.5) + 
      labs(x = "Rater ID", y = "CPPS Value") + 
      ylim(0,6) +
      ggtitle(paste(input$num))
  })
  
 # output$barplot <- renderPlot({
    
  #  filtered <-
   #   class_gathered %>%
  #    filter(!cpps_l=="cbavg",
   #          !cpps_l=="piavg") %>%
  #    filter(cpps_l == input$num,
  #           Video == input$tape
   #   )
    
  #  ggplot(filtered, aes(x=score)) +
   #   geom_bar() +
  #    xlim(0,6) +
   #   ggtitle(paste(input$num))


    
 # })
  
  output$summary <- renderPrint({
    
    joined %>% group_by(Video, cpps_f) %>%
      filter(cpps_f == "cbavg" | cpps_f == "piavg") %>%
      dplyr::summarize(mean = mean(score, na.rm = TRUE),
                       SD = sd(score,na.rm = TRUE),
                       Min = min(score, na.rm = TRUE),
                       Max = max(score, na.rm = TRUE)) %>%
      mutate_if(is.numeric, round, 2) %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = T)
  })
  
  output$kappaplot <- renderPlot({
    
    filtered <-
      joined %>%
      filter(!id %in% c("Jamie")) %>%
      dplyr::filter(Video == input$tape
      )
    
    ggplot(filtered, aes(y=w_kappa, x=as.factor(id), color=input$tape)) +
      geom_point(size = 4, position=position_dodge(width=0.5)) +
      geom_errorbar(
        aes(ymin = w_low, ymax = w_high),
        width = 0.3,
        linetype = "longdash",
        position=position_dodge(width=0.5))   +
      geom_hline(aes(yintercept = .60), linetype = "dashed") + 
      labs(x="Rater ID",
           y="Cohen's Kappa",
           subtitle = "Cohen's Kappa with Jamie") +
      ylim(-1,1) +
      ggtitle(paste(input$tape)) +
      theme(legend.position = "none")
  
   # final_kappa %>%
    #  ggplot(aes(y=w_kappa, x=as.factor(second_rater), color=tape)) +
    #  geom_point(size = 4, position=position_dodge(width=0.5)) +
    #  geom_errorbar(
    #    aes(ymin = w_low, ymax = w_high),
    #    width = 0.3,
    #    linetype = "longdash",
    #    position=position_dodge(width=0.5))  
    
  

  
  }) 
  
}
shinyApp(ui, server)
